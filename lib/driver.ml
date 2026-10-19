(* SPDX-License-Identifier: Apache-2.0 *)

type stage =
  | Tokens
  | Ast
  | Resolve
  | Tast
  | Check
  | Mir
  | Qbe
  | Asm
  | Obj
  | Bin

let stage_name : stage -> string = function
  | Tokens -> "tokens"
  | Ast -> "ast"
  | Resolve -> "resolve"
  | Tast -> "tast"
  | Check -> "check"
  | Mir -> "mir"
  | Qbe -> "qbe"
  | Asm -> "asm"
  | Obj -> "obj"
  | Bin -> "bin"

module Backend = struct
  type t = Qbe | X86

  let name : t -> string = function Qbe -> "qbe" | X86 -> "x86"

  let has_stage backend (stage : stage) =
    match stage with
    | Qbe | Asm -> ( match backend with Qbe -> true | X86 -> false)
    | Tokens | Ast | Resolve | Tast | Check | Mir | Obj | Bin -> true
end

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

let use_color () =
  match Sys.getenv_opt "NO_COLOR" with
  | Some value when not (String.is_empty value) -> false
  | _ -> Unix.isatty Unix.stderr

let die msg =
  let label = Diagnostic.severity_label (use_color ()) Diagnostic.Error in
  Printf.eprintf "%s: %s\n" label msg;
  exit 2

let run cmd =
  if Sys.command cmd <> 0 then begin
    let label = Diagnostic.severity_label (use_color ()) Diagnostic.Error in
    Printf.eprintf "%s: command failed: %s\n" label cmd;
    exit 1
  end

let shell_command = function
  | [] -> invalid_arg "shell_command"
  | command :: args -> Filename.quote_command command args

(* This lowers IL through qbe and returns the emitted asm path *)
let run_qbe qbe il =
  let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
  let tmp_asm = Filename.temp_file "ripe" ".s" in
  Out_channel.with_open_text tmp_qbe (fun oc -> output_string oc il);
  run (shell_command [ qbe; "-o"; tmp_asm; tmp_qbe ]);
  Sys.remove tmp_qbe;
  tmp_asm

let assemble qbe toolchain il =
  let tmp_asm = run_qbe qbe il in
  let tmp_obj = Filename.temp_file "ripe" ".o" in
  run (shell_command (toolchain.Config.assembler @ [ "-o"; tmp_obj; tmp_asm ]));
  Sys.remove tmp_asm;
  tmp_obj

let link toolchain ~output object_file libraries =
  let args =
    [ "-o"; output; object_file ]
    @ List.map (fun library -> "-l" ^ library) libraries
  in
  run (shell_command (toolchain.Config.linker @ args));
  Sys.remove object_file

let read_and_remove path =
  let contents = read_file path in
  Sys.remove path;
  contents

let dump_tokens read lexbuf =
  let buf = Buffer.create 256 in
  let rec loop () =
    let t, _ = read lexbuf in
    Buffer.add_string buf (Tokens.show_token t);
    Buffer.add_char buf '\n';
    if t <> Tokens.EOF then loop ()
  in
  loop ();
  Buffer.contents buf

let render_program program diags =
  let ctx =
    {
      Diagnostic.sm = program.Program.source_map;
      filename = program.Program.filename;
      color = use_color ();
    }
  in
  List.iter (fun d -> Printf.eprintf "%s" (Diagnostic.render ctx d)) diags

let file_tokens filename =
  if not (Sys.file_exists filename) then
    die (Printf.sprintf "no such file: %s" filename);
  let src = read_file filename in
  if not (String.is_valid_utf_8 src) then
    die (Printf.sprintf "not valid UTF-8: %s" filename);
  let lexbuf = Lexer.lexbuf_of_string src in
  dump_tokens Lexer.read lexbuf

let load filename =
  try Program.load filename with
  | Sys_error _ -> die (Printf.sprintf "no such file: %s" filename)
  | Program.InvalidUtf8 name -> die (Printf.sprintf "not valid UTF-8: %s" name)
  | Program.SourceTooLarge name ->
      die
        (Printf.sprintf "file too large: %s (over %d bytes)" name
           Span.max_offset)

let render_and_exit_if_failed program =
  let failed = Diagnostic.has_errors () in
  render_program program (Diagnostic.take ());
  if failed then exit 1

module Output = struct
  type t = Stdout | File of string

  let make = function "" -> Stdout | filename -> File filename

  (* This writes to -o if set or else stdout *)
  let text output s =
    match output with
    | Stdout -> print_string s
    | File filename ->
        Out_channel.with_open_text filename (fun oc -> output_string oc s)

  (* An object holds bytes that stdout would otherwise be free to translate *)
  let bytes output s =
    match output with
    | Stdout ->
        set_binary_mode_out stdout true;
        print_string s
    | File filename ->
        Out_channel.with_open_bin filename (fun oc -> output_string oc s)

  let base output source =
    match output with
    | Stdout -> Filename.remove_extension (Filename.basename source)
    | File filename -> filename
end

(* Every stage is a possible stopping point so we bail once we hit the target *)
let compile stage backend ~out libraries filename =
  let output = Output.make out in
  if not (Backend.has_stage backend stage) then
    die
      (Printf.sprintf "%s has no %s stage" (Backend.name backend)
         (stage_name stage));

  if stage = Tokens then begin
    Output.text output (file_tokens filename);
    exit 0
  end;

  let program = load filename in
  let stop_at target emit =
    if stage = target then begin
      emit ();
      render_and_exit_if_failed program;
      raise Exit
    end
  in
  try
    stop_at Ast (fun () ->
        Output.text output
          (String.concat "\n" (List.map Ast.show_decl program.Program.decls)
          ^ "\n"));
    let decls = program.Program.decls in
    let uses = Resolve.resolve decls in
    stop_at Resolve (fun () -> Output.text output (Resolve.dump uses));
    let tdecls = Sema.analyze uses decls in
    let emit_check_result () =
      if not (Diagnostic.has_errors ()) then
        Output.text output "typecheck: ok\n"
    in
    stop_at Check emit_check_result;
    stop_at Tast (fun () ->
        Output.text output
          (String.concat "\n" (List.map Tast.show_tdecl tdecls) ^ "\n"));
    render_and_exit_if_failed program;

    let mir = Mir.build tdecls in
    let mir = if stage = Bin then Mir.with_entry mir else mir in
    Mir.verify mir;
    stop_at Mir (fun () -> Output.text output (Mir.dump mir));

    let link_bytes object_bytes =
      stop_at Obj (fun () -> Output.bytes output object_bytes);
      let toolchain = Config.toolchain () in
      let tmp_obj = Filename.temp_file "ripe" ".o" in
      Out_channel.with_open_bin tmp_obj (fun oc ->
          output_string oc object_bytes);
      link toolchain ~output:(Output.base output filename) tmp_obj libraries
    in
    match backend with
    | Backend.X86 -> link_bytes (Codegenx86.emit mir)
    | Backend.Qbe ->
        let il = Codegenqbe.emit mir in

        stop_at Qbe (fun () -> Output.text output il);
        let qbe = Config.qbe () in
        stop_at Asm (fun () ->
            Output.text output (read_and_remove (run_qbe qbe il)));
        let toolchain = Config.toolchain () in
        stop_at Obj (fun () ->
            Output.bytes output (read_and_remove (assemble qbe toolchain il)));

        link toolchain
          ~output:(Output.base output filename)
          (assemble qbe toolchain il)
          libraries
  with
  | Exit -> ()
  | Codegenx86.Unsupported msg | Failure msg -> die msg
  | Diagnostic.Errors ds ->
      render_program program ds;
      exit 1
