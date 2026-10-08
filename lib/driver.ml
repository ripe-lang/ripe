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
  if Sys.command cmd <> 0 then (
    let label = Diagnostic.severity_label (use_color ()) Diagnostic.Error in
    Printf.eprintf "%s: command failed: %s\n" label cmd;
    exit 1)

let shell_command = function
  | [] -> invalid_arg "shell_command"
  | command :: args -> Filename.quote_command command args

(* Lower IL through qbe and return the emitted asm path *)
let run_qbe qbe il =
  let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
  let tmp_asm = Filename.temp_file "ripe" ".s" in
  Out_channel.with_open_text tmp_qbe (fun oc -> output_string oc il);
  run (shell_command [ qbe; "-o"; tmp_asm; tmp_qbe ]);
  Sys.remove tmp_qbe;
  tmp_asm

let run_linker target ~output ~object_file ~libraries =
  let args =
    Target.linker_args target ~output ~object_file
      ~runtime:(Config.runtime_object ()) ~libraries
  in
  run (shell_command args)

let compile_binary ~qbe target base il libraries =
  let tmp_asm = run_qbe qbe il in
  let tmp_obj = Filename.temp_file "ripe" ".o" in
  let args = Target.assembler_args target ~output:tmp_obj ~input:tmp_asm in
  run (shell_command args);
  run_linker target ~output:base ~object_file:tmp_obj ~libraries;
  Sys.remove tmp_asm;
  Sys.remove tmp_obj

let emit_asm qbe il =
  let tmp_asm = run_qbe qbe il in
  let asm = read_file tmp_asm in
  Sys.remove tmp_asm;
  asm

(* The same object the linker would have consumed, handed back instead *)
let emit_obj ~qbe target il =
  let tmp_asm = run_qbe qbe il in
  let tmp_obj = Filename.temp_file "ripe" ".o" in
  let args = Target.assembler_args target ~output:tmp_obj ~input:tmp_asm in
  run (shell_command args);
  let obj = read_file tmp_obj in
  Sys.remove tmp_asm;
  Sys.remove tmp_obj;
  obj

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

let show_program program =
  String.concat "\n" (List.map Ast.show_decl program.Program.decls) ^ "\n"

let show_tdecls tdecls =
  String.concat "\n" (List.map Tast.show_tdecl tdecls) ^ "\n"

let render_program program diags =
  let ctx =
    {
      Diagnostic.sm = program.Program.source_map;
      filename = program.Program.filename;
      color = use_color ();
    }
  in
  List.iter (fun d -> Printf.eprintf "%s" (Diagnostic.render ctx d)) diags

(* The C runtime we link calls main, so refuse before the linker leaks its own error *)
let check_has_main tdecls =
  let is_main decl =
    match decl with Tast.TFunc fd -> fd.Tast.entry_point | _ -> false
  in
  if not (List.exists is_main tdecls) then
    Diagnostic.emit
      (Diagnostic.error_no_span "no `main` function found"
      |> Diagnostic.help "add a `fn main()` entry point")

let file_tokens filename =
  if not (Sys.file_exists filename) then
    die (Printf.sprintf "no such file: %s" filename);
  let src = read_file filename in
  if not (String.is_valid_utf_8 src) then
    die (Printf.sprintf "not valid UTF-8: %s" filename);
  let lexbuf = Lexer.lexbuf_of_string src in
  dump_tokens (Lexer.read (Lexer.make_state 0)) lexbuf

let load filename =
  try Program.load ~read_file filename with
  | Sys_error _ -> die (Printf.sprintf "no such file: %s" filename)
  | Program.Invalid_utf8 name -> die (Printf.sprintf "not valid UTF-8: %s" name)
  | Program.Source_too_large name ->
      die
        (Printf.sprintf "more than %d bytes of source in one file: %s"
           Span.max_offset name)

let render_and_exit_if_failed program =
  let failed = Diagnostic.has_errors () in
  render_program program (Diagnostic.take ());
  if failed then exit 1

module Output = struct
  type t = Stdout | File of string

  let make = function "" -> Stdout | filename -> File filename

  (* Write to -o if set or stdout *)
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

(* Each stage is a possible stopping point so bail once we hit the target *)
let stop_at ~stage ~program target emit =
  if stage = target then begin
    emit ();
    render_and_exit_if_failed program;
    raise Exit
  end

let compile ~stage ~out ~libraries ~filename =
  let output = Output.make out in
  if stage = Tokens then (
    Output.text output (file_tokens filename);
    exit 0);

  let program = load filename in

  (* A missing main is noise once the program failed to load *)
  let load_had_errors = Diagnostic.has_errors () in

  let stop_at target emit = stop_at ~stage ~program target emit in
  try
    stop_at Ast (fun () -> Output.text output (show_program program));
    let decls = program.Program.decls in
    let uses = Resolve.resolve decls in
    stop_at Resolve (fun () -> Output.text output (Resolve.dump uses));
    let tdecls = Sema.analyze uses decls in
    let emit_check_result () =
      if not (Diagnostic.has_errors ()) then
        Output.text output "typecheck: ok\n"
    in
    stop_at Check emit_check_result;
    stop_at Tast (fun () -> Output.text output (show_tdecls tdecls));
    if stage = Bin && not load_had_errors then check_has_main tdecls;
    render_and_exit_if_failed program;

    let mir = Mir.build tdecls in
    Mir.verify mir;
    stop_at Mir (fun () -> Output.text output (Mir.dump mir));

    let il =
      Codegenqbe.emit ~filename:program.Program.filename
        ~source_map:program.Program.source_map mir
    in

    stop_at Qbe (fun () -> Output.text output il);
    let qbe = Config.qbe () in
    stop_at Asm (fun () -> Output.text output (emit_asm qbe il));
    stop_at Obj (fun () ->
        Output.bytes output (emit_obj ~qbe (Target.host ()) il));

    compile_binary ~qbe (Target.host ())
      (Output.base output filename)
      il libraries
  with
  | Exit -> ()
  | Diagnostic.Errors ds ->
      render_program program ds;
      exit 1
