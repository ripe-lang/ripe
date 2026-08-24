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

  let has_stage (backend : t) (stage : stage) : bool =
    match stage with
    | Qbe | Asm -> ( match backend with Qbe -> true | X86 -> false)
    | Tokens | Ast | Resolve | Tast | Check | Mir | Obj | Bin -> true
end

let read_file filename = In_channel.with_open_bin filename In_channel.input_all
let list_dir dir = Array.to_list (Sys.readdir dir)

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
let run_qbe ?(show_command = false) qbe il =
  let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
  let tmp_asm = Filename.temp_file "ripe" ".s" in
  Out_channel.with_open_text tmp_qbe (fun oc -> output_string oc il);
  let args = [ qbe; "-o"; tmp_asm; tmp_qbe ] in
  let command = shell_command args in
  let start = Unix.gettimeofday () in
  if show_command then Printf.eprintf "Running QBE:\n%s\n" command;
  run command;
  Sys.remove tmp_qbe;
  (tmp_asm, Unix.gettimeofday () -. start)

let run_linker ~show_commands target ~output ~object_file ~libraries =
  let args =
    Target.linker_args target ~output ~object_file
      ~runtime:(Config.runtime_object ()) ~libraries
  in
  let command = shell_command args in
  if show_commands then Printf.eprintf "Running linker:\n%s\n" command;
  run command

let link_object ~show_commands target base object_bytes libraries =
  let tmp_obj = Filename.temp_file "ripe" ".o" in
  Out_channel.with_open_bin tmp_obj (fun oc -> output_string oc object_bytes);
  run_linker ~show_commands target ~output:base ~object_file:tmp_obj ~libraries;
  Sys.remove tmp_obj

let compile_binary ~show_commands ~qbe target base il libraries =
  let backend_start = Unix.gettimeofday () in
  let tmp_asm, qbe_time = run_qbe ~show_command:show_commands qbe il in
  let tmp_obj = Filename.temp_file "ripe" ".o" in
  let assemble_args =
    Target.assembler_args target ~output:tmp_obj ~input:tmp_asm
  in
  let assemble = shell_command assemble_args in
  if show_commands then Printf.eprintf "Running assembler:\n%s\n" assemble;
  run assemble;
  let backend_time = Unix.gettimeofday () -. backend_start in
  let start = Unix.gettimeofday () in
  run_linker ~show_commands target ~output:base ~object_file:tmp_obj ~libraries;
  Sys.remove tmp_asm;
  Sys.remove tmp_obj;
  (qbe_time, backend_time, Unix.gettimeofday () -. start)

let emit_asm qbe il =
  let tmp_asm, _ = run_qbe qbe il in
  let asm = read_file tmp_asm in
  Sys.remove tmp_asm;
  asm

(* The same object the linker would have consumed, handed back instead *)
let emit_obj ~qbe target il =
  let tmp_asm, _ = run_qbe qbe il in
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
    let t, _, _ = read lexbuf in
    Buffer.add_string buf (Tokens.show_token t);
    Buffer.add_char buf '\n';
    if t <> Tokens.EOF then loop ()
  in
  loop ();
  Buffer.contents buf

let show_module (module_ : Ast.module_) =
  let header =
    match module_.Ast.header with
    | None -> []
    | Some header -> [ "module " ^ Interner.text header.Ast.name ]
  in
  let imports =
    List.map
      (fun import -> "import " ^ Ast.show_path import.Ast.path)
      module_.Ast.imports
  in
  String.concat "\n"
    (header @ imports @ List.map Ast.show_decl module_.Ast.decls)
  ^ "\n"

let show_program (program : Program.t) =
  program.Program.modules |> Array.to_list
  |> List.concat_map (fun (module_ : Program.module_) -> module_.Program.units)
  |> List.map (fun (unit_ : Program.unit_) -> show_module unit_.Program.ast)
  |> String.concat ""

let show_tdecls tdecls =
  String.concat "\n" (List.map Typedast.show_tdecl tdecls) ^ "\n"

let line_counts program =
  let count_processed_lines text =
    let length = String.length text in
    let rec loop position processed has_content =
      if position = length then processed + if has_content then 1 else 0
      else if text.[position] = '\n' then
        loop (position + 1) (processed + if has_content then 1 else 0) false
      else
        let has_content =
          has_content
          || text.[position] <> ' '
             && text.[position] <> '\t'
             && text.[position] <> '\r'
        in
        loop (position + 1) processed has_content
    in
    loop 0 0 false
  in
  let processed = ref 0 in
  let all = ref 0 in
  Array.iter
    (fun (module_ : Program.module_) ->
      List.iter
        (fun (unit_ : Program.unit_) ->
          let source_map = unit_.Program.source.Program.source_map in
          let source = Sourcemap.src source_map in
          let source_processed = count_processed_lines source in
          let source_all = Sourcemap.line_count source_map in
          processed := !processed + source_processed;
          all := !all + source_all)
        module_.Program.units)
    program.Program.modules;
  (!processed, !all)

let print_stats program ~frontend_time ~qbe_time ~compiler_time ~link_time
    ~total_time =
  let processed_lines, all_lines = line_counts program in
  let line_word count = if count = 1 then "line" else "lines" in
  let lines_per_second =
    if total_time > 0. then float_of_int all_lines /. total_time else 0.
  in
  Printf.eprintf "\n";
  Printf.eprintf "Compiled %d %s (%d raw lines)\n" processed_lines
    (line_word processed_lines)
    all_lines;
  Printf.eprintf "Front end time: %.6fs\n" frontend_time;
  Option.iter (Printf.eprintf "QBE time:       %.6fs\n") qbe_time;
  Printf.eprintf "Compiler time:  %.6fs\n" compiler_time;
  Printf.eprintf "Link time:      %.6fs\n" link_time;
  Printf.eprintf "Total time:     %.6fs (%.0f raw lines/s)\n" total_time
    lines_per_second

let report_stats stats program ~total_start ~frontend_time ~qbe_time
    ~compiler_time ~link_time =
  if stats then
    print_stats program ~frontend_time ~qbe_time ~compiler_time ~link_time
      ~total_time:(Unix.gettimeofday () -. total_start)

let source_ctx color (source : Program.source) : Diagnostic.ctx =
  {
    Diagnostic.sm = source.Program.source_map;
    filename = source.Program.filename;
    color;
  }

let program_context (program : Program.t) :
    (int -> Diagnostic.ctx) * Diagnostic.ctx =
  let color = use_color () in
  let source_at = Program.source_at program in
  ( (fun pos -> source_ctx color (source_at pos)),
    source_ctx color program.Program.root_source )

let render_program program diags =
  let context_at, default = program_context program in
  List.iter
    (fun d -> Printf.eprintf "%s" (Diagnostic.render_with context_at default d))
    diags

(* The C runtime we link calls main, so refuse before the linker leaks its own error *)
let check_has_main diags tdecls =
  let is_main decl =
    match decl with Typedast.TFunc fd -> fd.Typedast.entry_point | _ -> false
  in
  if not (List.exists is_main tdecls) then
    Diagnostic.emit diags
      (Diagnostic.error "no `main` function found"
      |> Diagnostic.help "add a `func main() i32` entry point")

(* The token dump never follows an import so it reads the root file itself *)
let root_tokens filename =
  if not (Sys.file_exists filename) then
    die (Printf.sprintf "no such file: %s" filename);
  let src = read_file filename in
  if not (String.is_valid_utf_8 src) then
    die (Printf.sprintf "not valid UTF-8: %s" filename);
  let lexbuf = Lexer.lexbuf_of_string src in
  dump_tokens (Lexer.read (Lexer.make_state 0)) lexbuf

let load ~diags ~search_roots ~filename =
  try
    Program.load ~diags ~read_file ~list_dir ~search_roots
      ~root_filename:filename ()
  with
  | Sys_error _ -> die (Printf.sprintf "no such file: %s" filename)
  | Program.Invalid_utf8 name -> die (Printf.sprintf "not valid UTF-8: %s" name)
  | Program.Source_too_large name ->
      die
        (Printf.sprintf "more than %d bytes of source in one program: %s"
           Span.max_offset name)

let render_and_exit_if_failed program diags =
  let failed = Diagnostic.has_errors diags in
  render_program program (Diagnostic.take diags);
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
let stop_at ~stage ~program ~diags target emit =
  if stage = target then begin
    emit ();
    render_and_exit_if_failed program diags;
    raise Exit
  end

let compile ~stage ~backend ~out ~libraries ~search_roots ~stats ~filename =
  let output = Output.make out in
  if not (Backend.has_stage backend stage) then
    die
      (Printf.sprintf "the %s backend has no %s stage" (Backend.name backend)
         (stage_name stage));

  if stage = Tokens then (
    Output.text output (root_tokens filename);
    exit 0);

  let total_start = Unix.gettimeofday () in
  let diags = Diagnostic.sink () in
  let program = load ~diags ~search_roots ~filename in

  (* A missing main is noise once the program failed to load *)
  let load_had_errors = Diagnostic.has_errors diags in

  let stop_at target emit = stop_at ~stage ~program ~diags target emit in
  try
    stop_at Ast (fun () -> Output.text output (show_program program));
    let resolved = Resolve.resolve_program ~diags program in
    let uses = resolved.Resolve.uses in
    let decls = resolved.Resolve.decls in
    stop_at Resolve (fun () -> Output.text output (Resolve.dump uses));
    let tdecls = Sema.analyze ~diags uses decls in
    let emit_check_result () =
      if not (Diagnostic.has_errors diags) then
        Output.text output "typecheck: ok\n"
    in
    stop_at Check emit_check_result;
    stop_at Tast (fun () -> Output.text output (show_tdecls tdecls));
    if stage = Bin && not load_had_errors then check_has_main diags tdecls;
    render_and_exit_if_failed program diags;

    let frontend_time = Unix.gettimeofday () -. total_start in
    let codegen_start = Unix.gettimeofday () in

    let mir = Mir.build tdecls in
    begin match Mir.verify mir with
    | Ok () -> ()
    | Error errors ->
        List.iter
          (fun (error : Mir.error) ->
            Diagnostic.emit diags
              (Diagnostic.internal ~span:error.Mir.error_span
                 (Mir.show_error error)))
          errors;
        render_and_exit_if_failed program diags;
        raise Exit
    end;
    stop_at Mir (fun () -> Output.text output (Mir.dump mir));

    let source_at = Program.source_at program in
    let source_of pos =
      let source = source_at pos in
      (source.Program.filename, source.Program.source_map)
    in

    match backend with
    | Backend.X86 ->
        let object_bytes = Codegenx86.emit ~source_of mir in
        let codegen_time = Unix.gettimeofday () -. codegen_start in
        stop_at Obj (fun () -> Output.bytes output object_bytes);

        let link_start = Unix.gettimeofday () in
        link_object ~show_commands:stats (Target.host ())
          (Output.base output filename)
          object_bytes libraries;
        let link_time = Unix.gettimeofday () -. link_start in
        report_stats stats program ~total_start ~frontend_time ~qbe_time:None
          ~compiler_time:(frontend_time +. codegen_time)
          ~link_time
    | Backend.Qbe ->
        let il = Codegenqbe.emit ~source_of mir in
        let codegen_time = Unix.gettimeofday () -. codegen_start in

        stop_at Qbe (fun () -> Output.text output il);
        let qbe = Config.qbe () in
        stop_at Asm (fun () -> Output.text output (emit_asm qbe il));
        stop_at Obj (fun () ->
            Output.bytes output (emit_obj ~qbe (Target.host ()) il));

        let qbe_time, backend_time, link_time =
          compile_binary ~show_commands:stats ~qbe (Target.host ())
            (Output.base output filename)
            il libraries
        in
        report_stats stats program ~total_start ~frontend_time
          ~qbe_time:(Some qbe_time)
          ~compiler_time:(frontend_time +. codegen_time +. backend_time)
          ~link_time
  with
  | Exit -> ()
  | Codegenx86.Unsupported msg -> die msg
  | Diagnostic.Errors ds ->
      render_program program ds;
      exit 1
