(* SPDX-License-Identifier: GPL-2.0-only *)

type stage = Tokens | Ast | Resolve | Tast | Check | Mir | Qbe | Asm | Bin

module Backend = struct
  type t = Qbe
end

let read_file filename = In_channel.with_open_bin filename In_channel.input_all
let list_dir dir = Array.to_list (Sys.readdir dir)

let use_color () =
  match Sys.getenv_opt "NO_COLOR" with
  | Some value when value <> "" -> false
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

(* Lower IL through qbe and return the emitted asm path *)
let run_qbe il =
  let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
  let tmp_asm = Filename.temp_file "ripe" ".s" in
  Out_channel.with_open_text tmp_qbe (fun oc -> output_string oc il);
  run (Printf.sprintf "%s -o %s %s" Config.qbe tmp_asm tmp_qbe);
  Sys.remove tmp_qbe;
  tmp_asm

let compile_binary base il libraries =
  let tmp_asm = run_qbe il in
  let args =
    [ "cc"; "-o"; base; tmp_asm; Config.runtime_object () ]
    @ List.map (fun library -> "-l" ^ library) libraries
  in
  run (String.concat " " (List.map Filename.quote args));
  Sys.remove tmp_asm

let emit_asm il =
  let tmp_asm = run_qbe il in
  let asm = read_file tmp_asm in
  Sys.remove tmp_asm;
  asm

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
      (fun import ->
        "import " ^ String.concat "." (List.map Interner.text import.Ast.path))
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
  String.concat "\n" (List.map Typed_ast.show_tdecl tdecls) ^ "\n"

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
    match decl with
    | Typed_ast.TFunc fd -> fd.Typed_ast.entry_point
    | _ -> false
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

let compile ~stage ~backend ~out ~libraries ~search_roots ~filename =
  (* Write to -o if set or stdout *)
  let output_text s =
    if out = "" then print_string s
    else Out_channel.with_open_text out (fun oc -> output_string oc s)
  in
  let output_binary il =
    let base =
      if out = "" then Filename.remove_extension (Filename.basename filename)
      else out
    in
    compile_binary base il libraries
  in
  if stage = Tokens then (
    output_text (root_tokens filename);
    exit 0);
  let diags = Diagnostic.sink () in
  let program = load ~diags ~search_roots ~filename in
  (* A missing main is noise once the program failed to load *)
  let load_had_errors = Diagnostic.has_errors diags in
  let render_and_exit_if_failed () =
    let failed = Diagnostic.has_errors diags in
    render_program program (Diagnostic.take diags);
    if failed then exit 1
  in
  (* Each stage is a possible stopping point so bail once we hit the target *)
  let stop_at target emit =
    if stage = target then (
      emit ();
      render_and_exit_if_failed ();
      raise Exit)
  in
  try
    stop_at Ast (fun () -> output_text (show_program program));
    let resolved = Resolve.resolve_program ~diags program in
    let uses = resolved.Resolve.uses in
    let decls = resolved.Resolve.decls in
    stop_at Resolve (fun () -> output_text (Resolve.dump uses));
    let tdecls = Typechecker.typecheck ~diags uses decls in
    let emit_check_result () =
      if not (Diagnostic.has_errors diags) then output_text "typecheck: ok\n"
    in
    stop_at Check emit_check_result;
    stop_at Tast (fun () -> output_text (show_tdecls tdecls));
    if stage = Bin && not load_had_errors then check_has_main diags tdecls;
    render_and_exit_if_failed ();
    let mir = Mir_build.build tdecls in
    Mir_verify.verify mir;
    stop_at Mir (fun () -> output_text (Mir_dump.program mir));
    let source_of pos =
      let source = Program.source_at program pos in
      (source.Program.filename, source.Program.source_map)
    in
    let il =
      match backend with Backend.Qbe -> Codegen_qbe.emit_mir ~source_of mir
    in
    stop_at Qbe (fun () -> output_text il);
    stop_at Asm (fun () -> output_text (emit_asm il));
    output_binary il
  with
  | Exit -> ()
  | Diagnostic.Errors ds ->
      render_program program ds;
      exit 1
