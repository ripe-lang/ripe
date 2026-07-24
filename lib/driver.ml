(* SPDX-License-Identifier: GPL-2.0-only *)

type stage = Tokens | Ast | Resolve | Tast | Check | Core | Qbe | Asm | Bin

module Backend = struct
  type t = Qbe
end

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

let die msg =
  let label =
    Diagnostic.severity_label (Unix.isatty Unix.stderr) Diagnostic.Error
  in
  Printf.eprintf "%s: %s\n" label msg;
  exit 2

let run cmd =
  if Sys.command cmd <> 0 then (
    let label =
      Diagnostic.severity_label (Unix.isatty Unix.stderr) Diagnostic.Error
    in
    Printf.eprintf "%s: command failed: %s\n" label cmd;
    exit 1)

(* lower IL through qbe and return the emitted asm path *)
let run_qbe il =
  let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
  let tmp_asm = Filename.temp_file "ripe" ".s" in
  Out_channel.with_open_text tmp_qbe (fun oc -> output_string oc il);
  run (Printf.sprintf "%s -o %s %s" Config.qbe tmp_asm tmp_qbe);
  Sys.remove tmp_qbe;
  tmp_asm

let compile_binary base il =
  let tmp_asm = run_qbe il in
  run (Printf.sprintf "cc -o %s %s %s" base tmp_asm (Config.runtime_object ()));
  Sys.remove tmp_asm

let emit_asm il =
  let tmp_asm = run_qbe il in
  let asm = read_file tmp_asm in
  Sys.remove tmp_asm;
  asm

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

let show_module (module_ : Ast.module_) =
  let imports =
    List.map
      (fun import -> "import " ^ String.concat "." import.Ast.path)
      module_.imports
  in
  String.concat "\n" (imports @ List.map Ast.show_decl module_.decls) ^ "\n"

let show_tdecls tdecls =
  String.concat "\n" (List.map Typed_ast.show_tdecl tdecls) ^ "\n"

let show_cdecls cdecls =
  String.concat "\n" (List.map Core.show_cdecl cdecls) ^ "\n"

let render_all ctx diags =
  List.iter (fun d -> Printf.eprintf "%s" (Diagnostic.render ctx d)) diags

(* the C runtime we link calls main, so refuse before the linker leaks its own
   error *)
let check_has_main tdecls =
  let is_main decl =
    match decl with Typed_ast.TFunc { name; _ } -> name = "main" | _ -> false
  in
  if not (List.exists is_main tdecls) then
    raise
      (Diagnostic.Errors
         [
           Diagnostic.error "no `main` function found"
           |> Diagnostic.help "add a `func main() i32` entry point";
         ])

(* read the source and build a fresh lexer plus the diagnostic context *)
let load filename =
  if not (Sys.file_exists filename) then
    die (Printf.sprintf "no such file %s" filename);
  let abs_filename = Unix.realpath filename in
  (* TODO(5d10): emit paths relative to project root *)
  let src = read_file filename in
  if not (String.is_valid_utf_8 src) then
    die (Printf.sprintf "%s: not valid UTF-8" filename);
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf abs_filename;
  let read = Lexer.read (Lexer.make_state 0) in
  let ctx =
    {
      Diagnostic.sm = Source_map.create src;
      filename = abs_filename;
      color = Unix.isatty Unix.stderr;
    }
  in
  (read, lexbuf, ctx)

let compile ~stage ~backend ~out ~filename =
  (* write to -o if set, else stdout *)
  let output_text s =
    if out = "" then print_string s
    else Out_channel.with_open_text out (fun oc -> output_string oc s)
  in
  let output_binary il =
    let base =
      if out = "" then Filename.remove_extension (Filename.basename filename)
      else out
    in
    compile_binary base il
  in
  let read, lexbuf, ctx = load filename in
  (* each stage is a possible stopping point, so bail once we hit the target *)
  let stop_at target emit =
    if stage = target then (
      emit ();
      raise Exit)
  in
  try
    stop_at Tokens (fun () -> output_text (dump_tokens read lexbuf));
    let module_ = Parser.parse read lexbuf in
    let decls = module_.Ast.decls in
    stop_at Ast (fun () -> output_text (show_module module_));
    let uses = Resolve.resolve ~module_id:0 decls in
    stop_at Resolve (fun () -> output_text (Resolve.dump uses));
    let tdecls, warns = Typechecker.typecheck uses decls in
    render_all ctx warns;
    stop_at Check (fun () -> output_text "typecheck: ok\n");
    stop_at Tast (fun () -> output_text (show_tdecls tdecls));
    let cdecls = Lower.lower tdecls in
    stop_at Core (fun () -> output_text (show_cdecls cdecls));
    let il = match backend with Backend.Qbe -> Codegen_qbe.emit_qbe cdecls in
    stop_at Qbe (fun () -> output_text il);
    stop_at Asm (fun () -> output_text (emit_asm il));
    check_has_main tdecls;
    output_binary il
  with
  | Exit -> ()
  | Diagnostic.Errors diags ->
      render_all ctx diags;
      exit 1
