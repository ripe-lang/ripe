(* SPDX-License-Identifier: GPL-2.0-only *)

type stage = Tokens | Ast | Tast | Check | Qbe | Asm | Bin

let stage_of_string = function
  | "tokens" -> Tokens
  | "ast" -> Ast
  | "tast" -> Tast
  | "check" -> Check
  | "qbe" -> Qbe
  | "asm" -> Asm
  | s ->
      Printf.eprintf "ripec: unknown emit stage: %s\n" s;
      exit 2

let stage = ref Bin
let out = ref ""
let file = ref ""

let usage_msg =
  "The Ripe compiler\n\n\
   Usage:\n\
  \  ripec [options] <file.rp>\n\n\
   Options:\n\
  \  -emit <stage>   stop after a stage: tokens|ast|tast|check|qbe|asm\n\
  \  -o <file>       write the output here\n\
  \  -help           show this help\n"

let print_help () =
  print_string usage_msg;
  exit 0

let die msg =
  Printf.eprintf "ripec: %s\n" msg;
  exit 2

let read_file filename = In_channel.with_open_bin filename In_channel.input_all

(* write to -o if set, else stdout *)
let output_text s =
  if !out = "" then print_string s
  else Out_channel.with_open_text !out (fun oc -> output_string oc s)

let dump_tokens lexbuf =
  let buf = Buffer.create 256 in
  let rec loop () =
    let t = Ripe.Lexer.read lexbuf in
    Buffer.add_string buf (Ripe.Tokens.show_token t);
    Buffer.add_char buf '\n';
    if t <> Ripe.Tokens.EOF then loop ()
  in
  loop ();
  Buffer.contents buf

let parse lexbuf =
  match Ripe.Parser.parse Ripe.Lexer.read lexbuf with
  | decls -> decls
  | exception Ripe.Parser.ParseErrors diags ->
      List.iter
        (fun (pos, msg) ->
          Printf.eprintf "%s:%d:%d: %s\n" pos.Lexing.pos_fname pos.pos_lnum
            (pos.pos_cnum - pos.pos_bol)
            msg)
        diags;
      exit 1

let typecheck filename src decls =
  match Ripe.Typechecker.typecheck filename src decls with
  | tdecls -> tdecls
  | exception Ripe.Typechecker.TypeErrors msgs ->
      List.iter (fun msg -> Printf.eprintf "%s\n" msg) msgs;
      exit 1

let run cmd =
  if Sys.command cmd <> 0 then (
    Printf.eprintf "ripec: command failed: %s\n" cmd;
    exit 1)

let qbe = match Sys.getenv_opt "QBE" with Some p -> p | None -> "qbe"

(* lower IL through qbe and return the emitted asm path *)
let run_qbe il =
  let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
  let tmp_asm = Filename.temp_file "ripe" ".s" in
  Out_channel.with_open_text tmp_qbe (fun oc -> output_string oc il);
  run (Printf.sprintf "%s -o %s %s" qbe tmp_asm tmp_qbe);
  Sys.remove tmp_qbe;
  tmp_asm

let compile_binary base il =
  let tmp_asm = run_qbe il in
  run (Printf.sprintf "cc -o %s %s" base tmp_asm);
  Sys.remove tmp_asm

let emit_asm il =
  let tmp_asm = run_qbe il in
  let asm = read_file tmp_asm in
  Sys.remove tmp_asm;
  asm

let compile filename =
  let abs_filename = Unix.realpath filename in
  (* TODO(5d10): emit paths relative to project root *)
  let src = read_file filename in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf abs_filename;
  Ripe.Lexer.reset ();
  match !stage with
  | Tokens -> output_text (dump_tokens lexbuf)
  | _ -> (
      let decls = parse lexbuf in
      match !stage with
      | Tokens -> assert false
      | Ast ->
          output_text
            (String.concat "\n" (List.map (fun d -> Ripe.Ast.show_decl d) decls)
            ^ "\n")
      | _ -> (
          let tdecls = typecheck abs_filename src decls in
          match !stage with
          | Tokens | Ast -> assert false
          | Check -> output_text "typecheck: ok\n"
          | Tast ->
              output_text
                (String.concat "\n"
                   (List.map (fun d -> Ripe.Typed_ast.show_tdecl d) tdecls)
                ^ "\n")
          | _ -> (
              let il = Ripe.Codegen.emit_qbe tdecls in
              match !stage with
              | Qbe -> output_text il
              | Asm -> output_text (emit_asm il)
              | _ ->
                  let base =
                    if !out = "" then
                      Filename.remove_extension (Filename.basename !file)
                    else !out
                  in
                  compile_binary base il)))

let () =
  let rec parse_args = function
    | [] -> ()
    | ("-help" | "--help" | "-h") :: _ -> print_help ()
    | "-emit" :: s :: rest ->
        stage := stage_of_string s;
        parse_args rest
    | "-o" :: p :: rest ->
        out := p;
        parse_args rest
    | ("-emit" | "-o") :: [] -> die "missing argument"
    | f :: _ when String.length f > 0 && f.[0] = '-' ->
        die (Printf.sprintf "unknown option: %s" f)
    | f :: rest ->
        file := f;
        parse_args rest
  in
  parse_args (List.tl (Array.to_list Sys.argv));
  if !file = "" then print_help () else compile !file
