(* SPDX-License-Identifier: GPL-2.0-only *)

(* TODO:  need a total rewrite of this once *)

let dump_ast = ref false
let do_typecheck = ref false
let emit_qbe = ref false
let file = ref ""

let spec =
  [
    ("-dump-ast", Arg.Set dump_ast, "Dump parsed AST");
    ("-typecheck", Arg.Set do_typecheck, "Run the typechecker");
    ("-emit-qbe", Arg.Set emit_qbe, "Emit QBE IL to stdout (not compile)");
  ]

let parse_file filename =
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  Lexing.set_filename lexbuf filename;
  Ripe.Lexer.paren_depth := 0;
  let decls =
    match Ripe.Parser.program Ripe.Lexer.read lexbuf with
    | decls ->
        close_in ic;
        decls
    | exception Ripe.Parser.Error ->
        close_in ic;
        let pos = lexbuf.lex_curr_p in
        Printf.eprintf "%s:%d:%d: syntax error\n" pos.pos_fname pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol);
        exit 1
  in

  if !dump_ast then
    List.iter (fun d -> print_endline (Ripe.Ast.decl_to_string d)) decls;
  if !do_typecheck || !emit_qbe || not !dump_ast then
    match Ripe.Typechecker.typecheck decls with
    | tdecls ->
        if !do_typecheck then print_endline "typecheck: ok"
        else
          let il = Ripe.Codegen.emit_qbe tdecls in
          if !emit_qbe then print_string il
          else
            let base = Filename.remove_extension (Filename.basename !file) in
            let tmp_qbe = Filename.temp_file "ripe" ".ssa" in
            let tmp_asm = Filename.temp_file "ripe" ".s" in
            let oc = open_out tmp_qbe in
            output_string oc il;
            close_out oc;
            let run cmd =
              if Sys.command cmd <> 0 then (
                Printf.eprintf "ripec: command failed: %s\n" cmd;
                exit 1)
            in
            run (Printf.sprintf "qbe -o %s %s" tmp_asm tmp_qbe);
            run (Printf.sprintf "cc -o %s %s" base tmp_asm);
            Sys.remove tmp_qbe;
            Sys.remove tmp_asm
    | exception Ripe.Typechecker.TypeError msg ->
        Printf.eprintf "typecheck error: %s\n" msg;
        exit 1

let () =
  (* TODO: Update usage text for options *)
  Arg.parse spec (fun f -> file := f) "Usage: ripec file.rp>";
  if !file = "" then Arg.usage spec "Usage: ripec <file.rp>"
  else parse_file !file
