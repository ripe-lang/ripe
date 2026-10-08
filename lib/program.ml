(* SPDX-License-Identifier: Apache-2.0 *)

exception Invalid_utf8 of string
exception Source_too_large of string

type t = { filename : string; source_map : Sourcemap.t; decls : Ast.decl list }

let load ~(read_file : string -> string) filename =
  let src = read_file filename in
  (* The lexer walks bytes so it would split a character in half *)
  if not (String.is_valid_utf_8 src) then raise (Invalid_utf8 filename);
  if String.length src > Span.max_offset then raise (Source_too_large filename);
  let source_map = Sourcemap.create src in
  let lexbuf = Lexer.lexbuf_of_string src in
  let read = Lexer.read (Lexer.make_state ()) in
  let decls = try Parser.parse read lexbuf with Parser.Failed -> [] in
  { filename; source_map; decls }
