(* SPDX-License-Identifier: Apache-2.0 *)

exception InvalidUtf8 of string
exception SourceTooLarge of string

type t = { filename : string; source_map : Sourcemap.t; decls : Ast.decl list }

let load filename =
  let src = In_channel.with_open_bin filename In_channel.input_all in
  (* The lexer walks bytes so it would split a character in half *)
  if not (String.is_valid_utf_8 src) then raise (InvalidUtf8 filename);
  if String.length src > Span.max_offset then raise (SourceTooLarge filename);
  let source_map = Sourcemap.create src in
  let lexbuf = Lexer.lexbuf_of_string src in
  let decls = try Parser.parse Lexer.read lexbuf with Parser.Failed -> [] in
  { filename; source_map; decls }
