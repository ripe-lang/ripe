(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens
open Ast

exception ParseError of string

type state = {
  mutable tok : token;
  lexbuf : Lexing.lexbuf;
  read : Lexing.lexbuf -> token;
}

let advance st = st.tok <- st.read st.lexbuf

let skip_semi st =
  while st.tok = SEMI do
    advance st
  done

let parse_decl st =
  skip_semi st;
  match st.tok with _ -> raise (ParseError "Expected declaration")

let parse_program st =
  let decls = ref [] in
  skip_semi st;
  while st.tok <> EOF do
    decls := parse_decl st :: !decls;
    skip_semi st
  done;
  List.rev !decls

let parse (read : Lexing.lexbuf -> Tokens.token) (lexbuf : Lexing.lexbuf) :
    Ast.decl list =
  let st = { tok = read lexbuf; lexbuf; read } in
  parse_program st
