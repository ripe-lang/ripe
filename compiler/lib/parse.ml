(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens
open Ast

exception ParseError of string

let parse (read : Lexing.lexbuf -> Tokens.token) (lexbuf : Lexing.lexbuf) :
    Ast.decl list =
  ignore (read, lexbuf);
  failwith "TODO: recursive descent parser"
