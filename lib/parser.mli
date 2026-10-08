(* SPDX-License-Identifier: Apache-2.0 *)

exception Failed

val parse :
  (Lexing.lexbuf -> Tokens.token * Ast.span) -> Lexing.lexbuf -> Ast.decl list
