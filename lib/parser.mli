(* SPDX-License-Identifier: GPL-2.0-only *)

val parse :
  diags:Diagnostic.sink ->
  (Lexing.lexbuf -> Tokens.token * Ast.span * int) ->
  Lexing.lexbuf ->
  Ast.module_
