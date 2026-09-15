(* SPDX-License-Identifier: Apache-2.0 *)

exception Unbalanced

val parse :
  diags:Diagnostic.sink ->
  (Lexing.lexbuf -> Tokens.token * Ast.span * int) ->
  Lexing.lexbuf ->
  Ast.module_
