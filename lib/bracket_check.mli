(* SPDX-License-Identifier: GPL-2.0-only *)

type step = Done | Stray | Open | Other

val step :
  Diagnostic.sink ->
  (Tokens.token * Ast.span) list ->
  Tokens.token ->
  Ast.span ->
  step
