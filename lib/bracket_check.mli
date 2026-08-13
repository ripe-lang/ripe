(* SPDX-License-Identifier: GPL-2.0-only *)

type opener
type opened = opener * Ast.span
type step = End | Closed of opened list | Stray | Open of opener | Other

val step : Diagnostic.sink -> opened list -> Tokens.token -> Ast.span -> step
