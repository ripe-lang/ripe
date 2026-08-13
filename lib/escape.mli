(* SPDX-License-Identifier: GPL-2.0-only *)

type escape = Slice | Address

val return_escapes : Typed_ast.texpr -> escape option
