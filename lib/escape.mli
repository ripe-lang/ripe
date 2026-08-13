(* SPDX-License-Identifier: GPL-2.0-only *)

type escape = Slice | Address

val escapes : Typed_ast.texpr -> escape option
val assign_escapes : Typed_ast.texpr -> Typed_ast.texpr -> escape option
