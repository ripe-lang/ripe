(* SPDX-License-Identifier: Apache-2.0 *)

open Types

val compatible : want:ty -> got:ty -> bool
val is_lvalue : Tast.texpr -> bool
val root_lvalue : Tast.texpr -> Tast.texpr option
val root_binding : Tast.texpr -> Symbol.t option
val is_integer : ty -> bool
val is_num_lit : Ast.expr -> bool
val suffix_kind : string -> int_kind
val float_suffix_kind : string -> float_kind
val cast_ok : src:ty -> tgt:ty -> bool
val widens_to : src:ty -> tgt:ty -> bool
val common_numeric_ty : ty -> ty -> ty option
val binop_accepts : Ast.binop -> ty -> bool
val unop_accepts : Ast.unop -> ty -> bool
