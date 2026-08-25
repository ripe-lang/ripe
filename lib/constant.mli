(* SPDX-License-Identifier: Apache-2.0 *)

open Types

type exact

type value =
  | VInt of exact * int_kind
  | VFloat of float * float_kind
  | VBool of bool
  | VChar of int

val zero : exact
val of_magnitude : ?neg:bool -> int64 -> exact
val exact_of : value -> exact
val int_of : value -> int64
val representable : int_kind -> exact -> bool
val of_float : float_kind -> float -> value
val of_literal : ty -> int64 -> value
val cast : ty -> value -> value
val unop : Ast.unop -> result_ty:ty -> value -> value option
val binop : Ast.binop -> result_ty:ty -> value -> value -> value option
val unsupported_const : Ast.span -> Diagnostic.t
