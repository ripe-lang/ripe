(* SPDX-License-Identifier: GPL-2.0-only *)

type t

val create : string -> t
val src : t -> string
val lookup : t -> int -> int * int
val span_to_locs : t -> Ast.span -> int * int * int * int
val line_bounds : t -> int -> int * int
