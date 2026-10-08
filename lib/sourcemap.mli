(* SPDX-License-Identifier: Apache-2.0 *)

type t

val create : string -> t
val src : t -> string
val lookup : t -> int -> int * int
val line_bounds : t -> int -> int * int
