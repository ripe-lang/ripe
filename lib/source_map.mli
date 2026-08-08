(* SPDX-License-Identifier: GPL-2.0-only *)

type t

(* Positions are global offsets so indexing into the text needs `rel` first *)
val create : base:int -> string -> t
val src : t -> string
val rel : t -> int -> int
val lookup : t -> int -> int * int
val span_to_locs : t -> Ast.span -> int * int * int * int
val line_bounds : t -> int -> int * int
