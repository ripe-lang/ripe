(* SPDX-License-Identifier: GPL-2.0-only *)

(* Names become ints so a syntax node holds no pointer and two names compare
   without touching the bytes *)
type id = int

val intern : string -> id
val text : id -> string
val pp : Format.formatter -> id -> unit
