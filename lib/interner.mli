(* SPDX-License-Identifier: Apache-2.0 *)

(* Names become ints so a syntax node holds no pointer and two names compare
   without touching the bytes *)
type id = int

val intern : string -> id
val text : id -> string
val pp : Format.formatter -> id -> unit
