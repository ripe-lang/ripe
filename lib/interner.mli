(* SPDX-License-Identifier: Apache-2.0 *)

(* A name becomes an int so a syntax node holds no pointer and two names compare
   without touching the bytes *)
type id = int

val intern : string -> id
val text : id -> string
val pp : Format.formatter -> id -> unit
