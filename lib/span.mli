(* SPDX-License-Identifier: Apache-2.0 *)

(* Offsets run across every source file at once so a file gets found by
   searching the bases instead of riding along in the span *)
type t

val make : int -> int -> t
val lo : t -> int
val hi : t -> int
val pp : Format.formatter -> t -> unit
val dummy : t

(* A span packs into one int so the caller has to stay inside this *)
val max_offset : int

module Table : Hashtbl.S with type key = t
