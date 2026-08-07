(* SPDX-License-Identifier: GPL-2.0-only *)

type file_id = int
type t = { file : file_id; lo : int; hi : int }

val make : file_id -> int -> int -> t
val pp : Format.formatter -> t -> unit
val show : t -> string
val dummy : t
