(* SPDX-License-Identifier: GPL-2.0-only *)

val qbe : string
val runtime_object : unit -> string
val split_paths : ?sep:char -> string -> string list
val search_roots : unit -> string list
