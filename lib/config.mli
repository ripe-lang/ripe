(* SPDX-License-Identifier: GPL-2.0-only *)

val qbe : string
val assembler : string
val lld : string
val lld_library_path : string option
val runtime_object : unit -> string
val split_paths : ?sep:char -> string -> string list
val search_roots : ?root_filename:string -> unit -> string list
