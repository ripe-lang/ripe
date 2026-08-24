(* SPDX-License-Identifier: Apache-2.0 *)

val qbe : unit -> string
val assembler : unit -> string
val linker : unit -> string
val runtime_object : unit -> string
val split_paths : ?sep:char -> string -> string list
val search_roots : ?root_filename:string -> unit -> string list
