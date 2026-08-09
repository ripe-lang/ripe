(* SPDX-License-Identifier: GPL-2.0-only *)

type t

val host : unit -> t
val command_env : t -> string
val assembler_args : t -> output:string -> input:string -> string list

val linker_args :
  t ->
  output:string ->
  object_file:string ->
  runtime:string ->
  libraries:string list ->
  string list
