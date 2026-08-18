(* SPDX-License-Identifier: Apache-2.0 *)

type t

val host : unit -> t
val assembler_args : t -> output:string -> input:string -> string list

val linker_args :
  t ->
  output:string ->
  object_file:string ->
  runtime:string ->
  libraries:string list ->
  string list
