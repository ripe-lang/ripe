(* SPDX-License-Identifier: Apache-2.0 *)

type toolchain = { assembler : string list; linker : string list }

val qbe : unit -> string
val assembler : unit -> string
val linker : unit -> string
val toolchain : unit -> toolchain
val runtime_object : unit -> string
