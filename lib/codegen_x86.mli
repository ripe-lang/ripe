(* SPDX-License-Identifier: Apache-2.0 *)

exception Unsupported of string

val emit_mir : source_of:(int -> string * Source_map.t) -> Mir.program -> string
