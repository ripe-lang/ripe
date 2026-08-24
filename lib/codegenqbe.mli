(* SPDX-License-Identifier: Apache-2.0 *)

val emit : source_of:(int -> string * Sourcemap.t) -> Mir.program -> string
