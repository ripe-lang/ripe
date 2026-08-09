(* SPDX-License-Identifier: GPL-2.0-only *)

val emit_mir : source_of:(int -> string * Source_map.t) -> Mir.program -> string
