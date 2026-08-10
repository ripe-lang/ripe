(* SPDX-License-Identifier: GPL-2.0-only *)

val qbe_ty : Types.ty -> string
val qbe_load : Types.ty -> string
val qbe_store : Types.ty -> string
val qbe_ext_ty : (Qname.t -> string) -> Types.ty -> string
val alloc_instr : Types.ty list Symbol.Table.t -> Types.ty -> string
val emit_mir : source_of:(int -> string * Source_map.t) -> Mir.program -> string
