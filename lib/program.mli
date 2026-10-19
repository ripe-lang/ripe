(* SPDX-License-Identifier: Apache-2.0 *)

exception InvalidUtf8 of string
exception SourceTooLarge of string

type t = { filename : string; source_map : Sourcemap.t; decls : Ast.decl list }

val load : string -> t
