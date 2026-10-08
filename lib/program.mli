(* SPDX-License-Identifier: Apache-2.0 *)

exception Invalid_utf8 of string
exception Source_too_large of string

type t = { filename : string; source_map : Sourcemap.t; decls : Ast.decl list }

val load : read_file:(string -> string) -> string -> t
