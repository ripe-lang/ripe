(* SPDX-License-Identifier: Apache-2.0 *)

type t

val resolve : Ast.decl list -> t
val sym_at : t -> Ast.span -> Symbol.t
val sym_at_opt : t -> Ast.span -> Symbol.t option
val shadowed_at : t -> Ast.span -> Symbol.t option
val keyname_of : Symbol.t -> Keyname.t
val local_decls : t -> Ast.decl list
val builtins : t -> (Symbol.key * Types.ty) list

(* This is the `--emit resolve` output *)
val dump : t -> string
