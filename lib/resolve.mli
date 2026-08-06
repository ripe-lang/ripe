(* SPDX-License-Identifier: GPL-2.0-only *)

type t
type resolved_program = { uses : t; decls : Ast.decl list }

val resolve :
  diags:Diagnostic.sink -> module_id:Symbol.module_id -> Ast.decl list -> t

val resolve_program : diags:Diagnostic.sink -> Program.t -> resolved_program
val sym_at : t -> Ast.span -> Symbol.t
val sym_at_opt : t -> Ast.span -> Symbol.t option
val qname_of : t -> Symbol.t -> Qname.t
val local_decls : t -> Ast.decl list
val module_path_at : t -> Ast.span -> string list
val builtins : t -> (Symbol.key * Types.builtin) list

(* This is the `--emit resolve` output *)
val dump : t -> string
