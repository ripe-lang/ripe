(* SPDX-License-Identifier: GPL-2.0-only *)

type t

val resolve :
  diags:Diagnostic.sink -> module_id:Symbol.module_id -> Ast.decl list -> t

val sym_at : t -> Ast.span -> Symbol.t
val sym_at_opt : t -> Ast.span -> Symbol.t option
val qname_of : t -> Symbol.t -> Qname.t

(* This is the `--emit resolve` output *)
val dump : t -> string
