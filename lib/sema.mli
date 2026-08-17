(* SPDX-License-Identifier: GPL-2.0-only *)

(* The partial tree stays available so later checks can still run *)
val analyze :
  diags:Diagnostic.sink -> Resolve.t -> Ast.decl list -> Typed_ast.tdecl list
