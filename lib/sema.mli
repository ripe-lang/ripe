(* SPDX-License-Identifier: Apache-2.0 *)

(* The partial tree stays available so later checks can still run *)
val analyze : Resolve.t -> Ast.decl list -> Tast.tdecl list
