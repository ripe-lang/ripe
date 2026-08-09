(* SPDX-License-Identifier: GPL-2.0-only *)

type stage = Tokens | Ast | Resolve | Tast | Check | Core | Qbe | Asm | Bin

module Backend : sig
  type t = Qbe
end

val compile :
  stage:stage ->
  backend:Backend.t ->
  out:string ->
  libraries:string list ->
  search_roots:string list ->
  stats:bool ->
  filename:string ->
  unit
