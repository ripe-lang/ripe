(* SPDX-License-Identifier: GPL-2.0-only *)

type stage =
  | Tokens
  | Ast
  | Resolve
  | Tast
  | Check
  | Mir
  | Qbe
  | Asm
  | Obj
  | Bin

val stage_name : stage -> string

module Backend : sig
  type t = Qbe | X86

  val name : t -> string
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
