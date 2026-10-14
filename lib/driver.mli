(* SPDX-License-Identifier: Apache-2.0 *)

type stage =
  | Tokens
  | Ast
  | Resolve
  | Tast
  | Check
  | Mir
  | Qbe
  | Llvm
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
  filename:string ->
  unit
