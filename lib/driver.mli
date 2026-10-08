(* SPDX-License-Identifier: Apache-2.0 *)

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

val compile :
  stage:stage ->
  out:string ->
  libraries:string list ->
  search_roots:string list ->
  filename:string ->
  unit
