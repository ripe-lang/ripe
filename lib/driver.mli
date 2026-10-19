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
  type t = Qbe | X86 | Llvm

  val name : t -> string
end

val compile : stage -> Backend.t -> out:string -> string list -> string -> unit
