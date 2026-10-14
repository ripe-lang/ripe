(* SPDX-License-Identifier: Apache-2.0 *)

val emit : Mir.program -> Llvm.llmodule
val object_file : Llvm.llmodule -> string
