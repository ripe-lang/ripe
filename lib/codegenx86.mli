(* SPDX-License-Identifier: Apache-2.0 *)

type reg =
  | Rax
  | Rcx
  | Rdx
  | Rbx
  | Rsp
  | Rbp
  | Rsi
  | Rdi
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15

type width = W32 | W64

type instr =
  | Mov_imm of width * reg * int64
  | Mov_reg of width * reg * reg
  | Call of string
  | Ret
  | Syscall

type t

exception Unsupported of string

val create : ?size:int -> unit -> t
val instr : t -> instr -> unit
val finish : t -> labels:(string * int) list -> string
val emit_mir : source_of:(int -> string * Sourcemap.t) -> Mir.program -> string
