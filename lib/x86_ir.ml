(* SPDX-License-Identifier: Apache-2.0 *)

(* Reference *)
(* 1. Intel 64 and IA-32 Architectures Software Developer Manuals https://www.intel.com/content/www/us/en/developer/articles/technical/intel-sdm.html *)

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

let reg_index = function
  | Rax -> 0
  | Rcx -> 1
  | Rdx -> 2
  | Rbx -> 3
  | Rsp -> 4
  | Rbp -> 5
  | Rsi -> 6
  | Rdi -> 7
  | R8 -> 8
  | R9 -> 9
  | R10 -> 10
  | R11 -> 11
  | R12 -> 12
  | R13 -> 13
  | R14 -> 14
  | R15 -> 15

(* Extended registers carry their fourth index bit in REX [1] *)
let is_extended register = reg_index register > 7
