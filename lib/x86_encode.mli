(* SPDX-License-Identifier: Apache-2.0 *)

type t

val create : ?size:int -> unit -> t

(* The next byte lands here so a label can be noted before it exists *)
val offset : t -> int

val instr : t -> X86_ir.instr -> unit

(* A call only learns its distance once every label has an offset *)
val finish : t -> labels:(string * int) list -> string
