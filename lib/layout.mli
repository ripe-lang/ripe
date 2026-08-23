(* SPDX-License-Identifier: Apache-2.0 *)

open Types

type structs

val make_structs : unit -> structs
val set_struct_fields : structs -> Symbol.key -> ty list -> unit

val struct_fields : structs -> Symbol.key -> ty iarray
val struct_field_ty : structs -> Qname.t -> int -> ty
val field_offset : structs -> Qname.t -> int -> int
val ty_size : structs -> ty -> int
val ty_align : structs -> ty -> int

(* The number of bytes from one element to the next after round the alignment *)
val stride : structs -> ty -> int

val align_to : int -> int -> int
