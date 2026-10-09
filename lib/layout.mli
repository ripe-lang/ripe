(* SPDX-License-Identifier: Apache-2.0 *)

open Types

type t

val create : unit -> t
val set_struct_fields : t -> Symbol.key -> ty list -> unit
val struct_fields : t -> Symbol.key -> ty iarray
val struct_field_ty : t -> Keyname.t -> int -> ty
val field_offset : t -> Keyname.t -> int -> int
val ty_size : t -> ty -> int
val ty_align : t -> ty -> int
val stride : t -> ty -> int
val align_to : int -> int -> int
