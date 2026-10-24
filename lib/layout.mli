(* SPDX-License-Identifier: Apache-2.0 *)

open Types

type t

val tag_ty : ty
val create : unit -> t
val set_variants : t -> Symbol.key -> adt_kind -> ty list list -> unit
val tagged : t -> Symbol.key -> bool
val has_payload : t -> ty -> bool
val named_aggregate : t -> ty -> Keyname.t option
val is_aggregate : t -> ty -> bool
val variants : t -> Symbol.key -> ty iarray iarray
val field_ty : t -> Keyname.t -> variant:int -> field:int -> ty
val field_offset : t -> Keyname.t -> variant:int -> field:int -> int
val ty_size : t -> ty -> int
val ty_align : t -> ty -> int
val stride : t -> ty -> int
val align_to : int -> align:int -> int
