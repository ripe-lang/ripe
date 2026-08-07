(* SPDX-License-Identifier: GPL-2.0-only *)

type t

val pp : Format.formatter -> t -> unit
val make : Symbol.key -> string list -> string -> t
val unresolved : string -> t
val show : t -> string
val key : t -> Symbol.key
val show_in : string list -> t -> string
