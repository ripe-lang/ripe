(* SPDX-License-Identifier: Apache-2.0 *)

type t

val pp : Format.formatter -> t -> unit
val make : Symbol.key -> string list -> string -> t
val unresolved : string -> t
val show : t -> string
val key : t -> Symbol.key
val show_in : string list -> t -> string
