(* SPDX-License-Identifier: Apache-2.0 *)

type site = { file : int; line : int; col : int; func : int }
type t

val create : string -> Sourcemap.t -> t
val enter_func : t -> string -> unit
val record : t -> Ast.span -> int
val strings : t -> string list
val sites : t -> site list
