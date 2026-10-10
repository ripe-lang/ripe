(* SPDX-License-Identifier: Apache-2.0 *)

type id = int
type key = private int

type kind =
  | Error
  | Func
  | LocalFunc
  | Extern
  | Global
  | Type
  | LocalType
  | Local
  | Param
  | ForVar
  | MatchBind

type t = {
  id : id;
  name : string;
  link_name : string;
  kind : kind;
  entry_point : bool;
  span : Ast.span;
  name_span : Ast.span;
}

val pp : Format.formatter -> t -> unit
val pp_id : Format.formatter -> id -> unit
val pp_key : Format.formatter -> key -> unit
val pp_kind : Format.formatter -> kind -> unit
val show_kind : kind -> string
val key : t -> key
val unresolved_key : key

module Table : Hashtbl.S with type key = key

val is_func : kind -> bool
val is_global : kind -> bool
val is_immutable : kind -> bool
val describe_kind : kind -> string
