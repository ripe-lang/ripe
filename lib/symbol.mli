(* SPDX-License-Identifier: Apache-2.0 *)

type id = int
type module_id = int

(* A module and an id packed together so a symbol compares and hashes as one *)
type key = private int
type visibility = Private | Public

type kind =
  | Error
  | Func
  | LocalFunc
  | Extern
  | Global of Ast.binding_kind
  | Type
  | LocalType
  | Local of Ast.binding_kind
  | Param
  | ForVar
  | Module
  | MatchBind

type t = {
  id : id;
  module_id : module_id;
  name : string;
  link_name : string;
  kind : kind;
  visibility : visibility;
  entry_point : bool;
  span : Ast.span;
  name_span : Ast.span;
}

val pp : Format.formatter -> t -> unit
val pp_id : Format.formatter -> id -> unit
val pp_module_id : Format.formatter -> module_id -> unit
val pp_key : Format.formatter -> key -> unit
val pp_kind : Format.formatter -> kind -> unit
val show_kind : kind -> string
val pp_visibility : Format.formatter -> visibility -> unit
val show_visibility : visibility -> string
val prelude_module_id : module_id
val key : t -> key
val unresolved_key : key
val module_id_of_key : key -> module_id
val id_of_key : key -> id

module Table : Hashtbl.S with type key = key

val is_func : kind -> bool
val is_global : kind -> bool
val is_immutable : kind -> bool
val is_const : kind -> bool
