(* SPDX-License-Identifier: Apache-2.0 *)

type id = int [@@deriving show { with_path = false }]

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
[@@deriving show { with_path = false }]

type t = {
  id : id;
  name : string;
  link_name : string;
  kind : kind;
  entry_point : bool;
  span : Ast.span;
  name_span : Ast.span;
}
[@@deriving show { with_path = false }]

type key = int [@@deriving show { with_path = false }]

let unresolved_key = -1
let key symbol = symbol.id

module Table = Hashtbl.Make (Int)

let is_func = function Func | LocalFunc | Extern -> true | _ -> false
let is_global = function Global -> true | _ -> false
let is_immutable = function ForVar | MatchBind | Param -> true | _ -> false

let describe_kind = function
  | Func | LocalFunc | Extern -> "a function"
  | Type | LocalType -> "a type"
  | Error | Global | Local | Param | ForVar | MatchBind -> "a value"
