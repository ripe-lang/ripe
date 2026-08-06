(* SPDX-License-Identifier: GPL-2.0-only *)

type id = int [@@deriving show { with_path = false }]
type module_id = int [@@deriving show { with_path = false }]
type visibility = Private | Public [@@deriving show { with_path = false }]

type kind =
  | Error
  | Func
  | Extern
  | Global
  | Type
  | LocalType
  | Local of Ast.binding_kind
  | Param
  | ForVar
  | Module
[@@deriving show { with_path = false }]

type t = {
  id : id;
  module_id : module_id;
  name : string;
  link_name : string;
  kind : kind;
  visibility : visibility;
  entry_point : bool;
  span : Ast.span;
}
[@@deriving show { with_path = false }]

type key = module_id * id [@@deriving show { with_path = false }]

let key (symbol : t) : key = (symbol.module_id, symbol.id)
let prelude_module_id : module_id = -2

let is_func (kind : kind) : bool =
  match kind with
  | Func | Extern -> true
  | Error | Global | Type | LocalType | Local _ | Param | ForVar | Module ->
      false

let is_global (kind : kind) : bool =
  match kind with
  | Global -> true
  | Error | Func | Extern | Type | LocalType | Local _ | Param | ForVar | Module
    ->
      false

let is_immutable (kind : kind) : bool =
  match kind with
  | Local (Ast.Let | Ast.Comptime) | ForVar | Module -> true
  | Error | Func | Extern | Global | Type | LocalType | Local Ast.Var | Param ->
      false

let is_comptime (kind : kind) : bool =
  match kind with
  | Local Ast.Comptime -> true
  | Error | Func | Extern | Global | Type | LocalType
  | Local (Ast.Var | Ast.Let)
  | Param | ForVar | Module ->
      false
