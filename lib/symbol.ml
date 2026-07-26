(* SPDX-License-Identifier: GPL-2.0-only *)

type id = int [@@deriving show { with_path = false }]
type module_id = int [@@deriving show { with_path = false }]
type visibility = Private | Public [@@deriving show { with_path = false }]

type kind =
  | Func
  | Extern
  | Global
  | Type
  | Local of Ast.binding_kind
  | Param
  | ForVar
[@@deriving show { with_path = false }]

type t = {
  id : id;
  module_id : module_id;
  name : string;
  link_name : string;
  kind : kind;
  visibility : visibility;
  span : Ast.span;
}
[@@deriving show { with_path = false }]

type key = module_id * id

let key (symbol : t) : key = (symbol.module_id, symbol.id)

let is_func (kind : kind) : bool =
  match kind with
  | Func | Extern -> true
  | Global | Type | Local _ | Param | ForVar -> false

let is_global (kind : kind) : bool =
  match kind with
  | Global -> true
  | Func | Extern | Type | Local _ | Param | ForVar -> false

let is_immutable (kind : kind) : bool =
  match kind with
  | Local (Ast.Let | Ast.Comptime) | ForVar -> true
  | Func | Extern | Global | Type | Local Ast.Var | Param -> false

let is_comptime (kind : kind) : bool =
  match kind with
  | Local Ast.Comptime -> true
  | Func | Extern | Global | Type | Local (Ast.Var | Ast.Let) | Param | ForVar
    ->
      false
