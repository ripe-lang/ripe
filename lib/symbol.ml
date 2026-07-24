(* SPDX-License-Identifier: GPL-2.0-only *)

type id = int [@@deriving show { with_path = false }]

type kind =
  | Func
  | Extern
  | Global
  | Type
  | Local of Ast.binding_kind
  | Param
  | ForVar
[@@deriving show { with_path = false }]

type t = { id : id; name : string; kind : kind; span : Ast.span }
[@@deriving show { with_path = false }]

let is_func = function Func | Extern -> true | _ -> false
let is_global = function Global -> true | _ -> false

let is_immutable = function
  | Local (Ast.Let | Ast.Comptime) | ForVar -> true
  | _ -> false

let is_comptime = function Local Ast.Comptime -> true | _ -> false
