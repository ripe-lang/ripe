(* SPDX-License-Identifier: GPL-2.0-only *)

(* TODO(1323): A counter is fine since I only compile one file but when I add
   modules I will need mangled names for the linker+ABI on top of the ids*)
type id = int [@@deriving show { with_path = false }]

type kind = Func | Extern | Global | Const | Var | Param | ForVar
[@@deriving show { with_path = false }]

type t = { id : id; name : string; kind : kind; span : Ast.span }
[@@deriving show { with_path = false }]

let is_func = function Func | Extern -> true | _ -> false
let is_global = function Global -> true | _ -> false
