(* SPDX-License-Identifier: Apache-2.0 *)

type id = int [@@deriving show { with_path = false }]
type module_id = int [@@deriving show { with_path = false }]
type visibility = Private | Public [@@deriving show { with_path = false }]

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
  name_span : Ast.span;
}
[@@deriving show { with_path = false }]

type key = int [@@deriving show { with_path = false }]

let id_bits = 32
let id_mask = 0xFFFF_FFFF

let make_key (module_id : module_id) (id : id) : key =
  (module_id lsl id_bits) lor (id land id_mask)

let key (symbol : t) : key = make_key symbol.module_id symbol.id
let module_id_of_key (key : key) : module_id = key asr id_bits
let id_of_key (key : key) : id = key land id_mask

module Table = Hashtbl.Make (struct
  type t = key

  let equal (a : t) (b : t) : bool = a = b

  (* Buckets index off the low bits so the module has to be folded in *)
  let hash (key : t) : int = key lxor (key asr id_bits)
end)

let prelude_module_id : module_id = -2

let is_func : kind -> bool = function
  | Func | LocalFunc | Extern -> true
  | _ -> false

let is_global : kind -> bool = function Global _ -> true | _ -> false

let is_immutable : kind -> bool = function
  | Local Ast.Comptime | ForVar | Module | MatchBind -> true
  | _ -> false

let is_comptime : kind -> bool = function
  | Local Ast.Comptime | Global Ast.Comptime -> true
  | _ -> false
