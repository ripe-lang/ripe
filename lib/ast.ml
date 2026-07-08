(* SPDX-License-Identifier: GPL-2.0-only *)

type span = Span.t = { lo : int; hi : int }

let pp_span = Span.pp
let show_span = Span.show
let dummy_span = Span.dummy

type typ_desc =
  | Named of string
  | Pointer of typ
  | FuncPtr of typ list * typ option
  (* TODO(f0b2): allow const-expr sizes, not just an int literal *)
  | Array of int * typ
  | Slice of typ

and typ = { tdesc : typ_desc; span : span }
[@@deriving show { with_path = false }]

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | And
  | Or
  | BitAnd
  | BitOr
  | BitXor
  | Lshift
  | Rshift
  | Assign
  | AddAssign
  | SubAssign
  | MulAssign
  | DivAssign
[@@deriving show { with_path = false }]

let show_binop_sym = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | Lshift -> "<<"
  | Rshift -> ">>"
  | Assign -> "="
  | AddAssign -> "+="
  | SubAssign -> "-="
  | MulAssign -> "*="
  | DivAssign -> "/="

type unop = Neg | Not | BitNot | Deref | AddressOf
[@@deriving show { with_path = false }]

type expr_desc =
  | Int of int64
  | Float of float
  | Bool of bool
  | Null
  | Char of char
  | String of string
  | Ident of string
  | Call of expr * expr list
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Range of expr * expr
  | RangeInclusive of expr * expr
  | FieldAccess of expr * string
  | Cast of expr * typ
  | SizeOf of typ
  | ArrayLit of expr list
  | Index of expr * expr
  | Undefined
  | StructLit of string * span * (string * span * expr) list
[@@deriving show { with_path = false }]

and expr = { desc : expr_desc; span : span }
[@@deriving show { with_path = false }]

(* TODO(68e6): Support tuple destructuring in var bindings e.g. var (a, b) = (x, y) *)
type stmt_desc =
  | Const of string * span * typ option * expr
  | Var of string * span * typ option * expr option
  | Return of expr option
  | If of (expr * stmt list) list * stmt list
  | While of expr * stmt list
  | For of string * span * expr * stmt list
  | Break
  | Continue
  | Expr of expr
  (* TODO(f0b0): blocks are statement only right now, make them usable as expressions like var p = { ... } *)
  | Block of stmt list
[@@deriving show { with_path = false }]

and stmt = { sdesc : stmt_desc; span : span }
[@@deriving show { with_path = false }]

type modifier = Pub | Inline [@@deriving show { with_path = false }]

type param = { name : string; typ : typ; span : span }
[@@deriving show { with_path = false }]

type field = {
  name : string;
  typ : typ;
  modifiers : modifier list;
  span : span;
}
[@@deriving show { with_path = false }]

type func_def = {
  name : string;
  params : param list;
  ret : typ option;
  body : stmt list;
  modifiers : modifier list;
  variadic : bool;
  span : span;
}
[@@deriving show { with_path = false }]

type struct_def = {
  name : string;
  fields : field list;
  modifiers : modifier list;
  span : span;
}
[@@deriving show { with_path = false }]

type global_def = {
  name : string;
  typ : typ;
  init : expr option;
  is_const : bool;
  span : span;
}
[@@deriving show { with_path = false }]

type type_alias_def = { name : string; typ : typ; span : span }
[@@deriving show { with_path = false }]

type decl =
  | Func of func_def
  | Struct of struct_def
  | Extern of func_def
  | Global of global_def
  | TypeAlias of type_alias_def
  | Newtype of type_alias_def
[@@deriving show { with_path = false }]
