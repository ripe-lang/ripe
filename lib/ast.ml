(* SPDX-License-Identifier: GPL-2.0-only *)

type span = Span.t = { lo : int; hi : int }

let pp_span = Span.pp
let show_span = Span.show
let dummy_span = Span.dummy

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
  | ModAssign
  | BitAndAssign
  | BitOrAssign
  | BitXorAssign
  | LshiftAssign
  | RshiftAssign
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
  | ModAssign -> "%="
  | BitAndAssign -> "&="
  | BitOrAssign -> "|="
  | BitXorAssign -> "^="
  | LshiftAssign -> "<<="
  | RshiftAssign -> ">>="

type unop = Neg | Not | BitNot | Deref | AddressOf
[@@deriving show { with_path = false }]

let show_unop_sym = function
  | Neg -> "-"
  | Not -> "!"
  | BitNot -> "~"
  | Deref -> "*"
  | AddressOf -> "&"

type binding_kind = Var | Let | Const [@@deriving show { with_path = false }]

type expr_desc =
  | Int of int64 * string option
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
  | Cast of expr * typ * bool
  | SizeOf of typ
  | ArrayLit of expr list
  | Index of expr * expr
  | Undefined
  | StructLit of string * span * (string * span * expr) list
  | BlockExpr of stmt list * expr
[@@deriving show { with_path = false }]

and expr = { desc : expr_desc; span : span }
[@@deriving show { with_path = false }]

and typ_desc =
  | Named of string
  | Pointer of typ
  | FuncPtr of typ list * typ option
  | Array of expr * typ
  | Slice of typ
[@@deriving show { with_path = false }]

and typ = { tdesc : typ_desc; span : span }
[@@deriving show { with_path = false }]

(* TODO(68e6): Support tuple destructuring in var bindings e.g. var (a, b) = (x, y) *)
and stmt_desc =
  | Binding of binding_kind * string * span * typ option * expr option
  | Return of expr option
  | If of (expr * stmt list) list * stmt list
  | While of expr * stmt list
  | For of string * span * expr * stmt list
  | Break
  | Continue
  | Expr of expr
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
  kind : binding_kind;
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
