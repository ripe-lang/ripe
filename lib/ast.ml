(* SPDX-License-Identifier: GPL-2.0-only *)

(* TODO(825c): Add file id for multi file comp later *)
(* I was going to wait before doing byte offset but the dump-ast was way too long. *)
type span = { lo : int; hi : int }

(* spans elided from dumps so derived show stays readable *)
let pp_span fmt (_ : span) = Format.pp_print_string fmt "_"
let show_span (_ : span) = "_"

let dummy_span = { lo = 0; hi = 0 }

type typ_desc =
  | Named of string
  | Pointer of typ
  | FuncPtr of typ list * typ option
  (* TODO(f0b2): allow const-expr sizes, not just an int literal *)
  | Array of int * typ
  | Slice of typ

and typ = { tdesc : typ_desc; span : span } [@@deriving show]

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
[@@deriving show]

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

type unop = Neg | Not | BitNot | Deref | AddressOf [@@deriving show]

type interp_part = Lit of string | Interp of expr [@@deriving show]

and expr_desc =
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | Char of char
  | String of string
  | Ident of string
  (* TODO(e8ba): change to Call of expr * expr list for function pointers and method calls *)
  | Call of string * expr list
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Range of expr * expr
  | RangeInclusive of expr * expr
  | FieldAccess of expr * string
  | Cast of expr * typ
  | SizeOf of typ
  | InterpString of interp_part list
  | ArrayLit of expr list
  | Index of expr * expr
  | Undefined
[@@deriving show]

and expr = { desc : expr_desc; span : span } [@@deriving show]

(* TODO(68e6): Support tuple destructuring in let/var bindings e.g. let (a, b) = (x, y) *)
type stmt_desc =
  | Const of string * typ option * expr
  | Var of string * typ option * expr option
  | Return of expr option
  | If of (expr * stmt list) list * stmt list
  | While of expr * stmt list
  | For of string * expr * stmt list
  | Break
  | Continue
  | Expr of expr
  | Block of stmt list
[@@deriving show]

and stmt = { sdesc : stmt_desc; span : span } [@@deriving show]

type modifier = Pub | Inline [@@deriving show]

(* kept separate for distinction *)
type param = { name : string; typ : typ; span : span } [@@deriving show]

type field = {
  name : string;
  typ : typ;
  modifiers : modifier list;
  span : span;
}
[@@deriving show]

type func_def = {
  name : string;
  params : param list;
  ret : typ option;
  body : stmt list;
  modifiers : modifier list;
  variadic : bool;
  span : span;
}
[@@deriving show]

type struct_def = {
  name : string;
  fields : field list;
  modifiers : modifier list;
  span : span;
}
[@@deriving show]

type global_def = {
  name : string;
  typ : typ;
  init : expr option;
  is_const : bool;
  span : span;
}
[@@deriving show]

type type_alias_def = { name : string; typ : typ; span : span }
[@@deriving show]

type decl =
  | Func of func_def
  | Struct of struct_def
  | Extern of func_def
  | Global of global_def
  | TypeAlias of type_alias_def
[@@deriving show]

let decl_to_string = show_decl
