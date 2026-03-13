(* SPDX-License-Identifier: GPL-2.0-only *)

type typ = Named of string | Pointer of typ [@@deriving show]

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

type unop =
  | Neg
  | Not
  | BitNot
  | PreInc
  | PreDec
  | PostInc
  | PostDec
  | Deref
  | AddressOf
[@@deriving show]

type interp_part = Lit of string | Interp of expr [@@deriving show]

and expr =
  | Int of int
  | Float of float
  | Bool of bool
  | Null
  | Char of char
  | String of string
  | Ident of string
  | Call of string * expr list
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Range of expr * expr
  | FieldAccess of expr * string
  | Cast of expr * typ
  | SizeOf of typ
  | InterpString of interp_part list
[@@deriving show]

(* TODO(68e6): Support tuple destructuring in let/var bindings e.g. let (a, b) = (x, y) *)
type stmt =
  | Let of string * typ option * expr
  | Var of string * typ option * expr
  | Return of expr option
  | If of (expr * stmt list) list * stmt list
  | While of expr * stmt list
  | For of string * expr * stmt list
  | CFor of stmt * expr * expr * stmt list
  | Break
  | Continue
  | Expr of expr
  | Block of stmt list
[@@deriving show]

type modifier = Pub | Inline [@@deriving show]

(* kept separate for distinction *)
type param = { name : string; typ : typ } [@@deriving show]

type field = { name : string; typ : typ; modifiers : modifier list }
[@@deriving show]

(* TODO(ea0e): add array types *)
type func_def = {
  name : string;
  params : param list;
  ret : typ option;
  body : stmt list;
  modifiers : modifier list;
}
[@@deriving show]

type struct_def = {
  name : string;
  fields : field list;
  modifiers : modifier list;
}
[@@deriving show]

type decl = Func of func_def | Struct of struct_def | Extern of func_def
[@@deriving show]

let decl_to_string = show_decl
