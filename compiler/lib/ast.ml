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

type unop = Neg | Not | BitNot | PreInc | PreDec | PostInc | PostDec | Deref | AddressOf
[@@deriving show]

type expr =
  | Int of int
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
[@@deriving show]

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
[@@deriving show]

(* kept separate for distinction *)
type param = { name : string; typ : typ } [@@deriving show]
type field = { name : string; typ : typ } [@@deriving show]

(* TODO: add array types *)
type func_def = {
  name : string;
  params : param list;
  ret : typ option;
  body : stmt list;
}
[@@deriving show]

type struct_def = { name : string; fields : field list } [@@deriving show]
type decl = Func of func_def | Struct of struct_def [@@deriving show]

let decl_to_string = show_decl
