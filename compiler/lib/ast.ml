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

type unop = Neg | Not | BitNot | PreInc | PreDec | PostInc | PostDec
[@@deriving show]

type expr =
  | Int of int
  | Bool of bool
  | Ident of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Range of expr * expr
[@@deriving show]

type stmt =
  | Let of string * expr
  | Var of string * expr
  | Return of expr
  | If of (expr * stmt list) list * stmt list
  | While of expr * stmt list
  | For of string * expr * stmt list
  | CFor of stmt * expr * expr * stmt list
  | Expr of expr
[@@deriving show]

type param = { name : string } [@@deriving show]

type func_def = { name : string; params : param list; body : stmt list }
[@@deriving show]

type decl = Func of func_def [@@deriving show]

let decl_to_string = show_decl
