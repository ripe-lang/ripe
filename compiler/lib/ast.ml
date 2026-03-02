type typ = Named of string [@@deriving show]

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
  | Let of string * typ option * expr
  | Var of string * typ option * expr
  | Return of expr
  | If of (expr * stmt list) list * stmt list
  | While of expr * stmt list
  | For of string * expr * stmt list
  | CFor of stmt * expr * expr * stmt list
  | Break
  | Continue
  | Expr of expr
[@@deriving show]

type param = { name : string; typ : typ } [@@deriving show]

(* TODO: add pointer and array types *)
type func_def = { name : string; params : param list; ret : typ option; body : stmt list }
[@@deriving show]

type decl = Func of func_def [@@deriving show]

let decl_to_string = show_decl
