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

type unop = Neg | Not | BitNot | PreInc | PreDec

type expr =
  | Int of int
  | Ident of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr

type stmt =
  | Let of string * expr
  | Var of string * expr
  | Return of expr
  | Expr of expr

type param = { name : string }
type func_def = { name : string; params : param list; body : stmt list }
type decl = Func of func_def
