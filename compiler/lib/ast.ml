type binop =
  | Add
  | Sub
  | Mul
  | Div

type expr =
  | Int   of int
  | Ident of string
  | BinOp of binop * expr * expr

type param = {
  name : string;
}

type func_def = {
  name   : string;
  params : param list;
  body   : expr;
}

type decl =
  | Func of func_def

