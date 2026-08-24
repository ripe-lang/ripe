(* SPDX-License-Identifier: Apache-2.0 *)

type local_id = int
type block_id = int
type storage_kind = Param | User | Temp | Result
type place_base = Local of local_id | Global of string

type constant =
  | Int of int64
  | Float of float
  | Bool of bool
  | Null
  | CStr of string
  | Char of int
  | Zero
  | Undef
  | Function of string
  | Str of string

type place = {
  base : place_base;
  projections : projection list;
  place_span : Ast.span;
}

and projection = Deref | Field of int | Index of operand
and operand = { desc : operand_desc; ty : Types.ty; span : Ast.span }
and operand_desc = Copy of place | Const of constant

type unop = Neg | Not | BitNot

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
  | BitAnd
  | BitOr
  | BitXor
  | Lshift
  | Rshift

type value = { desc : value_desc; ty : Types.ty }

and value_desc =
  | Use of operand
  | Unary of unop * operand
  | Binary of binop * operand * operand
  | Cast of operand
  | AddressOf of place
  | Len of place
  | DataPtr of place
  | SizeOf of Types.ty

type call_kind = Internal | External
type callee = Direct of string | Indirect of operand

type call = {
  destination : place option;
  callee : callee;
  kind : call_kind;
  args : operand list;
  return_ty : Types.ty;
  variadic_start : int option;
}

type check =
  | Bounds of operand * operand
  | SliceBounds of operand * operand * operand
  | Null of operand
  | DivZero of operand
  | NegativeShift of operand

type statement = { desc : statement_desc; span : Ast.span }

and statement_desc =
  | Assign of place * value
  | Call of call
  | Slice of place * place * operand * operand

type terminator = { desc : terminator_desc; span : Ast.span }

and terminator_desc =
  | Jump of block_id
  | Branch of operand * block_id * block_id
  | Assert of check * block_id * block_id
  | Panic of check
  | ReturnValue of operand option
  | Unreachable

type local = {
  name : string option;
  ty : Types.ty;
  storage : storage_kind;
  span : Ast.span;
}

type block = { statements : statement list; terminator : terminator option }

type func = {
  name : string;
  source_name : string;
  public : bool;
  params : local_id list;
  result : local_id option;
  locals : local array;
  blocks : block array;
  return_ty : Types.ty;
  entry_point : bool;
  span : Ast.span;
}

type struct_decl = { name : Qname.t; fields : Types.ty list; local : bool }

type global_value =
  | GlobalConst of constant * Types.ty
  | GlobalAddress of string
  | GlobalArray of global_value list
  | GlobalStruct of (int * global_value) list

type global = {
  name : string;
  ty : Types.ty;
  init : global_value option;
  public : bool;
}

type program = {
  structs : struct_decl list;
  globals : global list;
  functions : func list;
}

type error = { function_name : string; error_span : Ast.span; message : string }

val build : Tast.tdecl list -> program
val dump : program -> string
val show_error : error -> string
val verify : program -> (unit, error list) result
