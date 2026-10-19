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
  place_base : place_base;
  place_projs : projection list;
  place_span : Ast.span;
}

and projection = Deref | Field of int | Index of operand
and operand = { op_desc : operand_desc; op_ty : Types.ty; op_span : Ast.span }
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

type value = { val_desc : value_desc; val_ty : Types.ty }

and value_desc =
  | Use of operand
  | UnOp of unop * operand
  | BinOp of binop * operand * operand
  | Cast of operand
  | AddressOf of place
  | Len of place
  | DataPtr of place
  | SizeOf of Types.ty

type call_kind = Internal | External
type callee = Direct of string | Indirect of operand

type call = {
  call_dest : place option;
  call_callee : callee;
  call_kind : call_kind;
  call_args : operand list;
  call_ret : Types.ty;
  call_variadic_start : int option;
}

type check =
  | Bounds of operand * operand
  | SliceBounds of operand * operand * operand
  | Null of operand
  | DivZero of operand
  | NegativeShift of operand

type stmt = { stmt_desc : stmt_desc; stmt_span : Ast.span }

and stmt_desc =
  | Assign of place * value
  | Call of call
  | Slice of place * place * operand * operand

type terminator = { term_desc : terminator_desc; term_span : Ast.span }

and terminator_desc =
  | Jump of block_id
  | Branch of operand * block_id * block_id
  | Check of check * block_id
  | ReturnValue of operand option
  | Unreachable

type local = {
  local_name : string option;
  local_ty : Types.ty;
  local_storage : storage_kind;
  local_span : Ast.span;
}

type block = { block_stmts : stmt list; block_term : terminator option }

type func = {
  func_name : string;
  func_source_name : string;
  func_export : bool;
  func_abi : Types.func_abi;
  func_params : local_id list;
  func_result : local_id option;
  func_locals : local array;
  func_blocks : block array;
  func_ret : Types.ty;
  func_entry_point : bool;
  func_span : Ast.span;
}

type struct_decl = {
  struct_name : Keyname.t;
  struct_fields : Types.ty list;
  struct_local : bool;
}

type global_value =
  | GlobalConst of constant * Types.ty
  | GlobalAddress of string
  | GlobalArray of global_value list
  | GlobalStruct of (int * global_value) list

type global = {
  global_name : string;
  global_ty : Types.ty;
  global_init : global_value option;
}

type program = {
  prog_structs : struct_decl list;
  prog_globals : global list;
  prog_funcs : func list;
}

val build : Tast.tdecl list -> program
val dump : program -> string
val verify : program -> unit
