open Types

type local_id = int
type block_id = int
type storage_kind = Param | User | Temp
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

type place = {
  base : place_base;
  projections : projection list;
  place_span : Ast.span;
}

and projection = Deref | Field of int | Index of operand
and operand = { desc : operand_desc; ty : ty; span : Ast.span }
and operand_desc = Copy of place | Const of constant

type value = { desc : value_desc; ty : ty; span : Ast.span }

and value_desc =
  | Use of operand
  | Unary of Ast.unop * operand
  | Binary of Ast.binop * operand * operand
  | Cast of operand * Ast.cast_kind
  | AddressOf of place
  | Len of place
  | DataPtr of place
  | ToSlice of place
  | Slice of place * operand * operand
  | SizeOf of ty

type call_kind = Internal | External
type callee = Direct of string | Indirect of operand

type call = {
  destination : place option;
  callee : callee;
  kind : call_kind;
  args : operand list;
  return_ty : ty;
  variadic_start : int option;
}

type check =
  | Bounds of operand * operand
  | SliceBounds of operand * operand * operand
  | Null of operand
  | DivZero of operand
  | NegativeShift of operand
  | CastRange of operand * ty

type statement = { desc : statement_desc; span : Ast.span }
and statement_desc = Assign of place * value | Call of call | Check of check

type terminator = { desc : terminator_desc; span : Ast.span }

and terminator_desc =
  | Jump of block_id
  | Branch of operand * block_id * block_id
  | ReturnValue of operand option
  | Unreachable

type local = {
  name : string option;
  ty : ty;
  storage : storage_kind;
  span : Ast.span;
}

type block = { statements : statement list; terminator : terminator option }

type func = {
  name : string;
  params : local_id list;
  locals : local array;
  blocks : block array;
  return_ty : ty;
  entry_point : bool;
  span : Ast.span;
}

type struct_decl = { name : Qname.t; fields : ty list }

type global_value =
  | GlobalConst of constant * ty
  | GlobalAddress of string
  | GlobalArray of global_value list
  | GlobalStruct of (int * global_value) list

type global = { name : string; ty : ty; init : global_value option }

type program = {
  structs : struct_decl list;
  globals : global list;
  functions : func list;
}
