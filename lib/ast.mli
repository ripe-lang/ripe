(* SPDX-License-Identifier: Apache-2.0 *)

type span = Span.t
type name = Interner.id
type 'a spanned = { value : 'a; span : span }
type loop_label = name spanned
type ident = name spanned

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

type unop = Neg | Not | BitNot | Deref | AddressOf

type expr_desc =
  | Int of int64 * string option
  | Float of float * string option
  | Bool of bool
  | Null
  | Char of int
  | String of string
  | Ident of name
  | Call of expr * expr list
  | BinOp of binop spanned * expr * expr
  | Assign of binop option spanned * expr * expr
  | UnOp of unop * expr
  | Range of expr * expr
  | RangeInclusive of expr * expr
  | RangeFrom of expr
  | RangeTo of expr
  | RangeToInclusive of expr
  | RangeFull
  | Path of path
  | FieldAccess of expr * name spanned
  | Cast of typ * expr
  | SizeOf of typ
  | ArrayLit of expr list
  | Index of expr * expr
  | StructLit of name spanned * (name option spanned * expr) list
  | Block of block
  | If of (expr * block spanned) list * block spanned option
  | While of loop_label option * expr * block
  | For of loop_label option * name spanned * expr * block
  | Binding of name spanned * typ option * expr option
  | Return of expr option
  | Break of loop_label option * expr option
  | Continue of loop_label option
  | Loop of loop_label option * block
  | Match of expr * arm list
  | Unit

and expr = { desc : expr_desc; span : span }
and path = { owner : name spanned Nonempty.t; member : name spanned }
and arm = { pat : pattern; arm_body : block spanned; arm_span : span }
and pattern = { pdesc : pattern_desc; pspan : span }
and pattern_desc = PatValue of expr | PatWild | PatBind of name
and block = block_item list
and block_item = Expr of expr | Decl of local_decl

and local_decl =
  | LocalStruct of struct_def
  | LocalTypeAlias of type_alias_def
  | LocalFunc of func_def
  | LocalEnum of enum_def

and typ_desc =
  | Named of name
  | Pointer of typ
  | FuncPtr of abi * typ list * typ option
  | Array of expr * typ
  | Slice of typ
  | UnitType

and typ = { tdesc : typ_desc; tspan : span }
and abi = NoAbi | NamedAbi of string spanned
and field = { field_name : ident; field_typ : typ }

and struct_def = {
  struct_name : ident;
  fields : field list;
  struct_span : span;
}

and type_alias_def = { alias_name : ident; alias_typ : typ; alias_span : span }
and enum_def = { enum_name : ident; variants : ident list; enum_span : span }
and param = { param_name : ident; param_typ : typ; param_span : span }

and func_def = {
  func_name : ident;
  params : param list;
  ret : typ option;
  body : block;
  variadic : bool;
  extern_abi : abi;
  func_span : span;
}

type global_def = {
  name : ident;
  typ : typ option;
  init : expr option;
  span : span;
}

type decl =
  | Func of func_def
  | Struct of struct_def
  | Extern of func_def
  | Global of global_def
  | TypeAlias of type_alias_def
  | Enum of enum_def

val pp_span : Format.formatter -> span -> unit
val pp_loop_label : Format.formatter -> loop_label -> unit
val pp_binop : Format.formatter -> binop -> unit
val pp_unop : Format.formatter -> unop -> unit
val show_decl : decl -> string
val dummy_span : span
val spanned : 'a -> span -> 'a spanned
val show_binop_sym : binop -> string
val show_unop_sym : unop -> string
val path_expr : path -> expr
val owner_expr : path -> expr
val path_split : path -> name list * name
val decl_of_local : local_decl -> decl
