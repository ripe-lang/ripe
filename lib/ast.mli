(* SPDX-License-Identifier: Apache-2.0 *)

type span = Span.t
type name = Interner.id
type 'a spanned = { value : 'a; span : span }
type ident = name spanned
type loop_label = name spanned
type loop_source = PlainLoop | WhileLoop | ForLoop

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
type binder = RunTime | CompileTime

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
  | For of loop_label option * name spanned * expr * block
  | Binding of binding
  | Return of expr option
  | Break of loop_label option * expr option
  | Continue of loop_label option
  | Loop of loop_label option * loop_source * block
  | Match of expr * arm list
  | Unit
  | Lambda of lambda
  | StructType of field list
  | EnumType of variant list
  | PointerType of expr
  | FuncType of abi * expr list * expr
  | ArrayType of expr * expr
  | SliceType of expr
  | Hole
  | Extern of string spanned

and expr = { edesc : expr_desc; espan : span }
and block = expr list
and path = { owner : name spanned Nonempty.t; member : name spanned }
and arm = { pat : pattern; arm_body : block spanned; arm_span : span }
and pattern = { pdesc : pattern_desc; pspan : span }

and typ_desc =
  | Named of name
  | Pointer of typ
  | FuncPtr of abi * typ list * typ
  | Array of expr * typ
  | Slice of typ
  | UnitType

and typ = { tdesc : typ_desc; tspan : span }
and abi = NoAbi | NamedAbi of string spanned
and field = { field_name : ident; field_typ : typ }

and binding = {
  bind_name : ident;
  binder : binder;
  bind_form : bind_form;
  bind_span : span;
}

and bind_form = Typed of typ * expr | Inferred of expr | Declared of typ
and typ = expr

and lambda = {
  lambda_params : param list;
  lambda_ret : typ option;
  lambda_body : block;
  lambda_abi : abi;
  lambda_span : span;
}

type global_def = {
  name : ident;
  typ : typ option;
  init : expr option;
  span : span;
}

type decl = Bind of binding

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
val bind_value : binding -> expr option
