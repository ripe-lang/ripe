(* SPDX-License-Identifier: GPL-2.0-only *)

type span = Span.t = { file : Span.file_id; lo : int; hi : int }

let pp_span = Span.pp
let dummy_span = Span.dummy

type 'a spanned = { value : 'a; span : span }
[@@deriving show { with_path = false }]

let spanned value span = { value; span }

type loop_label = string spanned [@@deriving show { with_path = false }]

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
  | ModAssign
  | BitAndAssign
  | BitOrAssign
  | BitXorAssign
  | LshiftAssign
  | RshiftAssign
[@@deriving show { with_path = false }]

let show_binop_sym = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | And -> "&&"
  | Or -> "||"
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | Lshift -> "<<"
  | Rshift -> ">>"
  | Assign -> "="
  | AddAssign -> "+="
  | SubAssign -> "-="
  | MulAssign -> "*="
  | DivAssign -> "/="
  | ModAssign -> "%="
  | BitAndAssign -> "&="
  | BitOrAssign -> "|="
  | BitXorAssign -> "^="
  | LshiftAssign -> "<<="
  | RshiftAssign -> ">>="

let is_assignment_op (op : binop) : bool =
  match op with
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | BitAndAssign | BitOrAssign | BitXorAssign | LshiftAssign | RshiftAssign ->
      true
  | _ -> false

type unop = Pos | Neg | Not | BitNot | Deref | AddressOf
[@@deriving show { with_path = false }]

let show_unop_sym = function
  | Pos -> "+"
  | Neg -> "-"
  | Not -> "!"
  | BitNot -> "~"
  | Deref -> "*"
  | AddressOf -> "&"

type binding_kind = Var | Let | Comptime
[@@deriving show { with_path = false }]

type modifier = Pub [@@deriving show { with_path = false }]
type cast_kind = Normal | Checked [@@deriving show { with_path = false }]

let show_cast_op = function Normal -> "as" | Checked -> "as!"

type expr_desc =
  | ErrorExpr
  | Int of int64 * string option
  | Float of float
  | Bool of bool
  | Null
  | Char of int
  | String of string
  | Ident of string
  | Call of expr * expr list
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | Range of expr * expr
  | RangeInclusive of expr * expr
  | FieldAccess of expr * string * span
  | Cast of expr * typ * cast_kind
  | SizeOf of typ
  | ArrayLit of expr list
  | Index of expr * expr
  | Undefined
  | StructLit of string list * string * span * (string * span * expr) list
  | Block of block
  | If of (expr * block spanned) list * block spanned option
  | While of loop_label option * expr * block
  | For of loop_label option * string * span * expr * block
  | Binding of binding_kind * string * span * typ option * expr option
  | Return of expr option
  | Break of loop_label option
  | Continue of loop_label option
  | PairAssign of expr * expr * expr * expr
  | Loop of loop_label option * block
[@@deriving show { with_path = false }]

and expr = { desc : expr_desc; span : span }
[@@deriving show { with_path = false }]

and block = block_item list [@@deriving show { with_path = false }]

and block_item = Expr of expr | Decl of local_decl
[@@deriving show { with_path = false }]

and typ_desc =
  | ErrorType
  | Named of string list * string
  | Pointer of typ
  | FuncPtr of typ list * typ option
  | Array of expr * typ
  | Slice of typ
[@@deriving show { with_path = false }]

and typ = { tdesc : typ_desc; tspan : span }
[@@deriving show { with_path = false }]

and field = { field_name : string; field_typ : typ; field_span : span }
[@@deriving show { with_path = false }]

and struct_def = {
  struct_name : string;
  struct_name_span : span;
  fields : field list;
  struct_modifiers : modifier list;
  struct_span : span;
}
[@@deriving show { with_path = false }]

and type_alias_def = {
  alias_name : string;
  alias_name_span : span;
  alias_typ : typ;
  alias_modifiers : modifier list;
  alias_span : span;
}
[@@deriving show { with_path = false }]

and param = { param_name : string; param_typ : typ; param_span : span }
[@@deriving show { with_path = false }]

and func_def = {
  func_name : string;
  func_name_span : span;
  params : param list;
  ret : typ option;
  body : block;
  func_modifiers : modifier list;
  variadic : bool;
  func_span : span;
}
[@@deriving show { with_path = false }]

and local_decl =
  | LocalStruct of struct_def
  | LocalTypeAlias of type_alias_def
  | LocalNewtype of type_alias_def
  | LocalFunc of func_def
[@@deriving show { with_path = false }]

let show_named (path : string list) (name : string) : string =
  String.concat "." (path @ [ name ])

type global_def = {
  name : string;
  name_span : span;
  typ : typ;
  init : expr option;
  kind : binding_kind;
  modifiers : modifier list;
  span : span;
}
[@@deriving show { with_path = false }]

type decl =
  | Func of func_def
  | Struct of struct_def
  | Extern of func_def
  | Global of global_def
  | TypeAlias of type_alias_def
  | Newtype of type_alias_def
[@@deriving show { with_path = false }]

(* A local declaration carries the same payload so it checks like a global one *)
let decl_of_local : local_decl -> decl = function
  | LocalStruct sd -> Struct sd
  | LocalTypeAlias td -> TypeAlias td
  | LocalNewtype td -> Newtype td
  | LocalFunc fd -> Func fd

type import = { path : string list; span : span }
[@@deriving show { with_path = false }]

type module_header = { name : string; span : span }
[@@deriving show { with_path = false }]

type module_ = {
  header : module_header option;
  imports : import list;
  decls : decl list;
}
[@@deriving show { with_path = false }]
