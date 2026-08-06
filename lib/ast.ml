(* SPDX-License-Identifier: GPL-2.0-only *)

type span = Span.t = { file : Span.file_id; lo : int; hi : int }

let pp_span = Span.pp
let show_span = Span.show
let dummy_span = Span.dummy

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
  | FieldAccess of expr * string
  | Cast of expr * typ * cast_kind
  | SizeOf of typ
  | ArrayLit of expr list
  | Index of expr * expr
  | Undefined
  | StructLit of string list * string * span * (string * span * expr) list
  | Block of block
  | If of (expr * block) list * block option
  | While of expr * block
  | For of string * span * expr * block
  | Binding of binding_kind * string * span * typ option * expr option
  | Return of expr option
  | Break
  | Continue
  | PairAssign of expr * expr * expr * expr
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

and type_alias_def = {
  alias_name : string;
  alias_typ : typ;
  alias_modifiers : modifier list;
  alias_span : span;
}
[@@deriving show { with_path = false }]

and local_decl =
  | LocalTypeAlias of type_alias_def
  | LocalNewtype of type_alias_def
[@@deriving show { with_path = false }]

let show_named (path : string list) (name : string) : string =
  String.concat "." (path @ [ name ])

type param = { name : string; typ : typ; span : span }
[@@deriving show { with_path = false }]

type field = {
  name : string;
  typ : typ;
  modifiers : modifier list;
  span : span;
}
[@@deriving show { with_path = false }]

type func_def = {
  name : string;
  params : param list;
  ret : typ option;
  body : block;
  modifiers : modifier list;
  variadic : bool;
  span : span;
}
[@@deriving show { with_path = false }]

type struct_def = {
  name : string;
  fields : field list;
  modifiers : modifier list;
  span : span;
}
[@@deriving show { with_path = false }]

type global_def = {
  name : string;
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
  | LocalTypeAlias td -> TypeAlias td
  | LocalNewtype td -> Newtype td

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
