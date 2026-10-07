(* SPDX-License-Identifier: Apache-2.0 *)

type span = Span.t

let dummy_span = Span.dummy

(* x, point, Color *)
type name = Interner.id

let show_names names = String.concat "." (List.map Interner.text names)
let show_named path name = show_names (path @ [ name ])

(* The show deriver looks these printers up by name *)
let pp_span = Span.pp
let pp_name = Interner.pp

(* A generic wrapper saves writing a span field into every small record *)
type 'a spanned = { value : 'a; span : span }
[@@deriving show { with_path = false }]

let spanned value span = { value; span }

type loop_label = name spanned [@@deriving show { with_path = false }]

(* The point of struct point { x: i32 } *)
type ident = name option spanned [@@deriving show { with_path = false }]

let ident name span = spanned (Some name) span
let missing_ident span = spanned None span

(* A missing name gets empty text so later phases can't refer to it *)
let ident_text i =
  match i.value with Some name -> Interner.text name | None -> ""

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
[@@deriving show { with_path = false }]

(* The derived show prints Add but messages want the + the user wrote *)
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

type unop = Neg | Not | BitNot | Deref | AddressOf
[@@deriving show { with_path = false }]

let show_unop_sym = function
  | Neg -> "-"
  | Not -> "!"
  | BitNot -> "~"
  | Deref -> "*"
  | AddressOf -> "&"

type modifier = Pub [@@deriving show { with_path = false }]

type expr_desc =
  | ErrorExpr
  | Int of int64 * string option
  | Float of float * string option
  | Bool of bool
  | Null
  | Char of int
  | String of string
  | Ident of name
  | Call of expr * expr list
  | BinOp of binop * expr * expr
  | Assign of binop option * expr * expr
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
  | StructLit of name list * name spanned * (ident * expr) list
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

(* math.Vec, value.field *)
and path = { owner : name spanned Nonempty.t; member : name spanned }
and arm = { pat : pattern; arm_body : block spanned; arm_span : span }
and pattern = { pdesc : pattern_desc; pspan : span }

(* TODO(7ea0): no ranges, alternatives or destructuring *)
and pattern_desc = PatValue of expr | PatWild | PatBind of name
and block = block_item list
and block_item = Expr of expr | Decl of local_decl

and local_decl =
  | LocalStruct of struct_def
  | LocalTypeAlias of type_alias_def
  | LocalFunc of func_def
  | LocalEnum of enum_def

and typ_desc =
  | ErrorType
  | Named of name list * name
  | Pointer of typ
  | FuncPtr of abi * typ list * typ option
  | Array of expr * typ
  | Slice of typ
  | UnitType

(* The prefix keeps these labels apart from the expr ones in this group *)
and typ = { tdesc : typ_desc; tspan : span }

(* The "C" of extern "C" fn exit(code: i32) never *)
and abi = NoAbi | NamedAbi of string spanned | AbiError
and field = { field_name : ident; field_typ : typ }

(* struct point { x: i32, y: i32 } *)
and struct_def = {
  struct_name : ident;
  fields : field list option;
  struct_modifiers : modifier list;
  struct_span : span;
}

and type_alias_def = {
  alias_name : ident;
  alias_typ : typ;
  alias_modifiers : modifier list;
  alias_span : span;
}

(* TODO(f1ac): every enum is an i32 and flags don't exist *)
(* TODO(d737): a variant carries no explicit value and no payload *)
and enum_def = {
  enum_name : ident;
  variants : ident list option;
  enum_modifiers : modifier list;
  enum_span : span;
}

and param = { param_name : ident; param_typ : typ; param_span : span }

and func_def = {
  func_name : ident;
  params : param list;
  ret : typ option;
  body : block;
  func_modifiers : modifier list;
  variadic : bool;
  extern_abi : abi;
  func_span : span;
}
[@@deriving show { with_path = false }]

let error_typ span = { tdesc = ErrorType; tspan = span }

let path_expr p =
  let first = (Nonempty.hd p.owner).span in
  let last = p.member.span in
  { desc = Path p; span = Span.make (Span.lo first) (Span.hi last) }

let owner_expr p =
  match Nonempty.destruct_last p.owner with
  | [], { value; span } -> { desc = Ident value; span }
  | first :: rest, last ->
      path_expr { owner = Nonempty.make first rest; member = last }

let path_names p = List.map (fun n -> n.value) (Nonempty.to_list p.owner)
let path_split p = (path_names p, p.member.value)

type global_def = {
  name : ident;
  typ : typ option;
  init : expr option;
  modifiers : modifier list;
  span : span;
}
[@@deriving show { with_path = false }]

(* fn add(a: i32, b: i32) i32 { ... }, extern "C" fn puts(s: cstr) i32 *)
type decl =
  | Func of func_def
  | Struct of struct_def
  | Extern of func_def
  | Global of global_def
  | TypeAlias of type_alias_def
  | Enum of enum_def
[@@deriving show { with_path = false }]

(* A local decl carries the same payload so it checks like a global one *)
let decl_of_local = function
  | LocalStruct sd -> Struct sd
  | LocalTypeAlias td -> TypeAlias td
  | LocalFunc fd -> Func fd
  | LocalEnum ed -> Enum ed

type import = { path : name list; span : span }
[@@deriving show { with_path = false }]

type module_header = { name : name; span : span }
[@@deriving show { with_path = false }]

type module_ = {
  header : module_header option;
  imports : import list;
  decls : decl list;
}
[@@deriving show { with_path = false }]
