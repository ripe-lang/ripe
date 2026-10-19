(* SPDX-License-Identifier: Apache-2.0 *)

type span = Span.t

let dummy_span = Span.dummy

(* x, point, Color *)
type name = Interner.id

(* The show deriver looks these printers up by name *)
let pp_span = Span.pp
let pp_name = Interner.pp

(* A generic wrapper saves writing a span field into every small record *)
type 'a spanned = { value : 'a; span : span }
[@@deriving show { with_path = false }]

let spanned value span = { value; span }

(* The Point of Point :: struct { x: i32 } *)
type ident = name spanned [@@deriving show { with_path = false }]
type loop_label = name spanned [@@deriving show { with_path = false }]

(* A while is a loop too but only a plain loop can break with a value *)
type loop_source = PlainLoop | WhileLoop | ForLoop
[@@deriving show { with_path = false }]

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

(* The = of x := 5 and the second : of N :: 5 *)
type binder = RunTime | CompileTime [@@deriving show { with_path = false }]

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
  (* TODO(f1ac): every enum is an i32 and flags don't exist *)
  (* TODO(d737): a variant carries no explicit value *)
  | EnumType of variant list
  | PointerType of expr
  | FuncType of abi * expr list * expr
  | ArrayType of expr * expr
  | SliceType of expr
  | Hole
  | Extern of string spanned

and expr = { edesc : expr_desc; espan : span }
and block = expr list

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
  | Named of name
  | Pointer of typ
  | FuncPtr of abi * typ list * typ
  | Array of expr * typ
  | Slice of typ
  | UnitType

(* The prefix keeps these labels apart from the expr ones in this group *)
and typ = { tdesc : typ_desc; tspan : span }

(* The "C" of extern "C" fn exit(code: i32) never *)
and abi = NoAbi | NamedAbi of string spanned
and field = { field_name : ident; field_typ : typ }

(* Point :: struct { x: i32, y: i32 } *)
and struct_def = {
  struct_name : ident;
  fields : field list;
  struct_span : span;
}

(* x : i32 = 5, x := 5, x : i32 *)
and bind_form = Typed of typ * expr | Inferred of expr | Declared of typ

(* TODO(f1ac): every enum is an i32 and flags don't exist *)
(* TODO(d737): a variant carries no explicit value and no payload *)
and enum_def = { enum_name : ident; variants : ident list; enum_span : span }
and param = { param_name : ident; param_typ : typ; param_span : span }

(* fn(a: i32) -> i32 => a, fn { } *)
and lambda = {
  lambda_params : param list;
  lambda_ret : typ option;
  lambda_body : block;
  lambda_abi : abi;
  lambda_span : span;
}
[@@deriving show { with_path = false }]

(* main :: fn { }, count := 0, puts : (CStr) -> i32 : extern "C" *)
type decl = Bind of binding [@@deriving show { with_path = false }]

let path_expr p =
  let first = (Nonempty.hd p.owner).span in
  let last = p.member.span in
  { edesc = Path p; espan = Span.make (Span.lo first) (Span.hi last) }

let owner_expr p =
  match Nonempty.destruct_last p.owner with
  | [], { value; span } -> { edesc = Ident value; espan = span }
  | first :: rest, last ->
      path_expr { owner = Nonempty.make first rest; member = last }

let bind_value b =
  match b.bind_form with
  | Typed (_, e) | Inferred e -> Some e
  | Declared _ -> None

let path_names p = List.map (fun n -> n.value) (Nonempty.to_list p.owner)
let path_split p = (path_names p, p.member.value)

type global_def = {
  name : ident;
  typ : typ option;
  init : expr option;
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
