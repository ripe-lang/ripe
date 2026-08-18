(* SPDX-License-Identifier: Apache-2.0 *)

open Types

type texpr_desc =
  | TErrorExpr
  | TInt of int64
  | TFloat of float
  | TBool of bool
  | TNull
  | TCStr of string
  | TStr of string
  | TChar of int
  | TIdent of Symbol.t
  | TCall of texpr * texpr list * int option
  | TBinOp of Ast.binop * texpr * texpr
  | TAssign of Ast.binop option * texpr * texpr
  | TUnOp of Ast.unop * texpr
  | TFieldAccess of texpr * int
  (* Target type is the node type *)
  | TCast of texpr
  | TSizeOf of Types.ty
  | TRange of texpr * texpr
  | TRangeInclusive of texpr * texpr
  | TArrayLit of texpr list
  | TIndex of texpr * texpr
  | TLen of texpr
  | TSliceExpr of texpr * texpr * texpr
  | TDataPtr of texpr
  | TZero
  | TUndef
  | TStructLit of Qname.t * (int * texpr) list
  (* TODO: a payload variant carries its arguments here too *)
  | TVariant of Qname.t * int64
  | TBlock of tblock
  | TIf of (texpr * tblock) list * tblock option
  | TWhile of Ast.loop_label option * texpr * tblock
  | TFor of Ast.loop_label option * Symbol.t * ty * texpr * tblock
  | TBinding of Ast.binding_kind * Symbol.t * ty * texpr
  | TReturn of texpr option
  | TBreak of Ast.loop_label option * texpr option
  | TContinue of Ast.loop_label option
  | TPairAssign of texpr * texpr * texpr * texpr
  | TLocalDecl
  | TLoop of Ast.loop_label option * tblock
  | TMatch of texpr * tarm list
  | TUnit
and texpr = {
  desc : texpr_desc;
  ty : ty;
  span : Ast.span;
  const : Constant.value option; [@opaque]
}
[@@deriving show { with_path = false }]

and tblock = texpr list [@@deriving show { with_path = false }]

and tarm = { tpat : tpattern; tbody : tblock }
[@@deriving show { with_path = false }]

(* A test compares part of the scrutinee and a binding names part of it *)
and tpattern = TPatWild | TPatBind of Symbol.t * ty | TPatConst of int64
[@@deriving show { with_path = false }]

let mk ?(span = Ast.dummy_span) (ty : ty) (desc : texpr_desc) : texpr =
  { desc; ty; span; const = None }

type tfunc_def = {
  key : Symbol.key;
  name : string;
  source_name : string;
  entry_point : bool;
  params : (Symbol.t * ty) list;
  ret_ty : ty;
  body : tblock;
  modifiers : Ast.modifier list;
  variadic : bool;
}
[@@deriving show { with_path = false }]

type tglobal_def = {
  key : Symbol.key;
  name : string;
  ty : ty;
  init : texpr option;
  kind : Ast.binding_kind;
  modifiers : Ast.modifier list;
}
[@@deriving show { with_path = false }]

type tdecl =
  | TFunc of tfunc_def
  | TStruct of Qname.t * ty list * Ast.modifier list
    (* This has a name typed fields and modifiers *)
  | TLocalStruct of Qname.t * ty list
  | TExtern of tfunc_def
  | TGlobal of tglobal_def
  | TTypeAlias of Qname.t * ty
  (* An enum is an integer at runtime so nothing past here needs its variants *)
  | TEnum of Qname.t
[@@deriving show { with_path = false }]
