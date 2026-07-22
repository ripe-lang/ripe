(* SPDX-License-Identifier: GPL-2.0-only *)

open Types

type texpr_desc =
  | TInt of int64
  | TFloat of float
  | TBool of bool
  | TNull
  | TCStr of string
  | TChar of char
  | TIdent of Symbol.t
  | TCall of texpr * texpr list * int option
  | TBinOp of Ast.binop * texpr * texpr
  | TUnOp of Ast.unop * texpr
  | TFieldAccess of texpr * string
  (* target type is the node type *)
  | TCast of texpr * bool
  | TSizeOf of Types.ty
  | TRange of texpr * texpr
  | TRangeInclusive of texpr * texpr
  | TArrayLit of texpr list
  | TIndex of texpr * texpr
  | TLen of texpr
  | TToSlice of texpr
  | TSliceExpr of texpr * texpr * texpr
  | TDataPtr of texpr
  | TZero
  | TUndef
  | TStructLit of string * (string * texpr) list
  | TBlockExpr of tstmt list * texpr
  | TIfExpr of (texpr * texpr) list * texpr

and texpr = { desc : texpr_desc; ty : ty; span : Ast.span }
[@@deriving show { with_path = false }]

and tstmt_desc =
  | TBinding of Ast.binding_kind * Symbol.t * ty * texpr
  | TReturn of texpr option
  | TIf of (texpr * tstmt list) list * tstmt list
  | TWhile of texpr * tstmt list
  | TFor of Symbol.t * ty * texpr * tstmt list
  | TBreak
  | TContinue
  | TExpr of texpr
  | TBlock of tstmt list

and tstmt = { tsdesc : tstmt_desc; span : Ast.span }
[@@deriving show { with_path = false }]

let mk ?(span = Ast.dummy_span) (ty : ty) (desc : texpr_desc) : texpr =
  { desc; ty; span }

type tfunc_def = {
  name : string;
  params : (Symbol.t * ty) list;
  ret_ty : ty;
  body : tstmt list;
  modifiers : Ast.modifier list;
  variadic : bool;
}
[@@deriving show { with_path = false }]

type tglobal_def = {
  name : string;
  ty : ty;
  init : texpr option;
  kind : Ast.binding_kind;
}
[@@deriving show { with_path = false }]

type tdecl =
  | TFunc of tfunc_def
  | TStruct of string * (string * ty) list * Ast.modifier list
    (* name, typed fields, modifiers *)
  | TExtern of tfunc_def
  | TGlobal of tglobal_def
  | TTypeAlias of string * ty
  | TNewtype of string * ty
[@@deriving show { with_path = false }]
