(* SPDX-License-Identifier: GPL-2.0-only *)

open Types

type tinterp_part = TLit of string | TInterp of texpr

and texpr_desc =
  | TInt of int
  | TFloat of float
  | TBool of bool
  | TNull
  | TCStr of string
  | TChar of char
  | TIdent of string
  | TCall of string * texpr list
  | TBinOp of Ast.binop * texpr * texpr
  | TUnOp of Ast.unop * texpr
  | TFieldAccess of texpr * string
  (* target type is the node type *)
  | TCast of texpr
  | TSizeOf of Types.ty
  | TRange of texpr * texpr
  | TRangeInclusive of texpr * texpr
  | TInterpString of tinterp_part list
  | TArrayLit of texpr list
  | TIndex of texpr * texpr
  | TLen of texpr
  | TToSlice of texpr
  | TSliceExpr of texpr * texpr * texpr
  | TDataPtr of texpr
  | TZero
  | TUndef
  | TStructLit of string * (string * texpr) list

and texpr = { desc : texpr_desc; ty : ty }
[@@deriving show { with_path = false }]

let mk (ty : ty) (desc : texpr_desc) : texpr = { desc; ty }

type tstmt =
  | TConst of string * ty * texpr
  | TVar of string * ty * texpr
  | TReturn of texpr option
  | TIf of (texpr * tstmt list) list * tstmt list
  | TWhile of texpr * tstmt list
  | TFor of string * ty * texpr * tstmt list
  | TBreak
  | TContinue
  | TExpr of texpr
  | TBlock of tstmt list
[@@deriving show { with_path = false }]

type tfunc_def = {
  name : string;
  params : (string * ty) list;
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
  is_const : bool;
}
[@@deriving show { with_path = false }]

type tdecl =
  | TFunc of tfunc_def
  | TStruct of string * (string * ty) list * Ast.modifier list
    (* name, typed fields, modifiers *)
  | TExtern of tfunc_def
  | TGlobal of tglobal_def
  | TTypeAlias of string * ty
[@@deriving show { with_path = false }]
