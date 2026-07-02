(* SPDX-License-Identifier: GPL-2.0-only *)

open Types

type tinterp_part = TLit of string | TInterp of texpr

and texpr =
  | TInt of int * ty
  | TFloat of float * ty
  | TBool of bool
  | TNull of ty
  | TCStr of string
  | TChar of char
  | TIdent of string * ty
  | TCall of string * texpr list * ty
  | TBinOp of Ast.binop * texpr * texpr * ty
  | TUnOp of Ast.unop * texpr * ty
  | TFieldAccess of texpr * string * ty
  | TCast of texpr * ty
  | TSizeOf of Types.ty
  | TRange of texpr * texpr
  | TRangeInclusive of texpr * texpr
  | TInterpString of tinterp_part list
  | TArrayLit of texpr list * ty
  | TIndex of texpr * texpr * ty
  | TLen of texpr
  | TToSlice of texpr * ty
  | TSliceExpr of texpr * texpr * texpr * ty
  | TDataPtr of texpr * ty
  | TZero of ty
  | TUndef of ty
[@@deriving show]

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
[@@deriving show]

type tfunc_def = {
  name : string;
  params : (string * ty) list;
  ret_ty : ty;
  body : tstmt list;
  modifiers : Ast.modifier list;
  variadic : bool;
}
[@@deriving show]

type tglobal_def = {
  name : string;
  ty : ty;
  init : texpr option;
  is_const : bool;
}
[@@deriving show]

type tdecl =
  | TFunc of tfunc_def
  | TStruct of string * (string * ty) list * Ast.modifier list
    (* name, typed fields, modifiers *)
  | TExtern of tfunc_def
  | TGlobal of tglobal_def
  | TTypeAlias of string * ty
[@@deriving show]

let ty_of_texpr (e : texpr) : ty =
  match e with
  | TInt (_, t) -> t
  | TFloat (_, t) -> t
  | TBool _ -> TBool
  | TNull t -> t
  | TCStr _ -> TPointer (TInt I8)
  | TChar _ -> TInt I32
  | TIdent (_, t) -> t
  | TCall (_, _, t) -> t
  | TBinOp (_, _, _, t) -> t
  | TUnOp (_, _, t) -> t
  | TFieldAccess (_, _, t) -> t
  | TCast (_, t) -> t
  | TSizeOf _ -> TInt I64
  | TRange _ | TRangeInclusive _ ->
      raise (Invalid_argument "TODO(cb82): range type not yet defined")
  | TInterpString _ -> TPointer (TInt I8)
  | TArrayLit (_, t) -> t
  | TIndex (_, _, t) -> t
  | TLen _ -> TInt Usize
  | TToSlice (_, t) -> t
  | TSliceExpr (_, _, _, t) -> t
  | TDataPtr (_, t) -> t
  | TZero t -> t
  | TUndef t -> t
