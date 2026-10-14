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
  | TStructLit of Keyname.t * (int * texpr) list
  | TVariant of Keyname.t * int64
  | TBlock of tblock
  | TIf of (texpr * tblock) list * tblock option
  | TWhile of Ast.loop_label option * texpr * tblock
  | TFor of Ast.loop_label option * Symbol.t * ty * texpr * tblock
  | TBinding of Symbol.t * ty * texpr
  | TReturn of texpr option
  | TBreak of Ast.loop_label option * texpr option
  | TContinue of Ast.loop_label option
  | TLocalDecl
  | TLoop of Ast.loop_label option * tblock
  | TMatch of texpr * tarm list
  | TUnit

and texpr = { desc : texpr_desc; ty : ty; span : Ast.span }
and tblock = texpr list
and tarm = { tpat : tpattern; tbody : tblock }
and tpattern = TPatWild | TPatBind of Symbol.t * ty | TPatConst of int64

type tfunc_def = {
  key : Symbol.key;
  name : string;
  source_name : string;
  entry_point : bool;
  params : (Symbol.t * ty) list;
  ret_ty : ty;
  body : tblock;
  export : bool;
  variadic : bool;
  abi : func_abi;
}

type tglobal_def = {
  key : Symbol.key;
  name : string;
  ty : ty;
  init : texpr option;
}

type tdecl =
  | TFunc of tfunc_def
  | TStruct of Keyname.t * ty list
  | TLocalStruct of Keyname.t * ty list
  | TExtern of tfunc_def
  | TGlobal of tglobal_def
  | TTypeAlias of Keyname.t * ty
  | TEnum of Keyname.t

val mk : ?span:Ast.span -> ty -> texpr_desc -> texpr
val show_tdecl : tdecl -> string
