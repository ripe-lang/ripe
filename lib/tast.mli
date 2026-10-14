(* SPDX-License-Identifier: Apache-2.0 *)

open Types

type texpr_desc =
  | TErrorExpr
  | TIntLit of int64
  | TFloatLit of float
  | TBoolLit of bool
  | TNullLit
  | TCStrLit of string
  | TStrLit of string
  | TCharLit of int
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
  | TUnitLit

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
  | TFuncDecl of tfunc_def
  | TStructDecl of Keyname.t * ty list
  | TLocalStructDecl of Keyname.t * ty list
  | TExternDecl of tfunc_def
  | TGlobalDecl of tglobal_def
  | TTypeAliasDecl of Keyname.t * ty
  | TEnumDecl of Keyname.t

val mk : ?span:Ast.span -> ty -> texpr_desc -> texpr
val show_tdecl : tdecl -> string
