(* SPDX-License-Identifier: Apache-2.0 *)

open Types

(* A cast's target type is the node type *)
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
  | TSizeOf of ty
  | TRange of texpr * texpr
  | TRangeInclusive of texpr * texpr
  | TArrayLit of texpr list
  | TIndex of texpr * texpr
  | TLen of texpr
  | TSliceExpr of texpr * texpr * texpr
  | TDataPtr of texpr
  | TZero
  | TBuild of Keyname.t * int64 * (int * texpr) list
  | TVariant of Keyname.t * int64
  | TBlock of tblock
  | TIf of (texpr * tblock) list * tblock option
  | TFor of Symbol.t * ty * texpr * tblock
  | TBinding of Symbol.t * ty * texpr
  | TReturn of texpr option
  | TBreak of int * texpr option
  | TContinue of int
  | TLocalDecl
  | TLoop of tblock
  | TMatch of texpr * tarm list
  | TUnitLit
  | TTypeExpr

and texpr = { tdesc : texpr_desc; ty : ty; tspan : Ast.span }
[@@deriving show { with_path = false }]

and tblock = texpr list [@@deriving show { with_path = false }]

and tarm = { tpat : tpattern; tbody : tblock }
[@@deriving show { with_path = false }]

(* A test compares part of the scrutinee and a binding names part of it *)
and tpattern =
  | TPatWild
  | TPatBind of Symbol.t * ty
  | TPatConst of int64
  | TPatVariant of int64 * tpattern list
[@@deriving show { with_path = false }]

let mk span ty desc = { tdesc = desc; ty; tspan = span }

type tfunc_def = {
  tfunc_key : Symbol.key;
  tfunc_name : string;
  tfunc_entry_point : bool;
  tfunc_params : (Symbol.t * ty) list;
  tfunc_ret : ty;
  tfunc_body : tblock;
  tfunc_export : bool;
  tfunc_abi : func_abi;
}
[@@deriving show { with_path = false }]

type tglobal_def = {
  tglobal_key : Symbol.key;
  tglobal_name : string;
  tglobal_ty : ty;
  tglobal_init : texpr option;
}
[@@deriving show { with_path = false }]

type tdecl =
  | TFuncDecl of tfunc_def
  | TAdtDecl of Keyname.t * adt_kind * ty list list
  | TLocalAdtDecl of Keyname.t * adt_kind * ty list list
  | TExternDecl of string * ty
  | TGlobalDecl of tglobal_def
  | TTypeAliasDecl of Keyname.t * ty
[@@deriving show { with_path = false }]
