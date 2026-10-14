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
  (* Target type is the node type *)
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
  | TStructLit of Keyname.t * (int * texpr) list
  (* TODO(6c3f): a payload variant carries its arguments here too *)
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
[@@deriving show { with_path = false }]

and tblock = texpr list [@@deriving show { with_path = false }]

and tarm = { tpat : tpattern; tbody : tblock }
[@@deriving show { with_path = false }]

(* A test compares part of the scrutinee and a binding names part of it *)
and tpattern = TPatWild | TPatBind of Symbol.t * ty | TPatConst of int64
[@@deriving show { with_path = false }]

let mk ?(span = Ast.dummy_span) ty desc = { desc; ty; span }

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
[@@deriving show { with_path = false }]

type tglobal_def = {
  key : Symbol.key;
  name : string;
  ty : ty;
  init : texpr option;
}
[@@deriving show { with_path = false }]

type tdecl =
  | TFuncDecl of tfunc_def
  | TStructDecl of Keyname.t * ty list (* This has a name and typed fields *)
  | TLocalStructDecl of Keyname.t * ty list
  | TExternDecl of tfunc_def
  | TGlobalDecl of tglobal_def
  | TTypeAliasDecl of Keyname.t * ty
  (* An enum is an integer at runtime so nothing past here needs its variants *)
  | TEnumDecl of Keyname.t
[@@deriving show { with_path = false }]
