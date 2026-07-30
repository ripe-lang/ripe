(* SPDX-License-Identifier: GPL-2.0-only *)

open Types

type texpr_desc =
  | TErrorExpr
  | TInt of int64
  | TFloat of float
  | TBool of bool
  | TNull
  | TCStr of string
  | TChar of int
  | TIdent of Symbol.t
  | TCall of texpr * texpr list * int option
  | TBinOp of Ast.binop * texpr * texpr
  | TUnOp of Ast.unop * texpr
  | TFieldAccess of texpr * string
  (* Target type is the node type *)
  | TCast of texpr * Ast.cast_kind
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
  | TBlock of tblock
  | TIf of (texpr * tblock) list * tblock option
  | TWhile of texpr * tblock
  | TFor of Symbol.t * ty * texpr * tblock
  | TBinding of Ast.binding_kind * Symbol.t * ty * texpr
  | TReturn of texpr option
  | TBreak
  | TContinue
  | TPairAssign of texpr * texpr * texpr * texpr

and texpr = { desc : texpr_desc; ty : ty; span : Ast.span }
[@@deriving show { with_path = false }]

and tblock = texpr list [@@deriving show { with_path = false }]

let mk ?(span = Ast.dummy_span) (ty : ty) (desc : texpr_desc) : texpr =
  { desc; ty; span }

type tfunc_def = {
  name : string;
  params : (Symbol.t * ty) list;
  ret_ty : ty;
  body : tblock;
  modifiers : Ast.modifier list;
  variadic : bool;
}
[@@deriving show { with_path = false }]

let tfunc_name (fd : tfunc_def) : string = fd.name

type tglobal_def = {
  key : Symbol.key;
  name : string;
  ty : ty;
  init : texpr option;
  kind : Ast.binding_kind;
}
[@@deriving show { with_path = false }]

type tdecl =
  | TFunc of tfunc_def
  | TStruct of string * (string * ty) list * Ast.modifier list
    (* This has a name typed fields and modifiers *)
  | TExtern of tfunc_def
  | TGlobal of tglobal_def
  | TTypeAlias of string * ty
  | TNewtype of string * ty
[@@deriving show { with_path = false }]
