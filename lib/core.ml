(* SPDX-License-Identifier: GPL-2.0-only *)

open Types

type loop_id = int [@@deriving show { with_path = false }]

type cexpr_desc =
  | CInt of int64
  | CFloat of float
  | CBool of bool
  | CNull
  | CCStr of string
  | CStr of string
  | CChar of int
  | CIdent of Symbol.t
  | CCall of cexpr * cexpr list * int option
  | CBinOp of Ast.binop * cexpr * cexpr
  | CUnOp of Ast.unop * cexpr
  | CFieldAccess of cexpr * int
  (* Target type is the node type *)
  | CCast of cexpr * Ast.cast_kind
  | CSizeOf of Types.ty
  | CArrayLit of cexpr list
  | CIndex of cexpr * cexpr
  | CLen of cexpr
  | CToSlice of cexpr
  | CSliceExpr of cexpr * cexpr * cexpr
  | CDataPtr of cexpr
  | CZero
  | CUndef
  | CStructLit of Qname.t * (int * cexpr) list
  | CBlock of cblock
  | CIf of (cexpr * cblock) list * cblock option
  | CLoop of loop_id * cblock * cblock
  | CBinding of Ast.binding_kind * Symbol.t * ty * cexpr
  | CReturn of cexpr option
  | CBreak of loop_id
  | CContinue of loop_id

and cexpr = { desc : cexpr_desc; ty : ty; span : Ast.span }
and cblock = cexpr list [@@deriving show { with_path = false }]

let mk ?(span = Ast.dummy_span) (ty : ty) (desc : cexpr_desc) : cexpr =
  { desc; ty; span }

type cfunc_def = {
  name : string;
  entry_point : bool;
  params : (Symbol.t * ty) list;
  ret_ty : ty;
  body : cblock;
  modifiers : Ast.modifier list;
  variadic : bool;
}
[@@deriving show { with_path = false }]

type cglobal_def = {
  name : string;
  ty : ty;
  init : cexpr option;
  kind : Ast.binding_kind;
  modifiers : Ast.modifier list;
}
[@@deriving show { with_path = false }]

type cdecl =
  | CFunc of cfunc_def
  | CStruct of Qname.t * ty list * Ast.modifier list
    (* This has a name typed fields and modifiers *)
  | CLocalStruct of Qname.t * ty list
  | CExtern of cfunc_def
  | CGlobal of cglobal_def
  | CTypeAlias of Qname.t * ty
  | CNewtype of Qname.t * ty
[@@deriving show { with_path = false }]
