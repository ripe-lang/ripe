(* SPDX-License-Identifier: GPL-2.0-only *)

open Types

type tinterp_part = TLit of string | TInterp of texpr

and texpr =
  | TInt of int * ty
  | TFloat of float * ty
  | TBool of bool
  | TNull of ty
  | TString of string
  | TChar of char
  | TIdent of string * ty
  | TCall of string * texpr list * ty
  | TBinOp of Ast.binop * texpr * texpr * ty
  | TUnOp of Ast.unop * texpr * ty
  | TFieldAccess of texpr * string * ty
  | TCast of texpr * ty
  | TSizeOf of Types.ty
  | TRange of texpr * texpr
  | TInterpString of tinterp_part list

type tstmt =
  | TLet of string * ty * texpr
  | TVar of string * ty * texpr
  | TReturn of texpr option
  | TIf of (texpr * tstmt list) list * tstmt list
  | TWhile of texpr * tstmt list
  | TFor of string * ty * texpr * tstmt list
  | TCFor of tstmt * texpr * texpr * tstmt list
  | TBreak
  | TContinue
  | TExpr of texpr
  | TBlock of tstmt list

type tfunc_def = {
  name : string;
  params : (string * ty) list;
  ret_ty : ty;
  body : tstmt list;
  modifiers : Ast.modifier list;
}

type tdecl =
  | TFunc of tfunc_def
  | TStruct of string * (string * ty) list * Ast.modifier list
    (* name, typed fields, modifiers *)
  | TExtern of tfunc_def

let ty_of_texpr (e : texpr) : ty =
  match e with
  | TInt (_, t) -> t
  | TFloat (_, t) -> t
  | TBool _ -> TBool
  | TNull t -> t
  (* TODO(a99d): Replace TString with TSlice (TInt U8), fat pointer {ptr, len} like Rust/Zig. *)
  | TString _ -> TPointer (TInt I8)
  | TChar _ -> TInt I32
  | TIdent (_, t) -> t
  | TCall (_, _, t) -> t
  | TBinOp (_, _, _, t) -> t
  | TUnOp (_, _, t) -> t
  | TFieldAccess (_, _, t) -> t
  | TCast (_, t) -> t
  | TSizeOf _ -> TInt I64
  | TRange _ -> raise (Invalid_argument "TODO(cb82): range type not yet defined")
  | TInterpString _ -> TPointer (TInt I8)
