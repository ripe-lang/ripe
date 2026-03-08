open Types

type texpr =
  | TInt of int * ty
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
}

type tdecl =
  | TFunc of tfunc_def
  | TStruct of string * (string * ty) list (* name, typed fields *)
  | TExtern of tfunc_def

let ty_of_texpr (e : texpr) : ty =
  match e with
  | TInt (_, t) -> t
  | TBool _ -> TBool
  | TNull t -> t
  (* TODO: Revisit string/char literal typing for length/safety/etc *)
  | TString _ -> TPointer (TInt I8)
  | TChar _ -> TInt I32
  | TIdent (_, t) -> t
  | TCall (_, _, t) -> t
  | TBinOp (_, _, _, t) -> t
  | TUnOp (_, _, t) -> t
  | TFieldAccess (_, _, t) -> t
  | TCast (_, t) -> t
  | TSizeOf _ -> TInt I64
  | TRange _ -> raise (Invalid_argument "TODO: range type not yet defined")
