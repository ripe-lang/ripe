(* Bidirectional type checker *)

open Ast
open Types

exception TypeError of string

type func_sig = {
  param_tys : ty list;
  ret_ty    : ty;
}

type struct_info = { field_tys : (string * ty) list}

type env = {
  vars    : (string * ty) list;
  funcs   : (string, func_sig) Hashtbl.t;
  structs : (string, struct_info) Hashtbl.t;
  ret_ty  : ty;
  in_loop : bool;
}

(* control flow + struct/funcs rn *)
let make_env () : env =
  {
    vars = [];
    funcs = Hashtbl.create 16;
    structs = Hashtbl.create 16;
    ret_ty = TVoid;
    in_loop = false;
  }

let typecheck (_decls : decl list) : unit =
  let _env = make_env () in
  ()
