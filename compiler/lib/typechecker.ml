(* Bidirectional type checker *)

open Ast
open Types

exception TypeError of string

type func_sig = { param_tys : ty list; ret_ty : ty }
type struct_info = { field_tys : (string * ty) list }

type env = {
  vars : (string * ty) list;
  funcs : (string, func_sig) Hashtbl.t;
  structs : (string, struct_info) Hashtbl.t;
  ret_ty : ty;
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

let rec ty_of_ast (env : env) (t : typ) : ty =
  match t with
  | Named "i8" -> TInt I8
  | Named "i16" -> TInt I16
  | Named "i32" -> TInt I32
  | Named "i64" -> TInt I64
  | Named "u8" -> TInt U8
  | Named "u16" -> TInt U16
  | Named "u32" -> TInt U32
  | Named "u64" -> TInt U64
  | Named "bool" -> TBool
  | Named name ->
      if Hashtbl.mem env.structs name then TStruct name
      else raise (TypeError ("unknown type: " ^ name))
  | Pointer t -> TPointer (ty_of_ast env t)

(* First pass collecting signatures so that the compiler
   can handle forward references *)
let collect_func (env : env) (fd : func_def) : unit =
  Printf.printf "Signature: %s\n%!" fd.name;

  (* Map AST to internal type to avoid reparsing *)
  let param_tys =
    List.map
      (fun (p : param) ->
        let t = ty_of_ast env p.typ in
        Printf.printf "%s: %s\n%!" p.name (show_ty t);
        t)
      fd.params
  in

  (* Default to void type when user omits the return type *)
  let ret_ty = match fd.ret with Some t -> ty_of_ast env t | None -> TVoid in
  Printf.printf "returns: %s\n%!" (show_ty ret_ty);

  Hashtbl.replace env.funcs fd.name { param_tys; ret_ty }

let collect_decl (env : env) (decl : decl) : unit =
  match decl with Func fd -> collect_func env fd | _ -> ()

let typecheck (_decls : decl list) : unit =
  let _env = make_env () in
  List.iter (collect_decl _env) _decls;
  ()
