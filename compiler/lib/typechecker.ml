(* Bidirectional type checker *)

open Ast
open Types

exception TypeError of string

(* TODO: Figure out "-typecheck" with better Printf formatting *)

type func_sig = { param_tys : ty list; ret_ty : ty }
type struct_info = { field_tys : (string * ty) list }

type env = {
  vars : (string * ty) list;
  funcs : (string, func_sig) Hashtbl.t;
  structs : (string, struct_info) Hashtbl.t;
  ret_ty : ty;
  in_loop : bool;
}

let make_env () : env =
  {
    vars = [];
    funcs = Hashtbl.create 16;
    structs = Hashtbl.create 16;
    ret_ty = TVoid;
    in_loop = false;
  }

(* TODO: New bindings shadow older ones. *)
let extend_var (env : env) (name : string) (t : ty) : env =
  { env with vars = (name, t) :: env.vars }

let lookup_var (env : env) (name : string) : ty =
  match List.assoc_opt name env.vars with
  | Some t -> t
  | None -> raise (TypeError ("unbound variable: " ^ name))

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
  (* Printf.printf "Signature: %s\n%!" fd.name; *)

  (* Map AST to internal type to define the functions global
  signature *)
  let param_tys =
    List.map
      (fun (p : param) ->
        let t = ty_of_ast env p.typ in
        (* Printf.printf "%s: %s\n%!" p.name (show_ty t); *)
        t)
      fd.params
  in

  (* Default to void type when user omits the return type *)
  let ret_ty = match fd.ret with Some t -> ty_of_ast env t | None -> TVoid in
  (* Printf.printf "returns: %s\n%!" (show_ty ret_ty); *)

  Hashtbl.replace env.funcs fd.name { param_tys; ret_ty }

(* TODO: Support forward reference between structs *)
let collect_struct (env : env) (sd : struct_def) : unit =
  let field_tys =
    List.map (fun (f : field) -> (f.name, ty_of_ast env f.typ)) sd.fields
  in
  Hashtbl.replace env.structs sd.name { field_tys }

let collect_decl (env : env) (decl : decl) : unit =
  match decl with
  | Struct sd -> collect_struct env sd
  | Func fd | Extern fd -> collect_func env fd

(* Second pass doing the bidirectional type checking *)

(* Figure out the type*)
let rec synth (env : env) (e : expr) : Typed_ast.texpr =
  match e with
  | Int n ->
      Typed_ast.TInt (n, TInt I32)
      (* TODO: Widen based on value magnitude (fall back I32) *)
  | Bool b -> Typed_ast.TBool b
  | Null -> Typed_ast.TNull TNull
  | String s -> Typed_ast.TString s
  | Char c -> Typed_ast.TChar c
  | Ident name -> Typed_ast.TIdent (name, lookup_var env name)

(* MUST be this type *)

(* TODO: Better error messages *)
let rec check_stmt (env : env) (s : stmt) : env * Typed_ast.tstmt =
  match s with
  | Break ->
      if not env.in_loop then
        raise (TypeError "break statement must be inside a loop");
      (env, Typed_ast.TBreak)
  | Continue ->
      if not env.in_loop then
        raise (TypeError "continue statement must be inside a loop");
      (env, Typed_ast.TContinue)
  | Return None ->
      if env.ret_ty <> TVoid then
        raise (TypeError "empty return in non-void function");
      (env, Typed_ast.TReturn None)

(* FIXME: Temporary until exhaustive *)
(* | _ -> (env, Typed_ast.TContinue)   *)

(* Performance critical since this pass walks every statement *)
and check_stmts (env : env) (stmts : stmt list) : env * Typed_ast.tstmt list =
  let final_env, tstmts_reversed =
    List.fold_left
      (fun (current_env, acc) s ->
        let next_env, ts = check_stmt current_env s in
        (* Printf.printf "Added %d to environment.\n" (List.length next_env.vars); *)
        (next_env, ts :: acc)) (* Slow: e', acc @ [ ts ] *)
      (env, []) stmts
  in
  (final_env, List.rev tstmts_reversed)

let check_func (env : env) (fd : func_def) : Typed_ast.tfunc_def =
  (* Redo param types here so we can set up local scope before checking the body. *)
  let params =
    List.map (fun (p : param) -> (p.name, ty_of_ast env p.typ)) fd.params
  in

  (* Add each param to the env so the body can use them as locals. *)
  let param_env =
    List.fold_left (fun e (name, t) -> extend_var e name t) env params
  in

  let ret_ty = match fd.ret with Some t -> ty_of_ast env t | None -> TVoid in
  let body_env = { param_env with ret_ty } in

  let _, tbody = check_stmts body_env fd.body in

  { Typed_ast.name = fd.name; params; ret_ty; body = tbody }

let check_decl (env : env) (decl : decl) : Typed_ast.tdecl =
  match decl with
  | Func fd ->
      let tfd = check_func env fd in
      Typed_ast.TFunc tfd
  | Extern fd ->
      let tfd = check_func env fd in
      Typed_ast.TExtern tfd
  (* TODO: Add structs *)
  | _ -> failwith "Declaration not supported yet"
(* TODO: I need to think about global variables *)

let typecheck (decls : decl list) : Typed_ast.tdecl list =
  let env = make_env () in
  List.iter (collect_decl env) decls;
  List.map (check_decl env) decls
