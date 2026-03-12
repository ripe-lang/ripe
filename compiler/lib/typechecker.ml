(* SPDX-License-Identifier: GPL-2.0-only *)

(* Bidirectional type checker *)

open Ast
open Types

exception TypeError of string

(* TODO: Figure out "-typecheck" with better Printf formatting *)
(* TODO: I need to update Parser to allow `let` and `var` in toplevel
then update collect_decl in the first pass so that they're collected and
available for other functions *)

(* TODO: I should be allowed to shadow function name with a variable but not
with another function in the same scope. (same with structs) *)

(* lvalue - has a presis address in memory e.g. variable,s array elements, struct fields, etc *)
(* rvalue - temp value that doesn't have presis memory e.g literals, result of math, etc *)

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

let lookup_func (env : env) (name : string) : func_sig =
  match Hashtbl.find_opt env.funcs name with
  | Some s -> s
  | None -> raise (TypeError ("unknown function: " ^ name))

let lookup_struct (env : env) (name : string) : struct_info =
  match Hashtbl.find_opt env.structs name with
  | Some s -> s
  | None -> raise (TypeError ("unknown struct: " ^ name))

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
  | Named "f32" -> TFloat F32
  | Named "f64" -> TFloat F64
  | Named "bool" -> TBool
  | Named name ->
      if Hashtbl.mem env.structs name then TStruct name
      else raise (TypeError ("unknown type: " ^ name))
  | Pointer t -> TPointer (ty_of_ast env t)

(* Exact equality but NULL is compatible with any pointer *)
(* TODO: Is **i32 compatible with **null? TInt I8 with a TInt I32 (without cast)? *)
(* TODO: Add rawptr/void* *)
let rec compatible (want : ty) (got : ty) : bool =
  (* Printf.printf "Comparing %s with %s\n" (show_ty want) (show_ty got); *)
  match (want, got) with
  | TPointer _, TNull -> true
  | TPointer a, TPointer b -> compatible a b
  | _ -> want = got

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

  (* Default to void, except main implicitly returns i32 for the C runtime *)
  let ret_ty =
    match fd.ret with
    | Some t -> ty_of_ast env t
    | None -> if fd.name = "main" then TInt I32 else TVoid
  in
  (* Printf.printf "returns: %s\n%!" (show_ty ret_ty); *)

  (* FIXME: Check for duplicate function/extern definitions. Need to fix
     how extern foo() and a local foo() with the same name *)
  (* if Hashtbl.mem env.funcs fd.name then
    raise (TypeError ("function already defined: " ^ fd.name)) *)

  Hashtbl.replace env.funcs fd.name { param_tys; ret_ty }

(* TODO: Support forward reference between structs *)
(* This will fail if Struct A has a field of type Struct B and B is defined after A *)
(* FIXME: Add DFS cycle detection to prevent infinite recursion*)
let collect_struct (env : env) (sd : struct_def) : unit =
  if Hashtbl.mem env.structs sd.name then
    raise (TypeError ("struct already defined: " ^ sd.name));
  (* TODO: Add a rawptr/voidptr keyword for untyped pointers (C's void pointer) *)
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
      (* Printf.printf "int %d\n" n; *)
      Typed_ast.TInt (n, TInt I32)
      (* TODO: Widen based on value magnitude (fall back I32) *)
  | Float f -> Typed_ast.TFloat (f, TFloat F64)
  | Bool b ->
      (* Printf.printf "bool %b\n" b; *)
      Typed_ast.TBool b
  | Null ->
      (* print_endline "null"; *)
      Typed_ast.TNull TNull
  | String s ->
      (* Printf.printf "string \"%s\"\n" s; *)
      Typed_ast.TString s
  | Char c ->
      (* Printf.printf "char: '%c'\n" c; *)
      Typed_ast.TChar c
  | Ident name ->
      let t = lookup_var env name in
      (* Printf.printf "ident: `%s` (found type: %s)\n" name (show_ty t); *)
      Typed_ast.TIdent (name, t)
  | Call (name, args) ->
      let sig_ = lookup_func env name in
      let targs = check_args env sig_ args in
      Typed_ast.TCall (name, targs, sig_.ret_ty)
  | BinOp (op, l, r) -> synth_binop env op l r
  | UnOp (op, e) -> synth_unop env op e
  | FieldAccess (e, fname) -> synth_field env e fname
  | Cast (e, t) ->
      let te = synth env e in
      let ty = ty_of_ast env t in
      Typed_ast.TCast (te, ty)
  | SizeOf t -> Typed_ast.TSizeOf (ty_of_ast env t)
  | Range (a, b) ->
      let ta = synth env a in
      let tb = synth env b in
      Typed_ast.TRange (ta, tb)
(* | _ -> failwith ("Expression not yet implemented: " ^ show_expr e) *)

(* MUST be this type *)
and check (env : env) (e : expr) (want : ty) : Typed_ast.texpr =
  match e with
  | Int n -> (
      (* TODO: Validate n fits within want (e.g. reject 300 into u8). Also, inferred literals still default to I32 large values overflow. *)
      match want with
      | TInt _ -> Typed_ast.TInt (n, want)
      (* want is not an integer type at all e.g. let y: bool = 20 *)
      | _ ->
          raise
            (TypeError
               (Printf.sprintf "type mismatch: expected %s, got (TInt I32)"
                  (show_ty want))))
  | Float f -> (
      match want with
      | TFloat _ -> Typed_ast.TFloat (f, want)
      | _ ->
          raise
            (TypeError
               (Printf.sprintf "type mismatch: expected %s, got (TFloat F64)"
                  (show_ty want))))
  (* not a flexible literal, just check it matches want *)
  | _ ->
      let te = synth env e in
      let got = Typed_ast.ty_of_texpr te in
      if not (compatible want got) then
        raise
          (TypeError
             (Printf.sprintf "type mismatch: expected %s, got %s" (show_ty want)
                (show_ty got)));
      te

and check_args (env : env) (sig_ : func_sig) (args : expr list) :
    Typed_ast.texpr list =
  (* TODO: Support for variadic functions *)
  let n_params = List.length sig_.param_tys in
  let n_args = List.length args in
  if n_params <> n_args then
    raise
      (TypeError
         (Printf.sprintf "wrong number of arguments: expected %d, got %d"
            n_params n_args));
  List.map2 (fun e want -> check env e want) args sig_.param_tys

(* let is_numeric = function
    | TInt _ | TFloat -> true
    | _ -> false *)

(* let is_ordered = function
  | TInt _ | TFloat | TChar | TPointer _ -> true
  | _ -> false *)

(* let is_lvalue (te : Typed_ast.texpr) : bool =
  match te with
  | TIdent _ -> true
  | TUnOp (Deref, _, _) -> true
  | TFieldAccess _ -> true
  | _ -> false *)

and synth_binop (env : env) (op : binop) (l : expr) (r : expr) : Typed_ast.texpr
    =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, t)
      (* TODO: restrict to numeric types *)
  | Eq | Neq ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, TBool)
  | Lt | Gt | Lte | Gte ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      (* if not (is_ordered t) then
        raise (TypeError (Printf.sprintf "type %s is not ordered" (show_ty t))); *)
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, TBool)
      (* TODO: restrict to ordered types e.g. void > void should not work. bool > bool maybe*)
  | And | Or ->
      let tl = check env l TBool in
      let tr = check env r TBool in
      Typed_ast.TBinOp (op, tl, tr, TBool)
  | BitAnd | BitOr | BitXor | Lshift | Rshift ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, t)
      (* TODO: restrict to integer types *)
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign ->
      let tl = synth env l in
      (* if not (is_lvalue tl) then
        raise (TypeError "left-hand side of assignment must be an lvalue"); *)

      let t = Typed_ast.ty_of_texpr tl in
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, t)
(* TODO: check lvalue on left because 5 = 10 is allowed *)
(* | _ -> failwith ("Operator not yet implemented: " ^ show_binop op) *)

and synth_unop (env : env) (op : unop) (e : expr) : Typed_ast.texpr =
  match op with
  | Neg ->
      let te = synth env e in
      Typed_ast.TUnOp (op, te, Typed_ast.ty_of_texpr te)
      (* TODO: restrict to numeric *)
  | Not ->
      let te = check env e TBool in
      Typed_ast.TUnOp (op, te, TBool)
  | BitNot ->
      let te = synth env e in
      Typed_ast.TUnOp (op, te, Typed_ast.ty_of_texpr te)
      (* TODO: restrict to integer *)
  | PreInc | PreDec | PostInc | PostDec ->
      raise (TypeError "++/-- only allowed as statements")
  | Deref -> (
      let te = synth env e in
      match Typed_ast.ty_of_texpr te with
      | TPointer inner -> Typed_ast.TUnOp (op, te, inner)
      | t ->
          raise
            (TypeError ("cannot dereference non-pointer type: " ^ show_ty t)))
  | AddressOf ->
      let te = synth env e in
      Typed_ast.TUnOp (op, te, TPointer (Typed_ast.ty_of_texpr te))

(* Synthesize the type of a field access expression. *)
and synth_field (env : env) (e : expr) (fname : string) : Typed_ast.texpr =
  (* TODO: auto-deref ^struct so ptr.field works like ptr->field in C. *)
  let te = synth env e in
  match Typed_ast.ty_of_texpr te with
  | TStruct sname -> (
      let info = lookup_struct env sname in
      match List.assoc_opt fname info.field_tys with
      | Some ft -> Typed_ast.TFieldAccess (te, fname, ft)
      | None -> raise (TypeError ("struct " ^ sname ^ " has no field " ^ fname))
      )
  | t -> raise (TypeError ("field access on non-struct type: " ^ show_ty t))

let synth_inc_dec (env : env) (op : unop) (e : expr) : Typed_ast.texpr =
  let te = synth env e in
  Typed_ast.TUnOp (op, te, Typed_ast.ty_of_texpr te)

(* TODO: Better error messages *)
let rec check_stmt (env : env) (s : stmt) : env * Typed_ast.tstmt =
  match s with
  | Let (name, ann, e) ->
      let t, te =
        match ann with
        | Some a ->
            let want = ty_of_ast env a in
            let te = check env e want in
            (want, te)
        | None ->
            let te = synth env e in
            (Typed_ast.ty_of_texpr te, te)
      in
      (extend_var env name t, Typed_ast.TLet (name, t, te))
  | Var (name, ann, e) ->
      let t, te =
        match ann with
        | Some a ->
            let want = ty_of_ast env a in
            let te = check env e want in
            (want, te)
        | None ->
            let te = synth env e in
            (Typed_ast.ty_of_texpr te, te)
      in
      (extend_var env name t, Typed_ast.TVar (name, t, te))
  | Return None ->
      if env.ret_ty <> TVoid then
        raise (TypeError "empty return in non-void function");
      (env, Typed_ast.TReturn None)
  | Return (Some e) ->
      let te = check env e env.ret_ty in
      (env, Typed_ast.TReturn (Some te))
  | Expr (UnOp (((PreInc | PreDec | PostInc | PostDec) as op), e)) ->
      let te = synth_inc_dec env op e in
      (env, Typed_ast.TExpr te)
  | Expr e ->
      let te = synth env e in
      (env, Typed_ast.TExpr te)
  | If (branches, else_body) ->
      let tbranches =
        List.map
          (fun (cond, body) ->
            let tc = check env cond TBool in
            let _, tbody = check_stmts env body in
            (tc, tbody))
          branches
      in
      let _, telse = check_stmts env else_body in
      (env, Typed_ast.TIf (tbranches, telse))
  | While (cond, body) ->
      let tc = check env cond TBool in
      let _, tbody = check_stmts { env with in_loop = true } body in
      (env, Typed_ast.TWhile (tc, tbody))
  | For (name, iter, body) ->
      (* TODO: iter must be a range bind name to its element type *)
      let titer = synth env iter in
      let elem_ty = TInt I32 in
      let _, tbody =
        check_stmts (extend_var { env with in_loop = true } name elem_ty) body
      in
      (env, Typed_ast.TFor (name, elem_ty, titer, tbody))
  | CFor (init, cond, post, body) ->
      let env', tinit = check_stmt env init in
      let tcond = check env' cond TBool in
      let tpost =
        match post with
        | UnOp (((PreInc | PreDec | PostInc | PostDec) as op), e) ->
            synth_inc_dec env' op e
        | _ -> synth env' post
      in
      let _, tbody = check_stmts { env' with in_loop = true } body in
      (* Throwing away the previous env to be similar to C-style scoping *)
      (env, Typed_ast.TCFor (tinit, tcond, tpost, tbody))
  | Break ->
      if not env.in_loop then
        raise (TypeError "break statement must be inside a loop");
      (env, Typed_ast.TBreak)
  | Continue ->
      if not env.in_loop then
        raise (TypeError "continue statement must be inside a loop");
      (env, Typed_ast.TContinue)
  | Block stmts ->
      let _, tstmts = check_stmts env stmts in
      (env, Typed_ast.TBlock tstmts)
(* | _ -> failwith ("Statement not yet implemented: " ^ show_stmt s) *)

(* Performance critical since this pass walks every statement *)
and check_stmts (env : env) (stmts : stmt list) : env * Typed_ast.tstmt list =
  let final_env, tstmts_reversed =
    (* TODO: I keep checking after a return statement (raise a warning) *)
    List.fold_left
      (fun (current_env, acc) s ->
        let next_env, ts = check_stmt current_env s in
        (* Printf.printf "Added %d to environment.\n" (List.length next_env.vars); *)
        (next_env, ts :: acc))
        (* Slow: e', acc @ [ ts ] this is O(n^2) I think with append*)
      (env, []) stmts
  in
  (final_env, List.rev tstmts_reversed)

let check_func (env : env) (fd : func_def) : Typed_ast.tfunc_def =
  (* FIXME: recalculating params and ret_ty. *)
  (* Redo param types here so we can set up local scope before checking the body. *)
  let params =
    List.map (fun (p : param) -> (p.name, ty_of_ast env p.typ)) fd.params
  in

  (* Add each param to the env so the body can use them as locals. *)
  let param_env =
    List.fold_left (fun e (name, t) -> extend_var e name t) env params
  in

  let ret_ty =
    match fd.ret with
    | Some t -> ty_of_ast env t
    | None -> if fd.name = "main" then TInt I32 else TVoid
  in
  let body_env = { param_env with ret_ty } in

  (* TODO: I need to note "missing return statement" *)
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
  | Struct sd ->
      let info = lookup_struct env sd.name in
      Typed_ast.TStruct (sd.name, info.field_tys)
(* | _ -> failwith "Declaration not supported yet" *)
(* TODO: I need to think about global variables *)

let typecheck (decls : decl list) : Typed_ast.tdecl list =
  let env = make_env () in
  List.iter (collect_decl env) decls;
  List.map (check_decl env) decls
