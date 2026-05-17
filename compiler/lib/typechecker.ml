(* SPDX-License-Identifier: GPL-2.0-only *)

(* Bidirectional type checker *)

open Ast
open Types

exception TypeErrors of string list

(* TODO(38df): I need to update Parser to allow `let` and `var` in toplevel
then update collect_decl in the first pass so that they're collected and
available for other functions *)

(* TODO(0d41): I should be allowed to shadow function name with a variable but not
with another function in the same scope. (same with structs) *)

(* lvalue - has a presis address in memory e.g. variable,s array elements, struct fields, etc *)
(* rvalue - temp value that doesn't have presis memory e.g literals, result of math, etc *)

type func_sig = { param_tys : ty list; ret_ty : ty }
type struct_info = { field_tys : (string * ty) list }
type var_info = { ty : ty; used : bool ref; span : Ast.span }

type env = {
  vars : (string * var_info) list list;
  funcs : (string, func_sig) Hashtbl.t;
  structs : (string, struct_info) Hashtbl.t;
  ret_ty : ty;
  in_loop : bool;
  errors : string list ref;
  warnings : string list ref;
  filename : string;
  sm : Source_map.t;
  current_span : Ast.span ref;
}

let make_env (filename : string) (sm : Source_map.t) : env =
  {
    vars = [];
    funcs = Hashtbl.create 16;
    structs = Hashtbl.create 16;
    ret_ty = TVoid;
    in_loop = false;
    errors = ref [];
    warnings = ref [];
    filename;
    sm;
    current_span = ref Ast.dummy_span;
  }

let add_error (env : env) (msg : string) : unit =
  let span = !(env.current_span) in
  let line, col = Source_map.lookup env.sm span.lo in
  let formatted = Printf.sprintf "%s:%d:%d: %s" env.filename line col msg in
  env.errors := formatted :: !(env.errors)

let dummy_texpr = Typed_ast.TInt (0, TInt I32)

let add_warning (env : env) (msg : string) : unit =
  let span = !(env.current_span) in
  let line, col = Source_map.lookup env.sm span.lo in
  let formatted =
    Printf.sprintf "%s:%d:%d: warning: %s" env.filename line col msg
  in
  env.warnings := formatted :: !(env.warnings)

let push_scope (env : env) : env = { env with vars = [] :: env.vars }

let pop_scope (env : env) : unit =
  match env.vars with
  | [] -> ()
  | scope :: _ ->
      List.iter
        (fun (name, info) ->
          (* variables prefixed with '_' suppress unused warnings *)
          if (not !(info.used)) && name.[0] <> '_' then (
            env.current_span := info.span;
            add_warning env (Printf.sprintf "'%s' declared but never used" name)))
        scope

let extend_var ?(used = false) (env : env) (name : string) (t : ty) : env =
  let info = { ty = t; used = ref used; span = !(env.current_span) } in
  match env.vars with
  | [] -> assert false (* no active scope *)
  | scope :: rest ->
      if List.mem_assoc name scope then
        add_error env
          (Printf.sprintf "'%s' is already declared in this scope" name);
      { env with vars = ((name, info) :: scope) :: rest }

let lookup_var_opt (env : env) (name : string) : ty option =
  let rec search = function
    | [] -> None
    | scope :: rest -> (
        match List.assoc_opt name scope with
        | Some info ->
            info.used := true;
            Some info.ty
        | None -> search rest)
  in
  search env.vars

let lookup_var (env : env) (name : string) : ty =
  match lookup_var_opt env name with
  | Some t -> t
  | None -> (
      (* fall back to function table so function names can be used as values *)
      match Hashtbl.find_opt env.funcs name with
      | Some sg -> TFunc (sg.param_tys, sg.ret_ty)
      | None ->
          add_error env ("undefined variable '" ^ name ^ "'");
          TInt I32)

let lookup_func (env : env) (name : string) : func_sig =
  match Hashtbl.find_opt env.funcs name with
  | Some s -> s
  | None ->
      add_error env ("undefined function '" ^ name ^ "'");
      { param_tys = []; ret_ty = TVoid }

let lookup_struct (env : env) (name : string) : struct_info =
  match Hashtbl.find_opt env.structs name with
  | Some s -> s
  | None ->
      add_error env ("undefined struct '" ^ name ^ "'");
      { field_tys = [] }

let rec ty_of_ast (env : env) (t : typ) : ty =
  match t.tdesc with
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
      else (
        env.current_span := t.span;
        add_error env ("undefined type '" ^ name ^ "'");
        TInt I32)
  | Pointer t -> TPointer (ty_of_ast env t)
  | FuncPtr (ps, ret) ->
      let pts = List.map (ty_of_ast env) ps in
      let rt = match ret with Some t -> ty_of_ast env t | None -> TVoid in
      TFunc (pts, rt)

(* Exact equality but NULL is compatible with any pointer *)
(* TODO(b8e1): Is **i32 compatible with **null? TInt I8 with a TInt I32 (without cast)? *)
(* TODO(94c1): Add rawptr/void* *)
let rec compatible (want : ty) (got : ty) : bool =
  (* Printf.printf "Comparing %s with %s\n" (show_ty want) (show_ty got); *)
  match (want, got) with
  | TPointer _, TNull -> true
  | TPointer a, TPointer b -> compatible a b
  | TFunc (p1, r1), TFunc (p2, r2) ->
      List.length p1 = List.length p2
      && List.for_all2 compatible p1 p2
      && compatible r1 r2
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

(* TODO(d1ec): Support forward reference between structs *)
(* This will fail if Struct A has a field of type Struct B and B is defined after A *)
(* FIXME: Add DFS cycle detection to prevent infinite recursion*)
let collect_struct (env : env) (sd : struct_def) : unit =
  if Hashtbl.mem env.structs sd.name then (
    env.current_span := sd.span;
    add_error env ("'" ^ sd.name ^ "' is already defined"))
  else
    (* TODO(9b1e): Add a rawptr/voidptr keyword for untyped pointers (C's void pointer) *)
    let field_tys =
      List.map (fun (f : field) -> (f.name, ty_of_ast env f.typ)) sd.fields
    in
    Hashtbl.replace env.structs sd.name { field_tys }

let collect_decl (env : env) (decl : decl) : unit =
  match decl with
  | Struct sd -> collect_struct env sd
  | Func fd | Extern fd -> collect_func env fd

(* Second pass doing the bidirectional type checking *)

let is_lvalue (te : Typed_ast.texpr) : bool =
  match te with
  | TIdent _ -> true
  | TUnOp (Deref, _, _) -> true
  | TFieldAccess _ -> true
  | _ -> false

let is_numeric = function TInt _ | TFloat _ -> true | _ -> false
let is_ordered = function TInt _ | TFloat _ -> true | _ -> false
let is_integer = function TInt _ -> true | _ -> false

(* Figure out the type*)
let rec synth (env : env) (e : expr) : Typed_ast.texpr =
  env.current_span := e.span;
  match e.desc with
  | Int n ->
      (* Printf.printf "int %d\n" n; *)
      Typed_ast.TInt (n, TInt I32)
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
  | Call (name, args) -> (
      match lookup_var_opt env name with
      | Some (TFunc (param_tys, ret_ty)) ->
          let sig_ = { param_tys; ret_ty } in
          let targs = check_args env sig_ args in
          Typed_ast.TCall (name, targs, ret_ty)
      | Some _ ->
          add_error env ("'" ^ name ^ "' is not callable");
          dummy_texpr
      | None ->
          let sig_ = lookup_func env name in
          let targs = check_args env sig_ args in
          Typed_ast.TCall (name, targs, sig_.ret_ty))
  | BinOp (op, l, r) -> synth_binop env op l r
  | UnOp (op, e) -> synth_unop env op e
  | FieldAccess (inner_e, fname) -> synth_field env inner_e fname
  | Cast (e, t) ->
      let te = synth env e in
      let ty = ty_of_ast env t in
      Typed_ast.TCast (te, ty)
  | SizeOf t -> Typed_ast.TSizeOf (ty_of_ast env t)
  | Range (a, b) ->
      let ta = synth env a in
      let tb = synth env b in
      Typed_ast.TRange (ta, tb)
  | RangeInclusive (a, b) ->
      let ta = synth env a in
      let tb = synth env b in
      Typed_ast.TRangeInclusive (ta, tb)
  | InterpString parts ->
      let tparts =
        List.map
          (fun (p : interp_part) ->
            match p with
            | Lit s -> Typed_ast.TLit s
            | Interp e ->
                let te = synth env e in
                Typed_ast.TInterp te)
          parts
      in
      Typed_ast.TInterpString tparts
(* | _ -> failwith ("Expression not yet implemented: " ^ show_expr e) *)

(* MUST be this type *)
and check (env : env) (e : expr) (want : ty) : Typed_ast.texpr =
  env.current_span := e.span;
  match e.desc with
  | Int n -> (
      (* TODO(0ab1): Validate n fits within want (e.g. reject 300 into u8). Also, inferred literals still default to I32 large values overflow. *)
      match want with
      | TInt _ -> Typed_ast.TInt (n, want)
      (* want is not an integer type at all e.g. let y: bool = 20 *)
      | _ ->
          add_error env
            (Printf.sprintf "expected %s but found i32" (show_ty want));
          Typed_ast.TInt (n, TInt I32))
  | Float f -> (
      match want with
      | TFloat _ -> Typed_ast.TFloat (f, want)
      | _ ->
          add_error env
            (Printf.sprintf "expected %s but found f64" (show_ty want));
          Typed_ast.TFloat (f, TFloat F64))
  (* not a flexible literal, just check it matches want *)
  | _ ->
      let te = synth env e in
      let got = Typed_ast.ty_of_texpr te in
      if not (compatible want got) then
        add_error env
          (Printf.sprintf "expected %s but found %s" (show_ty want)
             (show_ty got));
      te

and check_args (env : env) (sig_ : func_sig) (args : expr list) :
    Typed_ast.texpr list =
  (* TODO(5038): Support for variadic functions *)
  let n_params = List.length sig_.param_tys in
  let n_args = List.length args in
  if n_params <> n_args then (
    add_error env
      (Printf.sprintf "expected %d arguments but got %d" n_params n_args);
    [])
  else List.map2 (fun e want -> check env e want) args sig_.param_tys

and synth_binop (env : env) (op : binop) (l : expr) (r : expr) : Typed_ast.texpr
    =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      if not (is_numeric t) then
        add_error env
          (Printf.sprintf "cannot apply '%s' to type '%s'" (show_binop_sym op)
             (show_ty t));
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, t)
  | Eq | Neq ->
      (* TODO(b5ca): dedicated "cannot chain comparison operators" message by checking if l is a comparison node *)
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, TBool)
  | Lt | Gt | Lte | Gte ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      if not (is_ordered t) then
        add_error env
          (Printf.sprintf "cannot apply '%s' to type '%s'" (show_binop_sym op)
             (show_ty t));
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, TBool)
  | And | Or ->
      let tl = check env l TBool in
      let tr = check env r TBool in
      Typed_ast.TBinOp (op, tl, tr, TBool)
  | BitAnd | BitOr | BitXor | Lshift | Rshift ->
      let tl = synth env l in
      let t = Typed_ast.ty_of_texpr tl in
      if not (is_integer t) then
        add_error env
          (Printf.sprintf "cannot apply '%s' to type '%s'" (show_binop_sym op)
             (show_ty t));
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, t)
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign ->
      let tl = synth env l in
      if not (is_lvalue tl) then
        add_error env "cannot assign to this expression";
      let t = Typed_ast.ty_of_texpr tl in
      let tr = check env r t in
      Typed_ast.TBinOp (op, tl, tr, t)
(* | _ -> failwith ("Operator not yet implemented: " ^ show_binop op) *)

and synth_unop (env : env) (op : unop) (e : expr) : Typed_ast.texpr =
  match op with
  | Neg ->
      let te = synth env e in
      let t = Typed_ast.ty_of_texpr te in
      if not (is_numeric t) then
        add_error env
          (Printf.sprintf "cannot apply '-' to type '%s'" (show_ty t));
      Typed_ast.TUnOp (op, te, t)
  | Not ->
      let te = check env e TBool in
      Typed_ast.TUnOp (op, te, TBool)
  | BitNot ->
      let te = synth env e in
      let t = Typed_ast.ty_of_texpr te in
      if not (is_integer t) then
        add_error env
          (Printf.sprintf "cannot apply '~' to type '%s'" (show_ty t));
      Typed_ast.TUnOp (op, te, t)
  | Deref -> (
      let te = synth env e in
      match Typed_ast.ty_of_texpr te with
      | TPointer inner -> Typed_ast.TUnOp (op, te, inner)
      | t ->
          add_error env
            ("cannot dereference non-pointer type '" ^ show_ty t ^ "'");
          dummy_texpr)
  | AddressOf ->
      let te = synth env e in
      Typed_ast.TUnOp (op, te, TPointer (Typed_ast.ty_of_texpr te))

(* Synthesize the type of a field access expression. *)
and synth_field (env : env) (e : expr) (fname : string) : Typed_ast.texpr =
  let te = synth env e in
  let rec peel = function
    | TStruct sname -> Some sname
    | TPointer t -> peel t
    | _ -> None
  in
  let ty = Typed_ast.ty_of_texpr te in
  match peel ty with
  | None ->
      add_error env ("type '" ^ show_ty ty ^ "' has no fields");
      dummy_texpr
  | Some sname -> (
      let info = lookup_struct env sname in
      match List.assoc_opt fname info.field_tys with
      | Some ft -> Typed_ast.TFieldAccess (te, fname, ft)
      | None ->
          add_error env ("'" ^ sname ^ "' has no field '" ^ fname ^ "'");
          dummy_texpr)

(* TODO(ccf6): Validate that the operand is a numeric type *)
(* TODO(b5ae): Better error messages *)
let rec check_stmt (env : env) (s : stmt) : env * Typed_ast.tstmt =
  env.current_span := s.span;
  match s.sdesc with
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
        match (ann, e) with
        | Some a, Some e ->
            let want = ty_of_ast env a in
            let te = check env e want in
            (want, te)
        | None, Some e ->
            let te = synth env e in
            (Typed_ast.ty_of_texpr te, te)
        | Some _, None -> failwith "zero-init var not yet implemented"
        | None, None ->
            add_error env (Printf.sprintf "cannot infer type of '%s'" name);
            (TInt I32, dummy_texpr)
      in
      (extend_var env name t, Typed_ast.TVar (name, t, te))
  | Return None ->
      if env.ret_ty <> TVoid then
        add_error env "empty return in non-void function";
      (env, Typed_ast.TReturn None)
  | Return (Some e) ->
      let te = check env e env.ret_ty in
      (env, Typed_ast.TReturn (Some te))
  | Expr e ->
      let te = synth env e in
      (env, Typed_ast.TExpr te)
  | If (branches, else_body) ->
      let tbranches =
        List.map
          (fun (cond, body) ->
            let tc = check env cond TBool in
            let inner = push_scope env in
            let final_inner, tbody = check_stmts inner body in
            pop_scope final_inner;
            (tc, tbody))
          branches
      in
      let inner = push_scope env in
      let final_inner, telse = check_stmts inner else_body in
      pop_scope final_inner;
      (env, Typed_ast.TIf (tbranches, telse))
  | While (cond, body) ->
      let tc = check env cond TBool in
      let inner = push_scope { env with in_loop = true } in
      let final_inner, tbody = check_stmts inner body in
      pop_scope final_inner;
      (env, Typed_ast.TWhile (tc, tbody))
  | For (name, iter, body) ->
      (* TODO(4994): iter must be a range bind name to its element type *)
      let titer = synth env iter in
      let elem_ty = TInt I32 in
      let inner = push_scope { env with in_loop = true } in
      let inner = extend_var inner name elem_ty in
      let final_inner, tbody = check_stmts inner body in
      pop_scope final_inner;
      (env, Typed_ast.TFor (name, elem_ty, titer, tbody))
  | Break ->
      if not env.in_loop then add_error env "break outside loop";
      (env, Typed_ast.TBreak)
  | Continue ->
      if not env.in_loop then add_error env "continue outside loop";
      (env, Typed_ast.TContinue)
  | Block stmts ->
      let inner = push_scope env in
      let final_inner, tstmts = check_stmts inner stmts in
      pop_scope final_inner;
      (env, Typed_ast.TBlock tstmts)

(* TODO(0c77): push/pop scope for match arms when match is implemented *)
(* | _ -> failwith ("Statement not yet implemented: " ^ show_stmt s) *)

(* Performance critical since this pass walks every statement *)
and check_stmts (env : env) (stmts : stmt list) : env * Typed_ast.tstmt list =
  let final_env, tstmts_reversed, _, _ =
    List.fold_left
      (fun (current_env, acc, returned, warned) (s : stmt) ->
        if returned && not warned then (
          current_env.current_span := s.span;
          add_warning current_env "unreachable code");
        let next_env, ts = check_stmt current_env s in
        (* Slow: e', acc @ [ ts ] this is O(n^2) I think with append*)
        let is_return = match s.sdesc with Return _ -> true | _ -> false in
        (next_env, ts :: acc, returned || is_return, warned || returned))
      (env, [], false, false) stmts
  in
  (final_env, List.rev tstmts_reversed)

let check_func (env : env) (fd : func_def) : Typed_ast.tfunc_def =
  let params =
    List.map (fun (p : param) -> (p.name, ty_of_ast env p.typ)) fd.params
  in

  let ret_ty =
    match fd.ret with
    | Some t -> ty_of_ast env t
    | None -> if fd.name = "main" then TInt I32 else TVoid
  in

  let func_env = push_scope { env with ret_ty } in
  (* params pre-marked used so they don't warn *)
  let param_env =
    List.fold_left
      (fun e (name, t) -> extend_var ~used:true e name t)
      func_env params
  in

  (* TODO(932a): check all paths return for non-void functions *)
  let final_env, tbody = check_stmts param_env fd.body in
  pop_scope final_env;

  {
    Typed_ast.name = fd.name;
    params;
    ret_ty;
    body = tbody;
    modifiers = fd.modifiers;
  }

let check_decl (env : env) (decl : decl) : Typed_ast.tdecl =
  match decl with
  | Func fd ->
      let tfd = check_func env fd in
      Typed_ast.TFunc tfd
  | Extern fd ->
      let tfd = check_func env fd in
      Typed_ast.TExtern tfd
  | Struct sd ->
      env.current_span := sd.span;
      let info = lookup_struct env sd.name in
      Typed_ast.TStruct (sd.name, info.field_tys, sd.modifiers)
(* | _ -> failwith "Declaration not supported yet" *)
(* TODO(880b): I need to think about global variables *)

let typecheck (filename : string) (src : string) (decls : decl list) :
    Typed_ast.tdecl list =
  let sm = Source_map.create src in
  let env = make_env filename sm in
  List.iter (collect_decl env) decls;
  let tdecls = List.map (check_decl env) decls in
  List.iter (fun w -> Printf.eprintf "%s\n%!" w) (List.rev !(env.warnings));
  match List.rev !(env.errors) with
  | [] -> tdecls
  | errors -> raise (TypeErrors errors)
