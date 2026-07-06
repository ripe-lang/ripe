(* SPDX-License-Identifier: GPL-2.0-only *)

(* Bidirectional type checker *)

open Ast
open Types
module T = Typed_ast

(* TODO(0d41): I should be allowed to shadow function name with a variable but not
with another function in the same scope. (same with structs) *)

(* lvalue - has a presis address in memory e.g. variable,s array elements, struct fields, etc *)
(* rvalue - temp value that doesn't have presis memory e.g literals, result of math, etc *)

type func_sig = { param_tys : ty list; ret_ty : ty; variadic : bool }
type struct_info = { field_tys : (string * ty) list }
type var_info = { ty : ty; used : bool ref; span : Ast.span }

type env = {
  vars : (string * var_info) list list;
  funcs : (string, func_sig) Hashtbl.t;
  structs : (string, struct_info) Hashtbl.t;
  globals : (string, ty * bool) Hashtbl.t;
  aliases : (string, ty) Hashtbl.t;
  newtypes : (string, ty) Hashtbl.t;
  ret_ty : ty;
  in_loop : bool;
  diags : Diagnostic.sink;
}

let make_env () : env =
  {
    vars = [];
    funcs = Hashtbl.create 16;
    structs = Hashtbl.create 16;
    globals = Hashtbl.create 16;
    aliases = Hashtbl.create 8;
    newtypes = Hashtbl.create 8;
    ret_ty = TVoid;
    in_loop = false;
    diags = Diagnostic.sink ();
  }

let emit (env : env) (d : Diagnostic.t) : unit = Diagnostic.emit env.diags d
let add_error (env : env) span msg = Diagnostic.error_at env.diags span msg
let dummy_texpr = T.mk (TInt I32) (T.TInt 0)
let add_warning (env : env) span msg = Diagnostic.warn_at env.diags span msg
let push_scope (env : env) : env = { env with vars = [] :: env.vars }

let warn_unused_in_scope (env : env) : unit =
  match env.vars with
  | [] -> ()
  | scope :: _ ->
      List.iter
        (fun (name, info) ->
          (* variables prefixed with '_' suppress unused warnings *)
          if (not !(info.used)) && name.[0] <> '_' then
            emit env
              (Diagnostic.warning (Printf.sprintf "unused variable: %s" name)
              |> Diagnostic.at info.span
              |> Diagnostic.help
                   (Printf.sprintf "prefix with an underscore: _%s" name)))
        scope

let extend_var ?(used = false) (env : env) (span : Ast.span) (name : string)
    (t : ty) : env =
  let info = { ty = t; used = ref used; span } in
  match env.vars with
  | [] -> assert false (* no active scope *)
  | scope :: rest -> { env with vars = ((name, info) :: scope) :: rest }

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

let lookup_var (env : env) (span : Ast.span) (name : string) : ty =
  match lookup_var_opt env name with
  | Some t -> t
  | None -> (
      (* locals shadow globals shadow functions *)
      match Hashtbl.find_opt env.globals name with
      | Some (t, _) -> t
      | None -> (
          (* fall back to function table so function names can be used as values *)
          match Hashtbl.find_opt env.funcs name with
          | Some sg -> TFunc (sg.param_tys, sg.ret_ty)
          | None ->
              emit env (Error.undefined_name span "variable" name);
              TInt I32))

let lookup_func (env : env) (span : Ast.span) (name : string) : func_sig =
  match Hashtbl.find_opt env.funcs name with
  | Some s -> s
  | None ->
      emit env (Error.undefined_name span "function" name);
      { param_tys = []; ret_ty = TVoid; variadic = false }

let is_const_global (env : env) (name : string) : bool =
  match Hashtbl.find_opt env.globals name with
  | Some (_, true) -> true
  | _ -> false

let lookup_struct (env : env) (span : Ast.span) (name : string) : struct_info =
  match Hashtbl.find_opt env.structs name with
  | Some s -> s
  | None ->
      emit env (Error.undefined_name span "struct" name);
      { field_tys = [] }

let rec ty_of_ast (env : env) (t : typ) : ty =
  match t.tdesc with
  | Named name -> (
      match List.assoc_opt name builtin_tys with
      | Some bt -> bt
      | None -> (
          if Hashtbl.mem env.structs name then TStruct name
          else
            match Hashtbl.find_opt env.newtypes name with
            | Some base -> TNewtype (name, base)
            | None -> (
                match Hashtbl.find_opt env.aliases name with
                | Some aliased -> aliased
                | None ->
                    emit env (Error.undefined_name t.span "type" name);
                    TInt I32)))
  | Pointer t -> TPointer (ty_of_ast env t)
  | Array (n, t) -> TArray (ty_of_ast env t, n)
  | Slice t -> TSlice (ty_of_ast env t)
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
  | TCStr, TPointer (TInt I8) | TPointer (TInt I8), TCStr -> true
  | TPointer a, TPointer b -> compatible a b
  (* a fixed array coerces to a slice of the same element type *)
  | TSlice a, TArray (b, _) -> compatible a b
  | TSlice a, TSlice b -> compatible a b
  | TFunc (p1, r1), TFunc (p2, r2) ->
      List.length p1 = List.length p2
      && List.for_all2 compatible p1 p2
      && compatible r1 r2
  | _ -> want = got

(* main implicitly returns i32 for the C runtime and everything else is void *)
let ret_ty_of (env : env) (fd : func_def) : ty =
  match fd.ret with
  | Some t -> ty_of_ast env t
  | None -> if fd.name = "main" then TInt I32 else TVoid

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

  let ret_ty = ret_ty_of env fd in
  (* Printf.printf "returns: %s\n%!" (show_ty ret_ty); *)

  (* FIXME(79e6): Check for duplicate function/extern definitions. Need to fix
     how extern foo() and a local foo() with the same name *)
  (* if Hashtbl.mem env.funcs fd.name then
    raise (TypeError ("function already defined: " ^ fd.name)) *)

  Hashtbl.replace env.funcs fd.name
    { param_tys; ret_ty; variadic = fd.variadic }

(* TODO(d1ec): Support forward reference between structs *)
(* This will fail if Struct A has a field of type Struct B and B is defined after A *)
(* FIXME(5b12): Add DFS cycle detection to prevent infinite recursion*)
let collect_struct (env : env) (sd : struct_def) : unit =
  if Hashtbl.mem env.structs sd.name then
    emit env (Error.named sd.span "already defined" sd.name)
  else
    (* TODO(9b1e): Add a rawptr/voidptr keyword for untyped pointers (C's void pointer) *)
    let field_tys =
      List.map (fun (f : field) -> (f.name, ty_of_ast env f.typ)) sd.fields
    in
    let seen = Hashtbl.create 8 in
    List.iter
      (fun (f : field) ->
        if Hashtbl.mem seen f.name then
          emit env (Error.named f.span "duplicate field" f.name)
        else Hashtbl.add seen f.name ())
      sd.fields;
    Hashtbl.replace env.structs sd.name { field_tys }

let collect_alias (env : env) (td : type_alias_def) : unit =
  if Hashtbl.mem env.aliases td.name then
    emit env (Error.named td.span "already defined" td.name)
  else
    let t = ty_of_ast env td.typ in
    Hashtbl.replace env.aliases td.name t

let collect_newtype (env : env) (td : type_alias_def) : unit =
  if Hashtbl.mem env.newtypes td.name then
    emit env (Error.named td.span "already defined" td.name)
  else
    let t = ty_of_ast env td.typ in
    Hashtbl.replace env.newtypes td.name t

let collect_global (env : env) (gd : global_def) : unit =
  if gd.is_const && gd.init = None then
    emit env (Error.named gd.span "const without initializer" gd.name);
  if Hashtbl.mem env.globals gd.name || Hashtbl.mem env.funcs gd.name then
    emit env (Error.named gd.span "already defined" gd.name);
  let t = ty_of_ast env gd.typ in
  Hashtbl.replace env.globals gd.name (t, gd.is_const)

let collect_decl (env : env) (decl : decl) : unit =
  match decl with
  | Struct sd -> collect_struct env sd
  | Func fd | Extern fd -> collect_func env fd
  | Global gd -> collect_global env gd
  | TypeAlias td -> collect_alias env td
  | Newtype td -> collect_newtype env td

(* Second pass doing the bidirectional type checking *)

let is_lvalue (te : T.texpr) : bool =
  match te.T.desc with
  | TIdent _ -> true
  | TUnOp (Deref, _) -> true
  | TFieldAccess _ -> true
  | TIndex _ -> true
  | _ -> false

let is_numeric = function TInt _ | TFloat _ -> true | _ -> false
let is_ordered = is_numeric
let is_integer = function TInt _ -> true | _ -> false
let is_int_literal (e : expr) = match e.desc with Int _ -> true | _ -> false

(* Figure out the type*)
(* stamp the source span here so the mk sites underneath stay span free *)
let rec synth (env : env) (e : expr) : T.texpr =
  { (synth_desc env e) with T.span = e.span }

and synth_desc (env : env) (e : expr) : T.texpr =
  match e.desc with
  | Int n ->
      (* Printf.printf "int %d\n" n; *)
      T.mk (TInt I32) (T.TInt n)
  | Float f -> T.mk (TFloat F64) (T.TFloat f)
  | Bool b ->
      (* Printf.printf "bool %b\n" b; *)
      T.mk TBool (T.TBool b)
  | Null ->
      (* print_endline "null"; *)
      T.mk TNull T.TNull
  | String s ->
      (* Printf.printf "string \"%s\"\n" s; *)
      T.mk (TPointer (TInt I8)) (T.TCStr s)
  | Char c ->
      (* Printf.printf "char: '%c'\n" c; *)
      T.mk (TInt I32) (T.TChar c)
  | Ident name ->
      let t = lookup_var env e.span name in
      (* Printf.printf "ident: `%s` (found type: %s)\n" name (show_ty t); *)
      T.mk t (T.TIdent name)
  | Call (name, args) -> (
      match lookup_var_opt env name with
      | Some (TFunc (param_tys, ret_ty)) ->
          let sig_ = { param_tys; ret_ty; variadic = false } in
          let targs = check_args env e.span sig_ args in
          T.mk ret_ty (T.TCall (name, targs, None))
      | Some _ ->
          emit env (Error.named e.span "not callable" name);
          dummy_texpr
      | None ->
          let sig_ = lookup_func env e.span name in
          let targs = check_args env e.span sig_ args in
          let fixed_count =
            if sig_.variadic then Some (List.length sig_.param_tys) else None
          in
          T.mk sig_.ret_ty (T.TCall (name, targs, fixed_count)))
  | BinOp (op, l, r) -> synth_binop env op l r
  | UnOp (op, e) -> synth_unop env op e
  | FieldAccess (inner_e, fname) -> synth_field env e.span inner_e fname
  | Cast (e, t) ->
      let te = synth env e in
      let ty = ty_of_ast env t in
      T.mk ty (T.TCast te)
  | SizeOf t -> T.mk (TInt I64) (T.TSizeOf (ty_of_ast env t))
  (* ranges are not first-class values and only work as for-loop iterators or slice bounds *)
  | Range _ | RangeInclusive _ ->
      add_error env e.span "range is only valid in a for loop or slice";
      dummy_texpr
  | ArrayLit [] ->
      add_error env e.span "cannot infer type of empty array literal";
      dummy_texpr
  | ArrayLit (e0 :: rest) ->
      let te0 = synth env e0 in
      let elem = te0.T.ty in
      let tes = te0 :: List.map (fun e -> check env e elem) rest in
      T.mk (TArray (elem, List.length tes)) (T.TArrayLit tes)
  | Index (base, idx) -> (
      let tbase = synth env base in
      match tbase.T.ty with
      | TArray (elem, _) | TSlice elem -> (
          match idx.desc with
          (* arr[lo..hi] produces a slice that borrows into the same storage;
             arr[lo..=hi] desugars to arr[lo..hi+1] *)
          | Range (lo, hi) | RangeInclusive (lo, hi) ->
              let inclusive =
                match idx.desc with RangeInclusive _ -> true | _ -> false
              in
              let tlo, thi, lt = check_range_bounds env lo hi in
              let thi =
                if inclusive then
                  T.mk lt (T.TBinOp (Ast.Add, thi, T.mk lt (T.TInt 1)))
                else thi
              in
              T.mk (TSlice elem) (T.TSliceExpr (tbase, tlo, thi))
          | _ ->
              let tidx = synth env idx in
              if not (is_integer tidx.T.ty) then
                add_error env idx.span "array index must be an integer";
              T.mk elem (T.TIndex (tbase, tidx)))
      | t ->
          emit env (Error.named e.span "cannot index type" (show_ty t));
          dummy_texpr)
  | InterpString [] -> T.mk (TPointer (TInt I8)) (T.TCStr "")
  | InterpString [ Lit s ] -> T.mk (TPointer (TInt I8)) (T.TCStr s)
  | InterpString parts ->
      let tparts =
        List.map
          (fun (p : interp_part) ->
            match p with
            | Lit s -> T.TLit s
            | Interp e ->
                let te = synth env e in
                T.TInterp te)
          parts
      in
      T.mk (TPointer (TInt I8)) (T.TInterpString tparts)
  | Undefined ->
      add_error env e.span "cannot infer type of undefined";
      dummy_texpr
  | StructLit (name, name_span, inits) ->
      if not (Hashtbl.mem env.structs name) then (
        emit env (Error.undefined_name name_span "struct" name);
        dummy_texpr)
      else
        let info = lookup_struct env name_span name in
        let seen = Hashtbl.create 4 in
        List.iter
          (fun (fname, fspan, _) ->
            if not (List.mem_assoc fname info.field_tys) then
              emit env (Error.named fspan "no field" fname)
            else if Hashtbl.mem seen fname then
              emit env (Error.named fspan "duplicate field" fname)
            else Hashtbl.replace seen fname ())
          inits;
        (* omitted fields are zero-initialized *)
        let tfields =
          List.map
            (fun (fname, ft) ->
              match
                List.find_map
                  (fun (n, _, e) -> if n = fname then Some e else None)
                  inits
              with
              | Some e -> (fname, check env e ft)
              | None -> (fname, T.mk ft T.TZero))
            info.field_tys
        in
        T.mk (TStruct name) (T.TStructLit (name, tfields))
(* | _ -> failwith ("Expression not yet implemented: " ^ show_expr e) *)

(* MUST be this type *)
and check (env : env) (e : expr) (want : ty) : T.texpr =
  { (check_desc env e want) with T.span = e.span }

and check_desc (env : env) (e : expr) (want : ty) : T.texpr =
  (* synthesize then check the result matches want *)
  let check_by_synth () =
    let te = synth env e in
    let got = te.T.ty in
    if not (compatible want got) then (
      emit env
        (Error.type_mismatch e.span ~expected:(show_ty want)
           ~found:(show_ty got));
      te)
    else
      match (want, got) with
      (* materialize the fat pointer when a fixed array coerces to a slice *)
      | TSlice _, TArray _ -> T.mk want (T.TToSlice te)
      | _ -> te
  in
  match e.desc with
  | Int n -> (
      (* TODO(0ab1): Validate n fits within want (e.g. reject 300 into u8). Also, inferred literals still default to I32 large values overflow. *)
      match want with
      | TInt _ -> T.mk want (T.TInt n)
      (* want is not an integer type at all e.g. let y: bool = 20 *)
      | _ ->
          emit env
            (Error.type_mismatch e.span ~expected:(show_ty want) ~found:"i32");
          T.mk (TInt I32) (T.TInt n))
  | Float f -> (
      match want with
      | TFloat _ -> T.mk want (T.TFloat f)
      | _ ->
          emit env
            (Error.type_mismatch e.span ~expected:(show_ty want) ~found:"f64");
          T.mk (TFloat F64) (T.TFloat f))
  | UnOp (Neg, { desc = Int n; _ }) -> check env { e with desc = Int (-n) } want
  | UnOp (Neg, { desc = Float f; _ }) ->
      check env { e with desc = Float (-.f) } want
  | ArrayLit elems -> (
      match want with
      | TArray (elem, n) ->
          if List.length elems <> n then
            emit env
              (Error.arity e.span
                 ~expected:(Printf.sprintf "expected %d elements" n)
                 ~found:(List.length elems));
          let tes = List.map (fun e -> check env e elem) elems in
          T.mk (TArray (elem, n)) (T.TArrayLit tes)
      | _ -> check_by_synth ())
  | Undefined -> T.mk want T.TUndef
  | _ -> check_by_synth ()

and check_range_bounds (env : env) (lo : expr) (hi : expr) =
  let tlo, thi, t =
    (* lone literal on the left, typed on the right: bend the literal to hi *)
    if is_int_literal lo && not (is_int_literal hi) then
      (* Printf.eprintf "range: branch 1 (bend literal lo to hi)\n"; *)
      let thi = synth env hi in
      let t = thi.T.ty in
      (check env lo t, thi, t)
    (* otherwise anchor on lo and check hi against it (also covers two literals) *)
      else
      (* Printf.eprintf "range: branch 2 (anchor on lo)\n"; *)
      let tlo = synth env lo in
      let t = tlo.T.ty in
      (tlo, check env hi t, t)
  in
  if not (is_integer t) then
    add_error env lo.span "range bounds must be integers";
  (tlo, thi, t)

and check_args (env : env) (span : Ast.span) (sig_ : func_sig)
    (args : expr list) : T.texpr list =
  let n_params = List.length sig_.param_tys in
  let n_args = List.length args in
  if sig_.variadic then
    if n_args < n_params then (
      emit env
        (Error.arity span
           ~expected:(Printf.sprintf "expected at least %d arguments" n_params)
           ~found:n_args);
      [])
    else
      let fixed = List.filteri (fun i _ -> i < n_params) args in
      let rest = List.filteri (fun i _ -> i >= n_params) args in
      List.map2 (fun e want -> check env e want) fixed sig_.param_tys
      @ List.map (synth env) rest
  else if n_params <> n_args then (
    emit env
      (Error.arity span
         ~expected:(Printf.sprintf "expected %d arguments" n_params)
         ~found:n_args);
    [])
  else List.map2 (fun e want -> check env e want) args sig_.param_tys

and synth_binop (env : env) (op : binop) (l : expr) (r : expr) : T.texpr =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      let tl = synth env l in
      let t = tl.T.ty in
      if not (is_numeric t) then
        add_error env l.span
          (Printf.sprintf "cannot apply `%s` to %s" (show_binop_sym op)
             (show_ty t));
      let tr = check env r t in
      T.mk t (T.TBinOp (op, tl, tr))
  | Eq | Neq ->
      (* TODO(b5ca): dedicated "cannot chain comparison operators" message by checking if l is a comparison node *)
      let tl = synth env l in
      let t = tl.T.ty in
      let tr = check env r t in
      T.mk TBool (T.TBinOp (op, tl, tr))
  | Lt | Gt | Lte | Gte ->
      let tl = synth env l in
      let t = tl.T.ty in
      if not (is_ordered t) then
        add_error env l.span
          (Printf.sprintf "cannot apply `%s` to %s" (show_binop_sym op)
             (show_ty t));
      let tr = check env r t in
      T.mk TBool (T.TBinOp (op, tl, tr))
  | And | Or ->
      let tl = check env l TBool in
      let tr = check env r TBool in
      T.mk TBool (T.TBinOp (op, tl, tr))
  | BitAnd | BitOr | BitXor | Lshift | Rshift ->
      let tl = synth env l in
      let t = tl.T.ty in
      if not (is_integer t) then
        add_error env l.span
          (Printf.sprintf "cannot apply `%s` to %s" (show_binop_sym op)
             (show_ty t));
      let tr = check env r t in
      T.mk t (T.TBinOp (op, tl, tr))
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign ->
      let tl = synth env l in
      if not (is_lvalue tl) then
        add_error env l.span "cannot assign to expression";
      (* reject assignment to a const global *)
      (match tl.T.desc with
      | TIdent name when is_const_global env name ->
          emit env (Error.named l.span "cannot assign to const" name)
      | _ -> ());
      let t = tl.T.ty in
      let tr = check env r t in
      T.mk t (T.TBinOp (op, tl, tr))
(* | _ -> failwith ("Operator not yet implemented: " ^ show_binop op) *)

and synth_unop (env : env) (op : unop) (e : expr) : T.texpr =
  match op with
  | Neg ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_numeric t) then
        add_error env e.span
          (Printf.sprintf "cannot apply `-` to %s" (show_ty t));
      T.mk t (T.TUnOp (op, te))
  | Not ->
      let te = check env e TBool in
      T.mk TBool (T.TUnOp (op, te))
  | BitNot ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_integer t) then
        add_error env e.span
          (Printf.sprintf "cannot apply `~` to %s" (show_ty t));
      T.mk t (T.TUnOp (op, te))
  | Deref -> (
      let te = synth env e in
      match te.T.ty with
      | TPointer inner -> T.mk inner (T.TUnOp (op, te))
      | t ->
          emit env (Error.named e.span "cannot dereference type" (show_ty t));
          dummy_texpr)
  | AddressOf ->
      let te = synth env e in
      if not (is_lvalue te) then
        add_error env e.span "cannot take address of expression";
      T.mk (TPointer te.T.ty) (T.TUnOp (op, te))

(* Synthesize the type of a field access expression. *)
and synth_field (env : env) (span : Ast.span) (e : expr) (fname : string) :
    T.texpr =
  let te = synth env e in
  let ty = te.T.ty in
  match ty with
  | TArray (elem, _) | TSlice elem -> (
      match fname with
      | "len" -> T.mk (TInt Usize) (T.TLen te)
      | "ptr" -> T.mk (TPointer elem) (T.TDataPtr te)
      | _ ->
          emit env
            (Error.named span "no field" fname
            |> Diagnostic.label (Printf.sprintf "on %s" (show_ty ty)));
          dummy_texpr)
  | _ -> synth_struct_field env span te ty fname

and synth_struct_field (env : env) (span : Ast.span) (te : T.texpr) (ty : ty)
    (fname : string) : T.texpr =
  let rec peel = function
    | TStruct sname -> Some sname
    | TPointer t -> peel t
    | _ -> None
  in
  match peel ty with
  | None ->
      emit env (Error.named span "type has no fields" (show_ty ty));
      dummy_texpr
  | Some sname -> (
      let info = lookup_struct env span sname in
      match List.assoc_opt fname info.field_tys with
      | Some ft -> T.mk ft (T.TFieldAccess (te, fname))
      | None ->
          emit env
            (Error.named span "no field" fname
            |> Diagnostic.label (Printf.sprintf "on struct %s" sname));
          dummy_texpr)

(* TODO(ccf6): Validate that the operand is a numeric type *)
(* TODO(b5ae): Better error messages *)
let rec check_stmt (env : env) (s : stmt) : env * T.tstmt =
  let env', tsdesc = check_stmt_desc env s in
  (env', { T.tsdesc; span = s.span })

and check_stmt_desc (env : env) (s : stmt) : env * T.tstmt_desc =
  match s.sdesc with
  | Const (name, nspan, ann, e) ->
      let t, te =
        match ann with
        | Some a ->
            let want = ty_of_ast env a in
            let te = check env e want in
            (want, te)
        | None ->
            let te = synth env e in
            (te.T.ty, te)
      in
      (extend_var env nspan name t, T.TConst (name, t, te))
  | Var (name, nspan, ann, e) ->
      let t, te =
        match (ann, e) with
        | Some a, Some e ->
            let want = ty_of_ast env a in
            let te = check env e want in
            (want, te)
        | None, Some e ->
            let te = synth env e in
            (te.T.ty, te)
        | Some a, None ->
            let want = ty_of_ast env a in
            (want, T.mk want T.TZero)
        | None, None ->
            emit env (Error.named nspan "cannot infer type" name);
            (TInt I32, dummy_texpr)
      in
      (extend_var env nspan name t, T.TVar (name, t, te))
  | Return None ->
      if env.ret_ty <> TVoid then
        add_error env s.span "empty return in non-void function";
      (env, T.TReturn None)
  | Return (Some e) ->
      let te = check env e env.ret_ty in
      (env, T.TReturn (Some te))
  | Expr e ->
      let te = synth env e in
      (env, T.TExpr te)
  | If (branches, else_body) ->
      let tbranches =
        List.map
          (fun (cond, body) ->
            let tc = check env cond TBool in
            let inner = push_scope env in
            let final_inner, tbody = check_stmts inner body in
            warn_unused_in_scope final_inner;
            (tc, tbody))
          branches
      in
      let inner = push_scope env in
      let final_inner, telse = check_stmts inner else_body in
      warn_unused_in_scope final_inner;
      (env, T.TIf (tbranches, telse))
  | While (cond, body) ->
      let tc = check env cond TBool in
      let inner = push_scope { env with in_loop = true } in
      let final_inner, tbody = check_stmts inner body in
      warn_unused_in_scope final_inner;
      (env, T.TWhile (tc, tbody))
  | For (name, nspan, iter, body) ->
      (* a range binds the loop var to the bound type and an array binds it to the
         element type. ranges are handled here since they are not first-class values *)
      let titer, elem_ty =
        match iter.desc with
        | Range (lo, hi) | RangeInclusive (lo, hi) ->
            let tlo, thi, t = check_range_bounds env lo hi in
            let node =
              match iter.desc with
              | RangeInclusive _ -> T.mk t (T.TRangeInclusive (tlo, thi))
              | _ -> T.mk t (T.TRange (tlo, thi))
            in
            (node, t)
        | _ -> (
            let ti = synth env iter in
            match ti.T.ty with
            | TArray (elem, _) | TSlice elem -> (ti, elem)
            | t ->
                emit env
                  (Error.named iter.span "cannot iterate over type" (show_ty t));
                (ti, TInt I32))
      in
      let inner = push_scope { env with in_loop = true } in
      let inner = extend_var inner nspan name elem_ty in
      let final_inner, tbody = check_stmts inner body in
      warn_unused_in_scope final_inner;
      (env, T.TFor (name, elem_ty, titer, tbody))
  | Break ->
      if not env.in_loop then add_error env s.span "break outside loop";
      (env, T.TBreak)
  | Continue ->
      if not env.in_loop then add_error env s.span "continue outside loop";
      (env, T.TContinue)
  | Block stmts ->
      let inner = push_scope env in
      let final_inner, tstmts = check_stmts inner stmts in
      warn_unused_in_scope final_inner;
      (env, T.TBlock tstmts)

(* TODO(0c77): push/pop scope for match arms when match is implemented *)
(* | _ -> failwith ("Statement not yet implemented: " ^ show_stmt s) *)

(* Performance critical since this pass walks every statement *)
and check_stmts (env : env) (stmts : stmt list) : env * T.tstmt list =
  let final_env, tstmts_reversed, _, _ =
    List.fold_left
      (fun (current_env, acc, returned, warned) (s : stmt) ->
        if returned && not warned then
          add_warning current_env s.span "unreachable code";
        let next_env, ts = check_stmt current_env s in
        (* break and continue end the block just like return does *)
        let terminates =
          match s.sdesc with Return _ | Break | Continue -> true | _ -> false
        in
        (next_env, ts :: acc, returned || terminates, warned || returned))
      (env, [], false, false) stmts
  in
  (final_env, List.rev tstmts_reversed)

(* every path through the stmts ends in a return *)
let rec stmts_return (stmts : stmt list) : bool = List.exists stmt_returns stmts

and stmt_returns (s : stmt) : bool =
  match s.sdesc with
  | Return _ -> true
  | Block body -> stmts_return body
  | If (branches, else_body) ->
      else_body <> [] && stmts_return else_body
      && List.for_all (fun (_, body) -> stmts_return body) branches
  | _ -> false

let check_func ?(is_extern = false) (env : env) (fd : func_def) : T.tfunc_def =
  let params_typed =
    List.map
      (fun (p : param) -> (p.name, ty_of_ast env p.typ, p.span))
      fd.params
  in
  let params = List.map (fun (name, t, _) -> (name, t)) params_typed in

  let ret_ty = ret_ty_of env fd in

  let func_env = push_scope { env with ret_ty } in
  (* params pre-marked used so they don't warn *)
  let param_env =
    List.fold_left
      (fun e (name, t, span) -> extend_var ~used:true e span name t)
      func_env params_typed
  in

  let final_env, tbody = check_stmts param_env fd.body in
  warn_unused_in_scope final_env;

  if
    (not is_extern) && ret_ty <> TVoid && fd.name <> "main"
    && not (stmts_return fd.body)
  then begin
    let span = match fd.ret with Some t -> t.span | None -> fd.span in
    emit env (Error.named span "missing return" fd.name)
  end;

  {
    T.name = fd.name;
    params;
    ret_ty;
    body = tbody;
    modifiers = fd.modifiers;
    variadic = fd.variadic;
  }

let rec is_const_texpr (env : env) (te : T.texpr) : bool =
  match te.T.desc with
  | TInt _ | TFloat _ | TBool _ | TNull | TChar _ | TCStr _ | TSizeOf _ -> true
  | TIdent name -> is_const_global env name
  | TUnOp (_, e) -> is_const_texpr env e
  | TBinOp (_, l, r) -> is_const_texpr env l && is_const_texpr env r
  | TCast e -> is_const_texpr env e
  | TZero -> true
  (* an array literal is constant when all its elements are *)
  | TArrayLit elems -> List.for_all (is_const_texpr env) elems
  | TStructLit (_, fields) ->
      List.for_all (fun (_, fe) -> is_const_texpr env fe) fields
  (* never compile-time by design *)
  | TCall _ | TFieldAccess _ | TRange _ | TRangeInclusive _ | TInterpString _
  | TIndex _ | TLen _ | TToSlice _ | TSliceExpr _ | TDataPtr _ ->
      false
  | TUndef -> true

let check_global (env : env) (gd : global_def) : T.tglobal_def =
  let t = ty_of_ast env gd.typ in
  let tinit =
    match gd.init with
    | None -> None
    | Some { desc = Undefined; _ } -> None
    | Some e ->
        let te = check env e t in
        if not (is_const_texpr env te) then
          emit env (Error.named e.span "initializer must be constant" gd.name);
        Some te
  in
  { T.name = gd.name; ty = t; init = tinit; is_const = gd.is_const }

let check_decl (env : env) (decl : decl) : T.tdecl =
  match decl with
  | Func fd ->
      let tfd = check_func env fd in
      T.TFunc tfd
  | Extern fd ->
      let tfd = check_func ~is_extern:true env fd in
      T.TExtern tfd
  | Struct sd ->
      let info = lookup_struct env sd.span sd.name in
      T.TStruct (sd.name, info.field_tys, sd.modifiers)
  | Global gd -> T.TGlobal (check_global env gd)
  | TypeAlias td ->
      let t = Hashtbl.find env.aliases td.name in
      T.TTypeAlias (td.name, t)
  | Newtype td ->
      let t = Hashtbl.find env.newtypes td.name in
      T.TNewtype (td.name, t)
(* | _ -> failwith "Declaration not supported yet" *)

(* hands warnings back for the edge to render and blows up on any error *)
let typecheck (decls : decl list) : T.tdecl list * Diagnostic.t list =
  let env = make_env () in
  List.iter (collect_decl env) decls;
  let tdecls = List.map (check_decl env) decls in
  let all = Diagnostic.drain env.diags in
  let is_err (d : Diagnostic.t) = d.severity = Diagnostic.Error in
  if List.exists is_err all then raise (Diagnostic.Errors all)
  else (tdecls, List.filter (fun d -> not (is_err d)) all)
