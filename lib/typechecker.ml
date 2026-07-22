(* SPDX-License-Identifier: GPL-2.0-only *)

(* Bidirectional type checker *)

open Ast
open Types
open Ty_pred
module T = Typed_ast

(* lvalue - has a presis address in memory e.g. variable,s array elements, struct fields, etc *)
(* rvalue - temp value that doesn't have presis memory e.g literals, result of math, etc *)

type func_sig = { param_tys : ty list; ret_ty : ty; variadic : bool }
type struct_info = { field_tys : (string * ty) list }

(* structs newtypes and aliases share one namespace of type names *)
type type_def = DStruct of struct_info | DNewtype of ty | DAlias of ty
type var_info = { ty : ty; used : bool ref; span : Ast.span }

(* the typed and value fields only ever go from None to Some so nothing rolls back *)
type gstate = {
  def : global_def;
  mutable typed : T.texpr option;
  mutable value : Const_eval.const_num option;
  (* busy means this global is mid evaluation so a self demand is a cycle *)
  mutable busy : bool;
}

type env = {
  vars : (string * var_info) list list;
  funcs : (string, func_sig) Hashtbl.t;
  types : (string, type_def) Hashtbl.t;
  (* struct field layouts mirror the DStruct entries in types so ty_size need not rebuild them *)
  struct_fields : (string, (string * ty) list) Hashtbl.t;
  globals : (string, ty * Ast.binding_kind) Hashtbl.t;
  (* constants evaluate on demand so an array size may name a later const *)
  g_state : (string, gstate) Hashtbl.t;
  l_vals : (Symbol.id, Const_eval.const_num) Hashtbl.t;
  ret_ty : ty;
  in_loop : bool;
  in_main : bool;
  diags : Diagnostic.sink;
  uses : Resolve.t;
}

let make_env (uses : Resolve.t) : env =
  {
    vars = [];
    funcs = Hashtbl.create 16;
    types = Hashtbl.create 16;
    struct_fields = Hashtbl.create 16;
    globals = Hashtbl.create 16;
    g_state = Hashtbl.create 16;
    l_vals = Hashtbl.create 16;
    ret_ty = TVoid;
    in_loop = false;
    in_main = false;
    diags = Diagnostic.sink ();
    uses;
  }

let dummy_const_num = Const_eval.Ni32 0l
let sym (env : env) (span : Ast.span) : Symbol.t = Resolve.sym_at env.uses span
let emit (env : env) (d : Diagnostic.t) : unit = Diagnostic.emit env.diags d
let add_error (env : env) span msg = Diagnostic.error_at env.diags span msg
let dummy_texpr = T.mk TError (T.TInt 0L)
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

let lookup_func (env : env) (span : Ast.span) (name : string) : func_sig =
  match Hashtbl.find_opt env.funcs name with
  | Some s -> s
  | None ->
      emit env (Error.undefined_name span "function" name);
      { param_tys = []; ret_ty = TVoid; variadic = false }

let is_const_global (env : env) (name : string) : bool =
  match Hashtbl.find_opt env.globals name with
  | Some (_, (Let | Const)) -> true
  | _ -> false

let is_comptime_global (env : env) (name : string) : bool =
  match Hashtbl.find_opt env.globals name with
  | Some (_, Const) -> true
  | _ -> false

let check_const_scalar (env : env) (span : Ast.span) (t : ty) : unit =
  if not (is_scalar t) then
    emit env
      Diagnostic.(
        error ("const must be a scalar, found " ^ show_ty t)
        |> at span
        |> help "use let for values that need storage")

let lookup_struct (env : env) (span : Ast.span) (name : string) : struct_info =
  match Hashtbl.find_opt env.types name with
  | Some (DStruct s) -> s
  | _ ->
      emit env (Error.undefined_name span "struct" name);
      { field_tys = [] }

(* a call to a never function is a terminator so reachability treats it like a return *)
let is_never_call (env : env) (e : expr) : bool =
  match e.desc with
  | Call ({ desc = Ident name; _ }, _) -> (
      match Hashtbl.find_opt env.funcs name with
      | Some s -> s.ret_ty = TNever
      | None -> false)
  | _ -> false

let rec ty_of_ast (env : env) (t : typ) : ty =
  match t.tdesc with
  (* never is the return type of a function that can't return so no value ever has it *)
  | Named "never" ->
      emit env
        Diagnostic.(
          error "never is only valid as a function return type"
          |> at t.span
          |> help "a value of type never cannot exist");
      TError
  | Named "opaque" ->
      emit env
        Diagnostic.(
          error "opaque is only valid as a pointee"
          |> at t.span
          |> help "use *opaque for an untyped pointer");
      TError
  | Named name -> (
      match List.assoc_opt name builtin_tys with
      | Some bt -> bt
      | None -> (
          match Hashtbl.find_opt env.types name with
          | Some (DStruct _) -> TStruct (name, [])
          | Some (DNewtype base) -> TNewtype (name, base)
          | Some (DAlias aliased) -> TAlias (name, aliased)
          | None -> Error.ice ~span:t.span "type name escaped the resolver"))
  | Pointer { tdesc = Named "opaque"; _ } -> TOpaquePtr
  | Pointer t -> TPointer (ty_of_ast env t)
  | Array (e, t) -> TArray (ty_of_ast env t, eval_array_size env e)
  | Slice t -> TSlice (ty_of_ast env t)
  | FuncPtr (ps, ret) ->
      let pts = List.map (ty_of_ast env) ps in
      let rt = match ret with Some t -> ty_of_ast env t | None -> TVoid in
      TFunc (pts, rt)

and lookup_var (env : env) (span : Ast.span) (name : string) : ty =
  match lookup_var_opt env name with
  | Some t -> t
  | None -> (
      (* locals shadow globals shadow functions *)
      match Hashtbl.find_opt env.globals name with
      | Some (t, _) -> t
      | None -> (
          (* an array size may name a global not collected yet so type it now *)
          match Hashtbl.find_opt env.g_state name with
          (* a global whose own size names it would loop forever *)
          | Some st when st.busy ->
              emit env (Error.named span "cyclic constant" name);
              TError
          | Some st ->
              st.busy <- true;
              let t = ty_of_ast env st.def.typ in
              st.busy <- false;
              Hashtbl.replace env.globals name (t, st.def.kind);
              t
          | None -> (
              (* fall back to function table so function names can be used as values *)
              match Hashtbl.find_opt env.funcs name with
              | Some sg -> TFunc (sg.param_tys, sg.ret_ty)
              | None ->
                  emit env (Error.undefined_name span "variable" name);
                  TInt I32)))

(* Second pass doing the bidirectional type checking *)

(* stamp the source span here so the mk sites underneath stay span free *)
and synth (env : env) (e : expr) : T.texpr =
  { (synth_desc env e) with T.span = e.span }

and synth_desc (env : env) (e : expr) : T.texpr =
  match e.desc with
  | Int (n, suf) ->
      let kind = match suf with Some s -> suffix_kind s | None -> I32 in
      if Int64.unsigned_compare n (int_kind_pos_limit kind) > 0 then
        emit env (Error.int_out_of_range e.span ~ty:(show_ty (TInt kind)));
      T.mk (TInt kind) (T.TInt n)
  | UnOp (Neg, { desc = Int (n, Some s); _ }) ->
      let kind = suffix_kind s in
      if Int64.unsigned_compare n (int_kind_neg_limit kind) > 0 then
        emit env (Error.int_out_of_range e.span ~ty:(show_ty (TInt kind)));
      T.mk (TInt kind) (T.TInt (Int64.neg n))
  | Float f -> T.mk (TFloat F64) (T.TFloat f)
  | Bool b -> T.mk TBool (T.TBool b)
  | Null -> T.mk TNull T.TNull
  | String s -> T.mk (TPointer (TInt I8)) (T.TCStr s)
  | Char c -> T.mk (TInt I32) (T.TChar c)
  | Ident name ->
      let t = lookup_var env e.span name in
      T.mk t (T.TIdent (sym env e.span))
  | Call (callee, args) -> synth_call env e.span callee args
  | BinOp (op, l, r) -> synth_binop env op l r
  | UnOp (op, e) -> synth_unop env op e
  | FieldAccess (inner_e, fname) -> synth_field env e.span inner_e fname
  (* TODO this trapping `as!` could be moved to the standard library in the future once sum types and generics land, then `as!` could switch to meaning force the conversion *)
  | Cast (operand, t, checked) ->
      let te = synth env operand in
      let ty = ty_of_ast env t in
      if not (cast_ok te.T.ty ty) then begin
        let d =
          Diagnostic.error "invalid cast"
          |> Diagnostic.at e.span
          |> Diagnostic.label
               (Printf.sprintf "cannot cast %s to %s" (show_ty te.T.ty)
                  (show_ty ty))
        in
        let d =
          if resolve_ty ty = TBool then
            Diagnostic.help "compare with zero instead e.g. `x != 0`" d
          else d
        in
        emit env d
      end
      else if checked then
        begin match (resolve_ty te.T.ty, resolve_ty ty) with
        | TInt _, TInt _ -> ()
        (* TODO(9eb1): let `as!` cover float casts too once I add a loss check for floats *)
        | _ ->
            emit env
              (Diagnostic.error "checked cast only supports integers"
              |> Diagnostic.at e.span
              |> Diagnostic.label "`as!` traps on integer overflow only"
              |> Diagnostic.help "use a plain `as` cast here")
        end;
      T.mk ty (T.TCast (te, checked))
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
  | Index (base, idx) -> synth_index env e.span base idx
  | Undefined ->
      add_error env e.span "cannot infer type of undefined";
      dummy_texpr
  | StructLit (name, name_span, inits) -> (
      match Hashtbl.find_opt env.types name with
      | Some (DStruct info) ->
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
          T.mk (TStruct (name, [])) (T.TStructLit (name, tfields))
      | _ ->
          emit env (Error.undefined_name name_span "struct" name);
          dummy_texpr)
  | BlockExpr (body, e) ->
      let inner = push_scope env in
      let final_inner, tbody = check_stmts inner body in
      let te = synth final_inner e in
      (* the body diverges so control never reaches the block's value *)
      if
        List.exists
          (fun s ->
            Reachability.stmt_returns (is_never_call env) s
            || match s.sdesc with Break | Continue -> true | _ -> false)
          body
      then add_warning env e.span "unreachable code";
      warn_unused_in_scope final_inner;
      T.mk te.T.ty (T.TBlockExpr (tbody, te))

(* MUST be this type *)
and check ?(adopt = false) (env : env) (e : expr) (want : ty) : T.texpr =
  { (check_desc ~adopt env e want) with T.span = e.span }

and check_desc ?(adopt = false) (env : env) (e : expr) (want : ty) : T.texpr =
  let target = if adopt then resolve_ty want else strip_alias want in
  (* synthesize then check the result matches want *)
  let check_by_synth () =
    let te = synth env e in
    let got = te.T.ty in
    if not (compatible want got) then (
      let mismatch =
        Error.type_mismatch e.span ~expected:(show_ty want) ~found:(show_ty got)
      in
      let mismatch =
        match (e.desc, strip_alias want) with
        | BinOp (Assign, _, _), TBool ->
            Diagnostic.help "did you mean `==` to compare?" mismatch
        | _ -> mismatch
      in
      emit env mismatch;
      te)
    else
      match (strip_alias want, strip_alias got) with
      (* materialize the fat pointer when a fixed array coerces to a slice *)
      | TSlice _, TArray _ -> T.mk want (T.TToSlice te)
      | _ -> te
  in
  match e.desc with
  | Int (_, Some s) ->
      (* the suffix already picked the type so a wrong target is an error not a quiet coercion *)
      let te = synth_desc env e in
      if strip_alias want <> te.T.ty then
        emit env
          (Error.type_mismatch e.span ~expected:(show_ty want)
             ~found:(show_ty te.T.ty));
      te
  | Int (n, None) -> (
      (* an untyped literal adopts a newtype over an int and checks its base *)
      match target with
      | TInt kind ->
          if Int64.unsigned_compare n (int_kind_pos_limit kind) > 0 then
            emit env
              (Error.int_out_of_range e.span ~ty:(show_ty (resolve_ty want)));
          T.mk want (T.TInt n)
      | TError -> T.mk want (T.TInt n)
      (* want is not an integer type at all e.g. let y: bool = 20 *)
      | _ ->
          emit env
            (Error.type_mismatch e.span ~expected:(show_ty want) ~found:"i32");
          T.mk (TInt I32) (T.TInt n))
  | Float f -> (
      match target with
      | TFloat _ -> T.mk want (T.TFloat f)
      | TError -> T.mk want (T.TFloat f)
      | _ ->
          emit env
            (Error.type_mismatch e.span ~expected:(show_ty want) ~found:"f64");
          T.mk (TFloat F64) (T.TFloat f))
  | UnOp (Neg, { desc = Int (n, None); _ }) -> (
      match target with
      | TInt kind ->
          if Int64.unsigned_compare n (int_kind_neg_limit kind) > 0 then
            emit env
              (Error.int_out_of_range e.span ~ty:(show_ty (resolve_ty want)));
          T.mk want (T.TInt (Int64.neg n))
      | _ -> check ~adopt env { e with desc = Int (Int64.neg n, None) } want)
  | UnOp (Neg, { desc = Float f; _ }) ->
      check env { e with desc = Float (-.f) } want
  | UnOp (Neg, { desc = Int (_, Some _); _ }) -> check_by_synth ()
  | UnOp (Neg, operand) when is_numeric (strip_alias want) ->
      T.mk want (T.TUnOp (Neg, check env operand want))
  | UnOp (BitNot, operand) when is_integer (strip_alias want) ->
      T.mk want (T.TUnOp (BitNot, check env operand want))
  | ArrayLit elems -> (
      match strip_alias want with
      | TArray (elem, n) ->
          if List.length elems <> n then
            emit env
              (Error.arity e.span
                 ~expected:(Printf.sprintf "expected %d elements" n)
                 ~found:(List.length elems));
          let tes = List.map (fun e -> check env e elem) elems in
          T.mk (TArray (elem, n)) (T.TArrayLit tes)
      | _ -> check_by_synth ())
  | BinOp (((Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor) as op), l, r)
    when (match op with
         | Mod | BitAnd | BitOr | BitXor -> is_integer
         | _ -> is_numeric)
           (strip_alias want) ->
      T.mk want (T.TBinOp (op, check env l want, check env r want))
  | BinOp (((Lshift | Rshift) as op), l, r) when is_integer (strip_alias want)
    ->
      T.mk want (T.TBinOp (op, check env l want, synth env r))
  | Undefined -> T.mk want T.TUndef
  | _ -> check_by_synth ()

and check_range_bounds (env : env) (lo : expr) (hi : expr) =
  let tlo, thi, t =
    (* lone literal on the left, typed on the right: bend the literal to hi *)
    if is_int_literal lo && not (is_int_literal hi) then
      let thi = synth env hi in
      let t = thi.T.ty in
      (check env lo t, thi, t)
      (* otherwise anchor on lo and check hi against it (also covers two literals) *)
    else
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
      (* c reads a float vararg as a double so widen it first *)
      let promote_vararg e =
        let te = synth env e in
        match resolve_ty te.T.ty with
        | TFloat F32 -> T.mk ~span:e.span (TFloat F64) (T.TCast (te, false))
        | _ -> te
      in
      List.map2 (check env) fixed sig_.param_tys @ List.map promote_vararg rest
  else if n_params <> n_args then (
    emit env
      (Error.arity span
         ~expected:(Printf.sprintf "expected %d arguments" n_params)
         ~found:n_args);
    [])
  else List.map2 (check env) args sig_.param_tys

and synth_binop (env : env) (op : binop) (l : expr) (r : expr) : T.texpr =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      let tl = synth env l in
      let t = tl.T.ty in
      if not (is_numeric t) then
        emit env
          (Error.bad_operand l.span ~op:(show_binop_sym op) ~ty:(show_ty t));
      (* qbe has no float remainder instruction *)
      if op = Mod && match strip_alias t with TFloat _ -> true | _ -> false
      then emit env (Error.bad_operand l.span ~op:"%" ~ty:(show_ty t));
      let tr = check env r t in
      T.mk t (T.TBinOp (op, tl, tr))
  | Eq | Neq ->
      (* TODO(b5ca): dedicated "cannot chain comparison operators" message by checking if l is a comparison node *)
      let tl = synth env l in
      let t = if tl.T.ty = TNull then (synth env r).T.ty else tl.T.ty in
      if not (is_comparable t) then
        emit env
          (Error.bad_operand l.span ~op:(show_binop_sym op) ~ty:(show_ty t));
      let tl = if tl.T.ty = TNull then check env l t else tl in
      let tr = check env r t in
      T.mk TBool (T.TBinOp (op, tl, tr))
  | Lt | Gt | Lte | Gte ->
      let tl = synth env l in
      let t = if tl.T.ty = TNull then (synth env r).T.ty else tl.T.ty in
      if not (is_ordered t) then
        emit env
          (Error.bad_operand l.span ~op:(show_binop_sym op) ~ty:(show_ty t));
      let tl = if tl.T.ty = TNull then check env l t else tl in
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
        emit env
          (Error.bad_operand l.span ~op:(show_binop_sym op) ~ty:(show_ty t));
      let tr =
        match op with
        (* the count keeps its own integer type since it is only a number of positions *)
        | Lshift | Rshift ->
            let tr = synth env r in
            if not (is_integer tr.T.ty) then
              add_error env r.span
                (Printf.sprintf "shift count must be an integer, found %s"
                   (show_ty tr.T.ty));
            tr
        | _ -> check env r t
      in
      T.mk t (T.TBinOp (op, tl, tr))
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | BitAndAssign | BitOrAssign | BitXorAssign | LshiftAssign | RshiftAssign ->
      synth_assign env op l r

and synth_assign (env : env) (op : binop) (l : expr) (r : expr) : T.texpr =
  let tl = synth env l in
  if not (is_lvalue tl) then add_error env l.span "cannot assign to expression";
  (match tl.T.desc with
  | TIdent s when Symbol.is_func s.kind ->
      emit env (Error.named l.span "cannot assign to function" s.name)
  | TIdent _ | TFieldAccess _ | TIndex _ -> (
      (* This catches assignment to an immutable binding whether it's local or global. *)
      match root_binding tl with
      | Some s
        when Symbol.is_immutable s.kind
             || (Symbol.is_global s.kind && is_const_global env s.name) ->
          emit env (Error.named l.span "cannot assign to immutable" s.name)
      | _ -> ())
  | _ -> ());
  let t = tl.T.ty in
  let operand_ok =
    match op with
    | Assign -> true
    | AddAssign | SubAssign | MulAssign | DivAssign -> is_numeric t
    (* qbe has no float remainder instruction *)
    | ModAssign ->
        is_numeric t
        && not (match strip_alias t with TFloat _ -> true | _ -> false)
    | BitAndAssign | BitOrAssign | BitXorAssign | LshiftAssign | RshiftAssign ->
        is_integer t
    | _ -> false
  in
  if not operand_ok then
    emit env (Error.bad_operand l.span ~op:(show_binop_sym op) ~ty:(show_ty t));
  let tr =
    match op with
    (* the count keeps its own type since it's just how far to shift *)
    | LshiftAssign | RshiftAssign ->
        let tr = synth env r in
        if not (is_integer tr.T.ty) then
          add_error env r.span
            (Printf.sprintf "shift count must be an integer, found %s"
               (show_ty tr.T.ty));
        tr
    | _ -> check env r t
  in
  T.mk TVoid (T.TBinOp (op, tl, tr))

and synth_unop (env : env) (op : unop) (e : expr) : T.texpr =
  match op with
  | Neg ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_numeric t) then
        emit env (Error.bad_operand e.span ~op:"-" ~ty:(show_ty t));
      T.mk t (T.TUnOp (op, te))
  | Not ->
      let te = check env e TBool in
      T.mk TBool (T.TUnOp (op, te))
  | BitNot ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_integer t) then
        emit env (Error.bad_operand e.span ~op:"~" ~ty:(show_ty t));
      T.mk t (T.TUnOp (op, te))
  | Deref -> (
      let te = synth env e in
      match strip_alias te.T.ty with
      | TPointer inner -> T.mk inner (T.TUnOp (op, te))
      | TError -> dummy_texpr
      | TOpaquePtr ->
          emit env
            Diagnostic.(
              error "cannot dereference *opaque"
              |> at e.span
              |> help "cast to a typed pointer first");
          dummy_texpr
      | t ->
          emit env (Error.named e.span "cannot dereference type" (show_ty t));
          dummy_texpr)
  | AddressOf ->
      let te = synth env e in
      (match te.T.desc with
      | T.TIdent s
        when Symbol.is_comptime s.kind
             || (Symbol.is_global s.kind && is_comptime_global env s.name) ->
          emit env
            Diagnostic.(
              error ("cannot take address of a constant: " ^ s.name)
              |> at e.span
              |> help "a const has no storage, use let")
      | _ ->
          if not (is_lvalue te) then
            add_error env e.span "cannot take address of expression");
      T.mk (TPointer te.T.ty) (T.TUnOp (op, te))

(* Synthesize the type of a field access expression. *)
and synth_field (env : env) (span : Ast.span) (e : expr) (fname : string) :
    T.texpr =
  let te = synth env e in
  let ty = te.T.ty in
  match strip_alias ty with
  | TArray (elem, _) | TSlice elem -> (
      match fname with
      | "len" -> T.mk (TInt Usize) (T.TLen te)
      | "ptr" -> T.mk (TPointer elem) (T.TDataPtr te)
      | _ ->
          emit env
            (Error.named span "no field" fname
            |> Diagnostic.label (Printf.sprintf "on %s" (show_ty ty)));
          dummy_texpr)
  | TOpaquePtr ->
      emit env
        Diagnostic.(
          error "cannot access a field of *opaque"
          |> at span
          |> help "cast to a typed pointer first");
      dummy_texpr
  | _ -> synth_struct_field env span te ty fname

and synth_struct_field (env : env) (span : Ast.span) (te : T.texpr) (ty : ty)
    (fname : string) : T.texpr =
  let rec peel depth = function
    | TStruct (sname, _) -> Some (sname, depth)
    | TAlias (_, base) -> peel depth base
    | TPointer t -> peel (depth + 1) t
    | _ -> None
  in
  match peel 0 ty with
  | None when strip_alias ty = TError -> dummy_texpr
  | None ->
      emit env (Error.named span "type has no fields" (show_ty ty));
      dummy_texpr
  | Some (_, depth) when depth > 1 ->
      let hint = Printf.sprintf "dereference first: `(*p).%s`" fname in
      emit env
        (Error.named span "too many pointer levels" (show_ty ty)
        |> Diagnostic.help hint);
      dummy_texpr
  | Some (sname, _) -> (
      let info = lookup_struct env span sname in
      match List.assoc_opt fname info.field_tys with
      | Some ft -> T.mk ft (T.TFieldAccess (te, fname))
      | None ->
          emit env
            (Error.named span "no field" fname
            |> Diagnostic.label (Printf.sprintf "on struct %s" sname));
          dummy_texpr)

and synth_call (env : env) (span : Ast.span) (callee : expr) (args : expr list)
    : T.texpr =
  match callee.desc with
  | Ident name when Symbol.is_func (sym env callee.span).kind ->
      let fn_sym = sym env callee.span in
      let sig_ = lookup_func env callee.span name in
      let targs = check_args env span sig_ args in
      let fixed_count =
        if sig_.variadic then Some (List.length sig_.param_tys) else None
      in
      let callee_texpr =
        T.mk (TFunc (sig_.param_tys, sig_.ret_ty)) (T.TIdent fn_sym)
      in
      T.mk sig_.ret_ty (T.TCall (callee_texpr, targs, fixed_count))
  | _ -> (
      (* the callee is a value holding a fn ptr so call through it *)
      let callee_texpr = synth env callee in
      match resolve_ty callee_texpr.T.ty with
      | TFunc (param_tys, ret_ty) ->
          let sig_ = { param_tys; ret_ty; variadic = false } in
          let targs = check_args env span sig_ args in
          T.mk ret_ty (T.TCall (callee_texpr, targs, None))
      | _ ->
          emit env
            (Diagnostic.error "not callable"
            |> Diagnostic.at callee.span
            |> Diagnostic.label
                 (Printf.sprintf "this has type %s" (show_ty callee_texpr.T.ty))
            );
          dummy_texpr)

and synth_index (env : env) (span : Ast.span) (base : expr) (idx : expr) :
    T.texpr =
  let tbase = synth env base in
  match strip_alias tbase.T.ty with
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
              T.mk lt (T.TBinOp (Ast.Add, thi, T.mk lt (T.TInt 1L)))
            else thi
          in
          T.mk (TSlice elem) (T.TSliceExpr (tbase, tlo, thi))
      | _ ->
          let tidx = synth env idx in
          if not (is_integer tidx.T.ty) then
            add_error env idx.span "array index must be an integer";
          T.mk elem (T.TIndex (tbase, tidx)))
  | TError -> dummy_texpr
  | TOpaquePtr ->
      emit env
        Diagnostic.(
          error "cannot index *opaque"
          |> at span
          |> help "cast to a typed pointer first");
      dummy_texpr
  | t ->
      emit env (Error.named span "cannot index type" (show_ty t));
      dummy_texpr

(* TODO(ccf6): Validate that the operand is a numeric type *)
(* TODO(b5ae): Better error messages *)
(* an array size can demand a const before its decl is checked so values
   resolve on demand from here and fold_consts below shares this resolver *)
and fold_num (env : env) (te : T.texpr) : Const_eval.const_num =
  Const_eval.fold_const_num
    ~sizeof:(ty_size env.struct_fields)
    ~resolve:(resolve_const env) te

and resolve_const (env : env) (s : Symbol.t) (_ : ty) (span : Ast.span) :
    Const_eval.const_num =
  match s.kind with
  | Symbol.Local Ast.Const -> (
      match Hashtbl.find_opt env.l_vals s.id with
      | Some v -> v
      | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ]))
  | Symbol.Global when Hashtbl.mem env.g_state s.name ->
      global_const_num env span s.name
  | _ -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])

and global_const_num (env : env) (span : Ast.span) (name : string) :
    Const_eval.const_num =
  let st =
    match Hashtbl.find_opt env.g_state name with
    | Some st -> st
    | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
  in
  match st.value with
  | Some v -> v
  | None ->
      if st.busy then
        raise (Diagnostic.Errors [ Error.named span "cyclic constant" name ]);
      let te =
        match global_typed_init env span name with
        | te -> te
        (* a dummy lands on failure so the error reports only once *)
        | exception e ->
            st.value <- Some dummy_const_num;
            raise e
      in
      st.busy <- true;
      let v =
        match
          if Const_eval.foldable te then fold_num env te else dummy_const_num
        with
        | v ->
            st.busy <- false;
            v
        | exception e ->
            st.busy <- false;
            st.value <- Some dummy_const_num;
            raise e
      in
      st.value <- Some v;
      v

(* typing shares the busy flag so a self demand mid typing is a cycle *)
and global_typed_init (env : env) (span : Ast.span) (name : string) : T.texpr =
  let st =
    match Hashtbl.find_opt env.g_state name with
    | Some st -> st
    | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
  in
  match st.typed with
  | Some te -> te
  | None ->
      if st.busy then
        raise (Diagnostic.Errors [ Error.named span "cyclic constant" name ]);
      let e, typ =
        match st.def with
        | { init = Some e; typ; _ } -> (e, typ)
        | _ -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
      in
      st.busy <- true;
      let te =
        match check env e (ty_of_ast env typ) with
        | te -> te
        | exception ex ->
            st.busy <- false;
            raise ex
      in
      st.busy <- false;
      st.typed <- Some te;
      te

(* a failed fold reports and hands back a dummy so checking continues *)
and fold_num_or (env : env) (default : Const_eval.const_num) (te : T.texpr) :
    Const_eval.const_num =
  if not (Const_eval.foldable te) then default
  else
    try fold_num env te
    with Diagnostic.Errors ds ->
      List.iter (emit env) ds;
      default

(* the folded size fixes dropped suffixes and silent wraps on huge counts *)
and eval_array_size (env : env) (e : expr) : int =
  let bad msg =
    add_error env e.span msg;
    0
  in
  let te = synth env e in
  if not (is_integer te.T.ty) then bad "array size must be an integer"
  else
    let v = fold_num_or env dummy_const_num te in
    let n = Const_eval.const_to_int64 te.T.ty v in
    (* the source may be an expression so the message shows the folded value *)
    let shown =
      if is_unsigned te.T.ty then Printf.sprintf "%Lu" n else Int64.to_string n
    in
    if
      Int64.compare n 0x7FFF_FFFFL > 0
      || (Int64.compare n 0L < 0 && is_unsigned te.T.ty)
    then bad ("array size is too large: " ^ shown)
    else if Int64.compare n 0L < 0 then bad ("array size is negative: " ^ shown)
    else Int64.to_int n

and check_stmt (env : env) (s : stmt) : env * T.tstmt =
  let env', tsdesc = check_stmt_desc env s in
  (env', { T.tsdesc; span = s.span })

and check_stmt_desc (env : env) (s : stmt) : env * T.tstmt_desc =
  match s.sdesc with
  | Binding (kind, name, nspan, ann, e) ->
      let t, te =
        match (ann, e) with
        | Some a, Some e ->
            let want = ty_of_ast env a in
            let te = check ~adopt:true env e want in
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
      if kind = Const then (
        check_const_scalar env nspan t;
        Hashtbl.replace env.l_vals (sym env nspan).id
          (fold_num_or env dummy_const_num te));
      (extend_var env nspan name t, T.TBinding (kind, sym env nspan, t, te))
  | Return None ->
      (* FIXME(603c): revisit once I decide how implicit and explicit returns work *)
      if env.ret_ty = TNever then
        add_error env s.span "a never function cannot return"
      else if env.ret_ty <> TVoid && not env.in_main then
        add_error env s.span "empty return in non-void function";
      (env, T.TReturn None)
  | Return (Some e) when env.ret_ty = TNever ->
      add_error env s.span "a never function cannot return";
      (env, T.TReturn (Some (synth env e)))
  | Return (Some e) ->
      let te = check env e env.ret_ty in
      (match Escape.return_escapes te with
      | Some kind ->
          let noun =
            match kind with
            | Escape.Slice -> "slice of a local"
            | Escape.Address -> "address of a local"
          in
          emit env
            (Diagnostic.error (noun ^ " escapes")
            |> Diagnostic.at e.span
            |> Diagnostic.label "points into freed stack memory")
      | None -> ());
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
            match strip_alias ti.T.ty with
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
      (env, T.TFor (sym env nspan, elem_ty, titer, tbody))
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

(* Performance critical since this pass walks every statement *)
and check_stmts (env : env) (stmts : stmt list) : env * T.tstmt list =
  let final_env, tstmts_reversed, _, _ =
    List.fold_left
      (fun (current_env, acc, returned, warned) (s : stmt) ->
        if returned && not warned then
          add_warning current_env s.span "unreachable code";
        let next_env, ts = check_stmt current_env s in
        (* break and continue end the block just like a returning if does *)
        let terminates =
          Reachability.stmt_returns (is_never_call current_env) s
          || match s.sdesc with Break | Continue -> true | _ -> false
        in
        (next_env, ts :: acc, returned || terminates, warned || returned))
      (env, [], false, false) stmts
  in
  (final_env, List.rev tstmts_reversed)

(* main implicitly returns i32 for the C runtime and everything else is void *)
let ret_ty_of (env : env) (fd : func_def) : ty =
  match fd.ret with
  | Some { tdesc = Named "never"; _ } -> TNever
  | Some t -> ty_of_ast env t
  | None -> if fd.name = "main" then TInt I32 else TVoid

(* First pass collecting signatures so that the compiler
   can handle forward references *)
let collect_func (env : env) (fd : func_def) : unit =
  let param_tys = List.map (fun (p : param) -> ty_of_ast env p.typ) fd.params in
  let ret_ty = ret_ty_of env fd in
  Hashtbl.replace env.funcs fd.name
    { param_tys; ret_ty; variadic = fd.variadic }

(* every kind of type name shares one namespace *)
let existing_type_kind (env : env) (name : string) : string option =
  if List.mem_assoc name builtin_tys then Some "a builtin type"
  else
    match Hashtbl.find_opt env.types name with
    | Some (DStruct _) -> Some "a struct"
    | Some (DNewtype _) -> Some "a newtype"
    | Some (DAlias _) -> Some "an alias"
    | None -> None

let reject_taken_type_name (env : env) (span : Ast.span) (name : string) : bool
    =
  match existing_type_kind env name with
  | Some kind ->
      emit env (Error.named span ("already defined as " ^ kind) name);
      true
  | None -> false

(* the name goes in first so a field can name this struct or one defined later *)
let reserve_struct_name (env : env) (sd : struct_def) : unit =
  if not (reject_taken_type_name env sd.span sd.name) then (
    let seen = Hashtbl.create 8 in
    List.iter
      (fun (f : field) ->
        if Hashtbl.mem seen f.name then
          emit env (Error.named f.span "duplicate field" f.name)
        else Hashtbl.add seen f.name ())
      sd.fields;
    Hashtbl.replace env.types sd.name (DStruct { field_tys = [] });
    Hashtbl.replace env.struct_fields sd.name [])

(* TODO(9b1e): Add a rawptr/voidptr keyword for untyped pointers (C's void pointer) *)
let resolve_struct_fields (env : env) (sd : struct_def) : unit =
  match Hashtbl.find_opt env.types sd.name with
  | Some (DStruct _) ->
      let field_tys =
        List.map (fun (f : field) -> (f.name, ty_of_ast env f.typ)) sd.fields
      in
      Hashtbl.replace env.types sd.name (DStruct { field_tys });
      Hashtbl.replace env.struct_fields sd.name field_tys
  | _ -> ()

(* a pointer or slice field is just an address so it can't grow the struct *)
let check_struct_cycle (env : env) (sd : struct_def) : unit =
  let fields_of name =
    Option.value ~default:[] (Hashtbl.find_opt env.struct_fields name)
  in
  let on_path = Hashtbl.create 8 in
  let rec reaches (target : string) (t : ty) : bool =
    match resolve_ty t with
    | TStruct (name, _) when name = target -> true
    | TStruct (name, _) when Hashtbl.mem on_path name -> false
    | TStruct (name, _) ->
        Hashtbl.add on_path name ();
        let hit =
          List.exists (fun (_, ft) -> reaches target ft) (fields_of name)
        in
        Hashtbl.remove on_path name;
        hit
    | TArray (elem, _) -> reaches target elem
    | _ -> false
  in
  if List.exists (fun (_, ft) -> reaches sd.name ft) (fields_of sd.name) then
    emit env (Error.named sd.span "recursive struct has infinite size" sd.name)

let collect_alias (env : env) (td : type_alias_def) : unit =
  if not (reject_taken_type_name env td.span td.name) then
    let t = ty_of_ast env td.typ in
    Hashtbl.replace env.types td.name (DAlias t)

let collect_newtype (env : env) (td : type_alias_def) : unit =
  if not (reject_taken_type_name env td.span td.name) then
    let t = ty_of_ast env td.typ in
    Hashtbl.replace env.types td.name (DNewtype t)

let collect_global (env : env) (gd : global_def) : unit =
  (if gd.init = None then
     match gd.kind with
     | Var -> ()
     | Let -> emit env (Error.named gd.span "let without initializer" gd.name)
     | Const ->
         emit env (Error.named gd.span "const without initializer" gd.name));
  let t = ty_of_ast env gd.typ in
  Hashtbl.replace env.globals gd.name (t, gd.kind)

let fill_struct_fields_decl (env : env) (decl : decl) : unit =
  match decl with Struct sd -> resolve_struct_fields env sd | _ -> ()

let check_cycle_decl (env : env) (decl : decl) : unit =
  match decl with Struct sd -> check_struct_cycle env sd | _ -> ()

let collect_decl (env : env) (decl : decl) : unit =
  match decl with
  | Struct sd -> reserve_struct_name env sd
  | Func fd | Extern fd -> collect_func env fd
  | Global gd -> collect_global env gd
  | TypeAlias td -> collect_alias env td
  | Newtype td -> collect_newtype env td

let check_func ?(is_extern = false) (env : env) (fd : func_def) : T.tfunc_def =
  (* the collected signature is reused so a bad array size errors once *)
  let collected = Hashtbl.find_opt env.funcs fd.name in
  let param_tys =
    match collected with
    | Some s when List.length s.param_tys = List.length fd.params -> s.param_tys
    | _ -> List.map (fun (p : param) -> ty_of_ast env p.typ) fd.params
  in
  let params_typed =
    List.map2 (fun (p : param) t -> (p.name, t, p.span)) fd.params param_tys
  in
  let params = List.map (fun (_, t, span) -> (sym env span, t)) params_typed in

  let ret_ty =
    match collected with Some s -> s.ret_ty | None -> ret_ty_of env fd
  in

  (* main always returns a 32 bit integer so any other type the user writes is rejected *)
  if fd.name = "main" && ret_ty <> TInt I32 then begin
    let span = match fd.ret with Some t -> t.span | None -> fd.span in
    emit env
      (Error.type_mismatch span ~expected:(show_ty (TInt I32))
         ~found:(show_ty ret_ty))
  end;

  let func_env = push_scope { env with ret_ty; in_main = fd.name = "main" } in
  (* an extern has no body so its params can't be used and stay quiet *)
  let param_env =
    List.fold_left
      (fun e (name, t, span) -> extend_var ~used:is_extern e span name t)
      func_env params_typed
  in

  let final_env, tbody = check_stmts param_env fd.body in
  warn_unused_in_scope final_env;

  if
    (not is_extern) && ret_ty <> TVoid && fd.name <> "main"
    && not (Reachability.stmts_return (is_never_call env) fd.body)
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
  (* A function address is a link time constant. *)
  | TIdent s -> Symbol.is_func s.kind || is_const_global env s.name
  (* the address of a global is a link time constant *)
  | TUnOp (Ast.AddressOf, { desc = TIdent s; _ }) -> Symbol.is_global s.kind
  | TUnOp (_, e) -> is_const_texpr env e
  | TBinOp (_, l, r) -> is_const_texpr env l && is_const_texpr env r
  | TCast (e, _) -> is_const_texpr env e
  | TZero -> true
  (* an array literal is constant when all its elements are *)
  | TArrayLit elems -> List.for_all (is_const_texpr env) elems
  | TStructLit (_, fields) ->
      List.for_all (fun (_, fe) -> is_const_texpr env fe) fields
  (* never compile-time by design *)
  | TCall _ | TFieldAccess _ | TRange _ | TRangeInclusive _ | TIndex _ | TLen _
  | TToSlice _ | TSliceExpr _ | TDataPtr _ | TBlockExpr _ ->
      false
  | TUndef -> true

let check_global (env : env) (gd : global_def) : T.tglobal_def =
  (* the collected type is reused so a bad array size errors once *)
  let t =
    match Hashtbl.find_opt env.globals gd.name with
    | Some (t, _) -> t
    | None -> ty_of_ast env gd.typ
  in
  if gd.kind = Const then check_const_scalar env gd.span t;
  let tinit =
    match gd.init with
    | None -> None
    | Some { desc = Undefined; span } when gd.kind = Const ->
        emit env
          Diagnostic.(
            error "const cannot be undefined"
            |> at span
            |> help "use let for values that need storage");
        None
    | Some { desc = Undefined; _ } -> None
    | Some e ->
        let te =
          if Hashtbl.mem env.g_state gd.name then
            global_typed_init env e.span gd.name
          else check env e t
        in
        if not (is_const_texpr env te) then (
          emit env (Error.named e.span "initializer must be constant" gd.name);
          None)
        else Some te
  in
  { T.name = gd.name; ty = t; init = tinit; kind = gd.kind }

let check_decl (env : env) (decl : decl) : T.tdecl =
  match decl with
  | Func fd ->
      let tfd = check_func env fd in
      T.TFunc tfd
  | Extern fd ->
      let tfd = check_func ~is_extern:true env fd in
      T.TExtern tfd
  | Struct sd ->
      (* a rejected duplicate never landed in the table and its fields are read directly *)
      let field_tys =
        match Hashtbl.find_opt env.types sd.name with
        | Some (DStruct info) -> info.field_tys
        | _ ->
            List.map
              (fun (f : field) -> (f.name, ty_of_ast env f.typ))
              sd.fields
      in
      T.TStruct (sd.name, field_tys, sd.modifiers)
  | Global gd -> T.TGlobal (check_global env gd)
  (* a rejected duplicate never landed in the table and falls back to the written type *)
  | TypeAlias td ->
      let t =
        match Hashtbl.find_opt env.types td.name with
        | Some (DAlias t) -> t
        | _ -> ty_of_ast env td.typ
      in
      T.TTypeAlias (td.name, t)
  | Newtype td ->
      let t =
        match Hashtbl.find_opt env.types td.name with
        | Some (DNewtype t) -> t
        | _ -> ty_of_ast env td.typ
      in
      T.TNewtype (td.name, t)

let fold_consts (env : env) (tdecls : T.tdecl list) : T.tdecl list =
  Const_fold.run ~emit:(emit env)
    ~force_const:(fun span name -> ignore (global_const_num env span name))
    ~local_value:(Hashtbl.find_opt env.l_vals)
    ~global_value:(fun name ->
      if is_comptime_global env name then
        match Hashtbl.find_opt env.g_state name with
        | Some { value = Some v; _ } -> Some v
        | _ -> None
      else None)
    ~fold_num:(fold_num env) tdecls

(* hands warnings back for the edge to render and blows up on any error *)
let typecheck (uses : Resolve.t) (decls : decl list) :
    T.tdecl list * Diagnostic.t list =
  let env = make_env uses in
  (* an early array size can demand any later const so defs go in first *)
  List.iter
    (function
      | Global ({ kind = Let | Const; _ } as gd) ->
          Hashtbl.replace env.g_state gd.name
            { def = gd; typed = None; value = None; busy = false }
      | _ -> ())
    decls;
  List.iter (collect_decl env) decls;
  List.iter (fill_struct_fields_decl env) decls;
  List.iter (check_cycle_decl env) decls;
  let tdecls = List.map (check_decl env) decls in
  let tdecls = fold_consts env tdecls in
  let all = Diagnostic.drain env.diags in
  let is_err (d : Diagnostic.t) = d.severity = Diagnostic.Error in
  if List.exists is_err all then raise (Diagnostic.Errors all)
  else (tdecls, List.filter (fun d -> not (is_err d)) all)
