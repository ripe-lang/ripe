(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast
open Types
open Ty_pred
module T = Typed_ast

type func_sig = { param_tys : ty list; ret_ty : ty; variadic : bool }
type struct_info = { field_tys : (string * ty) list }

(* Structs newtypes and aliases share one namespace of type names *)
type type_def = DStruct of struct_info | DNewtype of ty | DAlias of ty
type var_info = { ty : ty; used : bool ref; span : Ast.span }

(* The typed and value fields only ever go from None to Some so nothing rolls
   back *)
type gstate = {
  def : global_def;
  mutable typed : T.texpr option;
  mutable value : Const_eval.const_num option;
  (* Busy means this global is mid evaluation so a self demand is a cycle *)
  mutable busy : bool;
}

type env = {
  vars : (string * var_info) list list;
  funcs : (string, func_sig) Hashtbl.t;
  types : (string, type_def) Hashtbl.t;
  (* Struct field layouts mirror the DStruct entries in types so ty_size need
     not rebuild them *)
  struct_fields : (string, (string * ty) list) Hashtbl.t;
  globals : (string, ty * Ast.binding_kind) Hashtbl.t;
  (* Constants evaluate on demand so an array size may name a later const *)
  g_state : (string, gstate) Hashtbl.t;
  l_vals : (Symbol.key, Const_eval.const_num) Hashtbl.t;
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

let symbol_name (env : env) (symbol : Symbol.t) : string =
  Resolve.internal_name env.uses symbol

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
          (* Variables prefixed with '_' suppress unused warnings *)
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
  | [] -> assert false (* No active scope *)
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
  | Some (_, (Let | Comptime)) -> true
  | _ -> false

let is_comptime_global (env : env) (name : string) : bool =
  match Hashtbl.find_opt env.globals name with
  | Some (_, Comptime) -> true
  | _ -> false

let check_const_scalar (env : env) (span : Ast.span) (t : ty) : unit =
  if not (is_scalar t) then
    emit env
      Diagnostic.(
        error ("comptime must be a scalar, found " ^ show_ty t)
        |> at span
        |> help "use let for values that need storage")

let lookup_struct (env : env) (span : Ast.span) (name : string) : struct_info =
  match Hashtbl.find_opt env.types name with
  | Some (DStruct s) -> s
  | _ ->
      emit env (Error.undefined_name span "struct" name);
      { field_tys = [] }

let rec ty_of_ast (env : env) (t : typ) : ty =
  match t.tdesc with
  | ErrorType -> TError
  (* Never is the return type of a function that can't return so no value ever
     has it *)
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
      (* Locals shadow globals shadow functions *)
      match Hashtbl.find_opt env.globals name with
      | Some (t, _) -> t
      | None -> (
          (* An array size may name a global not collected yet so type it now *)
          match Hashtbl.find_opt env.g_state name with
          (* A global whose own size names it would loop forever *)
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
              (* Fall back to function table so function names can be used as
                 values *)
              match Hashtbl.find_opt env.funcs name with
              | Some sg -> TFunc (sg.param_tys, sg.ret_ty)
              | None ->
                  emit env (Error.undefined_name span "variable" name);
                  TInt I32)))

(* This pass does the bidirectional type checking *)

(* Stamp the source span here so the mk sites underneath stay span free *)
and synth (env : env) (e : expr) : T.texpr =
  { (synth_desc env e) with T.span = e.span }

and synth_desc (env : env) (e : expr) : T.texpr =
  match e.desc with
  | ErrorExpr -> dummy_texpr
  | Int (n, suf) ->
      let kind = match suf with Some s -> suffix_kind s | None -> I32 in
      if Int64.unsigned_compare n (int_kind_pos_limit kind) > 0 then
        emit env (Error.int_out_of_range e.span ~ty:(show_ty (TInt kind)));
      T.mk (TInt kind) (T.TInt n)
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      synth env { operand with span = e.span }
  | UnOp (Neg, { desc = Int (n, Some s); _ }) ->
      let kind = suffix_kind s in
      if Int64.unsigned_compare n (int_kind_neg_limit kind) > 0 then
        emit env (Error.int_out_of_range e.span ~ty:(show_ty (TInt kind)));
      T.mk (TInt kind) (T.TInt (Int64.neg n))
  | Float f -> T.mk (TFloat F64) (T.TFloat f)
  | Bool b -> T.mk TBool (T.TBool b)
  | Null -> T.mk TNull T.TNull
  | String s -> T.mk (TPointer (TInt I8)) (T.TCStr s)
  | Char c -> T.mk TChar (T.TChar c)
  | Ident name ->
      let t = lookup_var env e.span name in
      T.mk t (T.TIdent (sym env e.span))
  | Call (callee, args) -> synth_call env e.span callee args
  | BinOp (op, l, r) -> synth_binop env op l r
  | UnOp (op, e) -> synth_unop env op e
  | FieldAccess (inner_e, fname) -> synth_field env e.span inner_e fname
  | Cast (operand, t, kind) ->
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
      else if kind = Checked then
        begin match (resolve_ty te.T.ty, resolve_ty ty) with
        | TInt _, TInt _ -> ()
        | _ ->
            emit env
              (Diagnostic.error "checked cast only supports integers"
              |> Diagnostic.at e.span
              |> Diagnostic.label
                   (Printf.sprintf "`%s` traps on integer overflow only"
                      (show_cast_op Checked))
              |> Diagnostic.help
                   (Printf.sprintf "use a plain `%s` cast here"
                      (show_cast_op Normal)))
        end;
      T.mk ty (T.TCast (te, kind))
  | SizeOf t -> T.mk (TInt I64) (T.TSizeOf (ty_of_ast env t))
  (* Ranges are not first-class values and only work as for-loop iterators or
     slice bounds *)
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
          (* Omitted fields are zero-initialized *)
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
  | Block body ->
      let tb, ty = check_scoped_block env e.span body None false in
      T.mk ty (T.TBlock tb)
  | If (branches, else_body) -> check_if env e.span branches else_body None
  | While (cond, body) ->
      let tc = check env cond TBool in
      let tb, _ = check_scoped_block env e.span body None true in
      (* A while true with no break loops forever so it never yields control *)
      let diverges =
        match cond.desc with
        | Bool true -> not (Reachability.loop_has_break body)
        | _ -> false
      in
      T.mk (if diverges then TNever else TVoid) (T.TWhile (tc, tb))
  | For (name, nspan, iter, body) -> synth_for env e.span name nspan iter body
  | Binding (kind, name, nspan, ann, init) ->
      snd (check_binding env kind name nspan ann init)
  | Return init -> synth_return env e.span init
  | Break ->
      if not env.in_loop then add_error env e.span "break outside loop";
      T.mk TNever T.TBreak
  | Continue ->
      if not env.in_loop then add_error env e.span "continue outside loop";
      T.mk TNever T.TContinue
  | PairAssign (ft, st, fv, sv) -> synth_pair_assign env ft st fv sv

(* The value of a block is its last element and void when the block is empty *)
and tblock_ty (tb : T.tblock) : ty =
  match List.rev tb with te :: _ -> te.T.ty | [] -> TVoid

(* A literal or diverging tail bends to a sibling so it can't anchor the type *)
and arm_is_flexible (e : expr) : bool =
  match e.desc with
  | Int (_, None) | Float _ -> true
  | UnOp ((Pos | Neg), inner) -> arm_is_flexible inner
  | Block body -> block_is_flexible body
  | If (branches, else_body) ->
      Option.is_some else_body
      && List.for_all (fun (_, body) -> block_is_flexible body) branches
      && Option.fold ~none:false ~some:block_is_flexible else_body
  | _ -> false

and block_is_flexible (body : block) : bool =
  match List.rev body with last :: _ -> arm_is_flexible last | [] -> false

(* Probe a block's result type with diagnostics muted so a sibling can anchor
   it *)
and block_result_ty (env : env) (body : block) : ty =
  let quiet = { env with diags = Diagnostic.sink () } in
  let inner = push_scope quiet in
  let _, tb = check_block inner Ast.dummy_span body None in
  tblock_ty tb

and reconcile_if_result (env : env) (branches : (expr * block) list)
    (else_b : block) : ty =
  let arms = List.map snd branches @ [ else_b ] in
  let probes = List.map (fun body -> (body, block_result_ty env body)) arms in
  let rigid =
    List.find_opt
      (fun (body, t) -> (not (block_is_flexible body)) && t <> TNever)
      probes
  in
  match rigid with
  | Some (_, t) -> t
  | None -> (
      match List.find_opt (fun (_, t) -> t <> TNever) probes with
      | Some (_, t) -> t
      | None -> TNever)

(* Thread env so a binding is visible to later elements then check the tail
   against want and discard the rest *)
and check_block (env : env) (span : Ast.span) (body : block) (want : ty option)
    : env * T.tblock =
  let rec go env diverged acc (elems : expr list) =
    match elems with
    | [] ->
        (match want with
        | Some w when strip_alias w <> TVoid ->
            emit env
              (Error.type_mismatch span ~expected:(show_ty w)
                 ~found:(show_ty TVoid))
        | _ -> ());
        (env, List.rev acc)
    | [ last ] ->
        (* A dead tail keeps only its warning and its type need not match *)
        let tail_want = if diverged then None else want in
        if diverged then add_warning env last.span "unreachable code";
        let env, te = check_elem env last tail_want in
        (env, List.rev (te :: acc))
    | e :: rest ->
        if diverged then add_warning env e.span "unreachable code";
        let env, te = check_elem env e None in
        go env (diverged || te.T.ty = TNever) (te :: acc) rest
  in
  go env false [] body

and check_elem (env : env) (e : expr) (want : ty option) : env * T.texpr =
  match e.desc with
  | Binding (kind, name, nspan, ann, init) ->
      let env', tb = check_binding env kind name nspan ann init in
      (match want with
      | Some w when strip_alias w <> TVoid ->
          emit env
            (Error.type_mismatch e.span ~expected:(show_ty w)
               ~found:(show_ty TVoid))
      | _ -> ());
      (env', tb)
  | _ ->
      let te =
        match want with Some w -> check env e w | None -> synth env e
      in
      (env, te)

(* Push a scope for the block then flag any leftover bindings *)
and check_scoped_block (env : env) (span : Ast.span) (body : block)
    (want : ty option) (in_loop : bool) : T.tblock * ty =
  let base = if in_loop then { env with in_loop = true } else env in
  let inner = push_scope base in
  let final_inner, tb = check_block inner span body want in
  warn_unused_in_scope final_inner;
  (tb, tblock_ty tb)

and check_binding (env : env) (kind : Ast.binding_kind) (name : string)
    (nspan : Ast.span) (ann : typ option) (init : expr option) : env * T.texpr =
  let t, te =
    match (ann, init) with
    | Some a, Some e ->
        let want = ty_of_ast env a in
        let te = check ~adopt:true env e want in
        (want, te)
    | None, Some e ->
        let te = synth env e in
        if te.T.ty = TVoid then (
          emit env (Error.named e.span "cannot bind void value" name);
          (TError, te))
        else (te.T.ty, te)
    | Some a, None ->
        let want = ty_of_ast env a in
        (want, T.mk want T.TZero)
    | None, None ->
        emit env (Error.named nspan "cannot infer type" name);
        (TInt I32, dummy_texpr)
  in
  if kind = Comptime then (
    check_const_scalar env nspan t;
    Hashtbl.replace env.l_vals
      (Symbol.key (sym env nspan))
      (fold_num_or env dummy_const_num te));
  ( extend_var env nspan name t,
    T.mk TVoid (T.TBinding (kind, sym env nspan, t, te)) )

and synth_return (env : env) (span : Ast.span) (init : expr option) : T.texpr =
  match init with
  | None ->
      if env.ret_ty = TNever then
        add_error env span "a never function cannot return"
      else if env.ret_ty <> TVoid && not env.in_main then
        add_error env span "empty return in non-void function";
      T.mk TNever (T.TReturn None)
  | Some e when env.ret_ty = TNever ->
      add_error env span "a never function cannot return";
      T.mk TNever (T.TReturn (Some (synth env e)))
  | Some e ->
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
      T.mk TNever (T.TReturn (Some te))

and synth_for (env : env) (span : Ast.span) (name : string) (nspan : Ast.span)
    (iter : expr) (body : block) : T.texpr =
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
        | TError -> (ti, TError)
        | TArray (elem, _) | TSlice elem -> (ti, elem)
        | t ->
            emit env
              (Error.named iter.span "cannot iterate over type" (show_ty t));
            (ti, TInt I32))
  in
  let inner = push_scope { env with in_loop = true } in
  let inner = extend_var inner nspan name elem_ty in
  let final_inner, tb = check_block inner span body None in
  warn_unused_in_scope final_inner;
  T.mk TVoid (T.TFor (sym env nspan, elem_ty, titer, tb))

(* One if handles both a value and a plain statement and want None means
   synthesize *)
and check_if (env : env) (span : Ast.span) (branches : (expr * block) list)
    (else_body : block option) (want : ty option) : T.texpr =
  match want with
  | None -> (
      match else_body with
      | None ->
          let tbranches =
            List.map
              (fun (c, body) ->
                ( check env c TBool,
                  fst (check_scoped_block env span body None false) ))
              branches
          in
          T.mk TVoid (T.TIf (tbranches, None))
      | Some else_b ->
          let rty = reconcile_if_result env branches else_b in
          check_if env span branches else_body (Some rty))
  | Some w ->
      let tbranches =
        List.map
          (fun (c, body) ->
            ( check env c TBool,
              fst (check_scoped_block env span body (Some w) false) ))
          branches
      in
      let telse =
        match else_body with
        | Some body ->
            Some (fst (check_scoped_block env span body (Some w) false))
        | None ->
            if strip_alias w <> TVoid then
              emit env
                (Error.type_mismatch span ~expected:(show_ty w)
                   ~found:(show_ty TVoid));
            None
      in
      let arm_tys =
        (match telse with Some tb -> [ tblock_ty tb ] | None -> [])
        @ List.map (fun (_, tb) -> tblock_ty tb) tbranches
      in
      (* Every arm diverges so the whole if yields no value *)
      let ty =
        if Option.is_some telse && List.for_all (fun t -> t = TNever) arm_tys
        then TNever
        else w
      in
      T.mk ty (T.TIf (tbranches, telse))

(* This has to be this type *)
and check ?(adopt = false) (env : env) (e : expr) (want : ty) : T.texpr =
  { (check_desc ~adopt env e want) with T.span = e.span }

and check_desc ?(adopt = false) (env : env) (e : expr) (want : ty) : T.texpr =
  let target = if adopt then resolve_ty want else strip_alias want in
  (* Synthesize then check the result matches want *)
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
      (* Materialize the fat pointer when a fixed array coerces to a slice *)
      | TSlice _, TArray _ -> T.mk want (T.TToSlice te)
      | _ -> te
  in
  match e.desc with
  | ErrorExpr -> dummy_texpr
  | Int (_, Some _) ->
      (* The suffix already picked the type so a wrong target is an error not a
         quiet coercion *)
      let te = synth_desc env e in
      if strip_alias want <> te.T.ty then
        emit env
          (Error.type_mismatch e.span ~expected:(show_ty want)
             ~found:(show_ty te.T.ty));
      te
  | Int (n, None) -> (
      (* An untyped literal adopts a newtype over an int and checks its base *)
      match target with
      | TInt kind ->
          if Int64.unsigned_compare n (int_kind_pos_limit kind) > 0 then
            emit env
              (Error.int_out_of_range e.span ~ty:(show_ty (resolve_ty want)));
          T.mk want (T.TInt n)
      | TError -> T.mk want (T.TInt n)
      (* Want is not an integer type at all e.g. let y: bool = 20 *)
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
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      check ~adopt env { operand with span = e.span } want
  | UnOp (Neg, operand) when is_numeric (strip_alias want) ->
      T.mk want (T.TUnOp (Neg, check env operand want))
  | UnOp (Pos, operand) when is_numeric (strip_alias want) ->
      T.mk want (T.TUnOp (Pos, check env operand want))
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
  | Block body ->
      let tb, ty = check_scoped_block env e.span body (Some want) false in
      T.mk ty (T.TBlock tb)
  | If (branches, else_body) ->
      check_if env e.span branches else_body (Some want)
  | Undefined -> T.mk want T.TUndef
  | _ -> check_by_synth ()

and check_range_bounds (env : env) (lo : expr) (hi : expr) =
  let tlo, thi, t =
    (* This bends a lone literal on the left toward the typed value on the
       right *)
    if is_int_literal lo && not (is_int_literal hi) then
      let thi = synth env hi in
      let t = thi.T.ty in
      (check env lo t, thi, t)
      (* Otherwise anchor on lo and check hi against it (also covers two
         literals) *)
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
      (* C reads a float vararg as a double so widen it first *)
      let promote_vararg e =
        let te = synth env e in
        match resolve_ty te.T.ty with
        | TFloat F32 -> T.mk ~span:e.span (TFloat F64) (T.TCast (te, Normal))
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
      (* QBE has no float remainder instruction *)
      if op = Mod && match strip_alias t with TFloat _ -> true | _ -> false
      then emit env (Error.bad_operand l.span ~op:"%" ~ty:(show_ty t));
      let tr = check env r t in
      T.mk t (T.TBinOp (op, tl, tr))
  | Eq | Neq ->
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
        (* The count keeps its own integer type since it is only a number of
           positions *)
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
  let tl, tr = check_assign_operands env op l r in
  T.mk TVoid (T.TBinOp (op, tl, tr))

and check_assign_operands (env : env) (op : binop) (l : expr) (r : expr) :
    T.texpr * T.texpr =
  let tl = synth env l in
  if not (is_lvalue tl) then add_error env l.span "cannot assign to expression";
  (match tl.T.desc with
  | T.TIdent s when Symbol.is_func s.Symbol.kind ->
      emit env (Error.named l.span "cannot assign to function" s.Symbol.name)
  | T.TIdent _ | T.TFieldAccess _ | T.TIndex _ -> (
      (* This catches assignment to an immutable binding whether it's local or
         global. *)
      match root_binding tl with
      | Some s
        when Symbol.is_immutable s.Symbol.kind
             || Symbol.is_global s.Symbol.kind
                && is_const_global env (symbol_name env s) ->
          emit env
            (Error.named l.span "cannot assign to immutable" s.Symbol.name)
      | _ -> ())
  | _ -> ());
  let t = tl.T.ty in
  let operand_ok =
    match op with
    | Assign -> true
    | AddAssign | SubAssign | MulAssign | DivAssign -> is_numeric t
    (* QBE has no float remainder instruction *)
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
    (* The count keeps its own type since it's just how far to shift *)
    | LshiftAssign | RshiftAssign ->
        let tr = synth env r in
        if not (is_integer tr.T.ty) then
          add_error env r.span
            (Printf.sprintf "shift count must be an integer, found %s"
               (show_ty tr.T.ty));
        tr
    | _ -> check env r t
  in
  (tl, tr)

and synth_pair_assign (env : env) (ft : expr) (st : expr) (fv : expr)
    (sv : expr) : T.texpr =
  let ft, fv = check_assign_operands env Assign ft fv in
  let st, sv = check_assign_operands env Assign st sv in
  T.mk TVoid (T.TPairAssign (ft, st, fv, sv))

and synth_unop (env : env) (op : unop) (e : expr) : T.texpr =
  match op with
  | Pos ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_numeric t) then
        emit env (Error.bad_operand e.span ~op:"+" ~ty:(show_ty t));
      T.mk t (T.TUnOp (op, te))
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
        when Symbol.is_comptime s.Symbol.kind
             || Symbol.is_global s.Symbol.kind
                && is_comptime_global env (symbol_name env s) ->
          emit env
            Diagnostic.(
              error ("cannot take address of a constant: " ^ s.Symbol.name)
              |> at e.span
              |> help "a const has no storage, use let")
      | _ ->
          (* TODO(abe2): In the future allow &(a + b) once I figure out the
             memory and what to do about temporary lifetimes *)
          if not (is_lvalue te) then
            add_error env e.span "cannot take address of expression");
      T.mk (TPointer te.T.ty) (T.TUnOp (op, te))

(* This figures out the type of a field access *)
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
  | Ident name when Symbol.is_func (sym env callee.span).Symbol.kind ->
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
      (* The callee is a value holding a fn ptr so call through it *)
      let callee_texpr = synth env callee in
      match resolve_ty callee_texpr.T.ty with
      | TError -> dummy_texpr
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
      (* Arr[lo..hi] produces a slice that borrows into the same storage;
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

(* An array size can demand a const before its decl is checked so values resolve
   on demand from here and fold_consts below shares this resolver *)
and fold_num (env : env) (te : T.texpr) : Const_eval.const_num =
  Const_eval.fold_const_num
    ~sizeof:(ty_size env.struct_fields)
    ~resolve:(resolve_const env) te

and resolve_const (env : env) (s : Symbol.t) (_ : ty) (span : Ast.span) :
    Const_eval.const_num =
  match s.Symbol.kind with
  | Symbol.Local Ast.Comptime -> (
      match Hashtbl.find_opt env.l_vals (Symbol.key s) with
      | Some v -> v
      | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ]))
  | Symbol.Global when Hashtbl.mem env.g_state (symbol_name env s) ->
      global_const_num env span (symbol_name env s)
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
        (* A dummy lands on failure so the error reports only once *)
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

(* Typing shares the busy flag so a self demand mid typing is a cycle *)
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

(* A failed fold reports and hands back a dummy so checking continues *)
and fold_num_or (env : env) (default : Const_eval.const_num) (te : T.texpr) :
    Const_eval.const_num =
  if not (Const_eval.foldable te) then default
  else
    try fold_num env te
    with Diagnostic.Errors ds ->
      List.iter (emit env) ds;
      default

(* The folded size fixes dropped suffixes and silent wraps on huge counts *)
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
    (* The source may be an expression so the message shows the folded value *)
    let shown =
      if is_unsigned te.T.ty then Printf.sprintf "%Lu" n else Int64.to_string n
    in
    if
      Int64.compare n 0x7FFF_FFFFL > 0
      || (Int64.compare n 0L < 0 && is_unsigned te.T.ty)
    then bad ("array size is too large: " ^ shown)
    else if Int64.compare n 0L < 0 then bad ("array size is negative: " ^ shown)
    else Int64.to_int n

(* Main implicitly returns i32 for the C runtime and everything else is void *)
let ret_ty_of (env : env) (fd : func_def) : ty =
  match fd.ret with
  | Some { tdesc = Named "never"; _ } -> TNever
  | Some t -> ty_of_ast env t
  | None -> if fd.name = "main" then TInt I32 else TVoid

(* First pass collecting signatures so that the compiler can handle forward
   references *)
let collect_func (env : env) (fd : func_def) : unit =
  let param_tys = List.map (fun (p : param) -> ty_of_ast env p.typ) fd.params in
  let ret_ty = ret_ty_of env fd in
  Hashtbl.replace env.funcs fd.name
    { param_tys; ret_ty; variadic = fd.variadic }

(* Every kind of type name shares one namespace *)
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

(* The name goes in first so a field can name this struct or one defined
   later *)
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

let resolve_struct_fields (env : env) (sd : struct_def) : unit =
  match Hashtbl.find_opt env.types sd.name with
  | Some (DStruct _) ->
      let field_tys =
        List.map (fun (f : field) -> (f.name, ty_of_ast env f.typ)) sd.fields
      in
      Hashtbl.replace env.types sd.name (DStruct { field_tys });
      Hashtbl.replace env.struct_fields sd.name field_tys
  | _ -> ()

(* A pointer or slice field is just an address so it can't grow the struct *)
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
     | Comptime ->
         emit env (Error.named gd.span "comptime without initializer" gd.name));
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
  (* The collected signature is reused so a bad array size errors once *)
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

  (* Main always returns a 32 bit integer so any other type the user writes is
     rejected *)
  if fd.name = "main" && ret_ty <> TInt I32 then begin
    let span = match fd.ret with Some t -> t.span | None -> fd.span in
    emit env
      (Error.type_mismatch span ~expected:(show_ty (TInt I32))
         ~found:(show_ty ret_ty))
  end;

  let func_env = push_scope { env with ret_ty; in_main = fd.name = "main" } in
  (* An extern has no body so its params can't be used and stay quiet *)
  let param_env =
    List.fold_left
      (fun e (name, t, span) -> extend_var ~used:is_extern e span name t)
      func_env params_typed
  in

  (* A non-void body's trailing value is its return value so it flows against
     the declared return type while main and void bodies just run for effect *)
  let implicit_return =
    (not is_extern) && ret_ty <> TVoid && fd.name <> "main"
  in
  let want_tail = if implicit_return then Some ret_ty else None in
  let final_env, tbody0 = check_block param_env fd.span fd.body want_tail in
  warn_unused_in_scope final_env;
  let tbody =
    match (implicit_return, List.rev tbody0) with
    (* A live value tail returns while a diverging tail already left on its
       own *)
    | true, last :: rest when last.T.ty = ret_ty && ret_ty <> TNever ->
        let ret = T.mk ~span:last.T.span TNever (T.TReturn (Some last)) in
        List.rev (ret :: rest)
    | _ -> tbody0
  in

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
  | T.TErrorExpr -> false
  | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TChar _ | T.TCStr _
  | T.TSizeOf _ ->
      true
  (* A function address is a link time constant *)
  | T.TIdent s ->
      Symbol.is_func s.Symbol.kind || is_const_global env (symbol_name env s)
  (* The address of a global is a link time constant *)
  | T.TUnOp (Ast.AddressOf, { T.desc = T.TIdent s; _ }) ->
      Symbol.is_global s.Symbol.kind
  | T.TUnOp (_, e) -> is_const_texpr env e
  | T.TBinOp (_, l, r) -> is_const_texpr env l && is_const_texpr env r
  | T.TCast (e, _) -> is_const_texpr env e
  | T.TZero -> true
  (* An array literal is constant when all its elements are *)
  | T.TArrayLit elems -> List.for_all (is_const_texpr env) elems
  | T.TStructLit (_, fields) ->
      List.for_all (fun (_, fe) -> is_const_texpr env fe) fields
  (* Never compile-time by design *)
  | T.TCall _ | T.TFieldAccess _ | T.TRange _ | T.TRangeInclusive _ | T.TIndex _
  | T.TLen _ | T.TToSlice _ | T.TSliceExpr _ | T.TDataPtr _ | T.TBlock _
  | T.TIf _ | T.TWhile _ | T.TFor _ | T.TBinding _ | T.TReturn _ | T.TBreak
  | T.TContinue ->
      false
  | T.TUndef -> true
  | T.TPairAssign _ -> false

let check_global (env : env) (gd : global_def) : T.tglobal_def =
  (* The collected type is reused so a bad array size errors once *)
  let t =
    match Hashtbl.find_opt env.globals gd.name with
    | Some (t, _) -> t
    | None -> ty_of_ast env gd.typ
  in
  if gd.kind = Comptime then check_const_scalar env gd.span t;
  let tinit =
    match gd.init with
    | None -> None
    | Some { desc = Undefined; span } when gd.kind = Comptime ->
        emit env
          Diagnostic.(
            error "comptime cannot be undefined"
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
      (* A rejected duplicate never landed in the table and its fields are read
         directly *)
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
  (* A rejected duplicate never landed in the table and falls back to the
     written type *)
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
    ~local_value:(fun symbol -> Hashtbl.find_opt env.l_vals (Symbol.key symbol))
    ~global_value:(fun symbol ->
      let name = symbol_name env symbol in
      if is_comptime_global env name then
        match Hashtbl.find_opt env.g_state name with
        | Some { value = Some v; _ } -> Some v
        | _ -> None
      else None)
    ~fold_num:(fold_num env) tdecls

(* Hands warnings back for the edge to render and blows up on any error *)
let typecheck (uses : Resolve.t) (decls : decl list) :
    T.tdecl list * Diagnostic.t list =
  let env = make_env uses in
  (* An early array size can demand any later const so defs go in first *)
  List.iter
    (function
      | Global ({ kind = Let | Comptime; _ } as gd) ->
          Hashtbl.replace env.g_state gd.name
            { def = gd; typed = None; value = None; busy = false }
      | _ -> ())
    decls;
  List.iter (collect_decl env) decls;
  List.iter (fill_struct_fields_decl env) decls;
  List.iter (check_cycle_decl env) decls;
  let tdecls = List.map (check_decl env) decls in
  fold_consts env tdecls
