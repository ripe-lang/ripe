(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast
open Types
open Ty_pred
module T = Typed_ast

type func_sig = {
  param_tys : ty list;
  ret_ty : ty;
  variadic : bool;
  abi : Types.func_abi;
}

type struct_info = { field_tys : (Ast.name * ty) list }

(* Structs newtypes aliases and builtins share one namespace of type names *)
type type_def =
  | DStruct of struct_info
  | DNewtype of ty
  | DAlias of ty
  | DBuiltin of Types.builtin

type result_use = Infer | Expect of ty | Discard
type var_info = { name : Ast.name; ty : ty; used : bool ref; span : Ast.span }

(* The typed and value fields only ever go from None to Some so nothing rolls back *)
type gstate = {
  def : global_def;
  mutable typed : T.texpr option;
  mutable value : Const_eval.const_num option;
  (* Busy means this global is mid evaluation so a self demand is a cycle *)
  mutable busy : bool;
}

type loop_ctx = {
  lbl : Ast.name option;
  valued : bool;
  mutable result : (ty * Ast.span) option;
  mutable bare_break : Ast.span option;
}

type env = {
  vars : (Symbol.key * var_info) list list;
  funcs : func_sig Symbol.Table.t;
  types : type_def Symbol.Table.t;
  (* Struct field layouts mirror the DStruct entries in types so ty_size need not rebuild them *)
  struct_fields : ty list Symbol.Table.t;
  globals : (ty * Ast.binding_kind) Symbol.Table.t;
  (* Constants evaluate on demand so an array size may name a later const *)
  g_state : gstate Symbol.Table.t;
  l_vals : Const_eval.const_num Symbol.Table.t;
  ret_ty : ty;
  loops : loop_ctx list;
  in_main : bool;
  suppress_warnings : bool;
  (* Whoever reads the message is inside this module so its path drops out *)
  reader_path : string list;
  diags : Diagnostic.sink;
  uses : Resolve.t;
}

let make_env (diags : Diagnostic.sink) (uses : Resolve.t) : env =
  let types = Symbol.Table.create 16 in
  let seed (key, builtin) = Symbol.Table.replace types key (DBuiltin builtin) in
  List.iter seed (Resolve.builtins uses);
  {
    vars = [];
    funcs = Symbol.Table.create 16;
    types;
    struct_fields = Symbol.Table.create 16;
    globals = Symbol.Table.create 16;
    g_state = Symbol.Table.create 16;
    l_vals = Symbol.Table.create 16;
    ret_ty = TVoid;
    loops = [];
    in_main = false;
    suppress_warnings = Diagnostic.has_errors diags;
    reader_path = [];
    diags;
    uses;
  }

let show_ty (env : env) (t : ty) : string = Types.show_ty_in env.reader_path t

let decl_span : decl -> Ast.span = function
  | Func fd | Extern fd -> fd.func_span
  | Global gd -> gd.span
  | Struct sd -> sd.struct_span
  | TypeAlias td | Newtype td -> td.alias_span

(* The path comes off the declaration being checked not off the root module *)
let reading (env : env) (decl : decl) : env =
  { env with reader_path = Resolve.module_path_at env.uses (decl_span decl) }

(* The two fields every slice and string answers to, interned once *)
let len_name : Ast.name = Interner.intern "len"
let ptr_name : Ast.name = Interner.intern "ptr"
let dummy_const_num = Const_eval.Ni32 0l
let sym (env : env) (span : Ast.span) : Symbol.t = Resolve.sym_at env.uses span
let emit (env : env) (d : Diagnostic.t) : unit = Diagnostic.emit env.diags d
let add_error (env : env) span msg = Diagnostic.emit_error_at env.diags span msg
let dummy_texpr = T.mk TError T.TErrorExpr

let add_warning (env : env) (span : Ast.span) (msg : string) : unit =
  if not env.suppress_warnings then Diagnostic.emit_warn_at env.diags span msg

let push_scope (env : env) : env = { env with vars = [] :: env.vars }

let warn_unused_in_scope (env : env) : unit =
  match env.vars with
  | scope :: _ when not env.suppress_warnings ->
      List.iter
        (fun (_, (info : var_info)) ->
          let shown = Interner.text info.name in
          (* Variables prefixed with '_' suppress unused warnings *)
          if (not !(info.used)) && shown.[0] <> '_' then
            emit env
              (Diagnostic.warning (Printf.sprintf "unused variable: %s" shown)
              |> Diagnostic.at info.span
              |> Diagnostic.help
                   (Printf.sprintf "prefix with an underscore: _%s" shown)))
        scope
  | _ -> ()

let extend_var ?(used = false) ?(deduplicate = false) (env : env)
    (span : Ast.span) (name : Ast.name) (t : ty) : env =
  let key = Symbol.key (sym env span) in
  let info = { name; ty = t; used = ref used; span } in
  match env.vars with
  | [] -> assert false (* No active scope *)
  | scope :: _ when deduplicate && List.mem_assoc key scope -> env
  | scope :: rest -> { env with vars = ((key, info) :: scope) :: rest }

let lookup_var_opt (env : env) (span : Ast.span) : ty option =
  let key = Symbol.key (sym env span) in
  let rec search = function
    | [] -> None
    | scope :: rest -> (
        match List.assoc_opt key scope with
        | Some info ->
            info.used := true;
            Some info.ty
        | None -> search rest)
  in
  search env.vars

(* Nothing got resolved here so this key never matches a real entry *)
let unresolved_key : Symbol.key = Symbol.make_key (-1) (-1)

(* Two declarations can go by one name so lookups key on which one it is *)
let key_at (env : env) (span : Ast.span) : Symbol.key =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> Symbol.key symbol
  | None -> unresolved_key

let builtin_at (env : env) (span : Ast.span) : Types.builtin option =
  match Symbol.Table.find_opt env.types (key_at env span) with
  | Some (DBuiltin b) -> Some b
  | Some (DStruct _ | DNewtype _ | DAlias _) | None -> None

(* The path comes off the symbol so a message can say which module a type is from *)
let qname_at (env : env) (span : Ast.span) (fallback : string) : Qname.t =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> Resolve.qname_of env.uses symbol
  | None -> Qname.unresolved fallback

(* What the linker calls this declaration was worked out once by the resolver *)
let link_name_at (env : env) (span : Ast.span) (fallback : string) : string =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> symbol.Symbol.link_name
  | None -> fallback

let is_entry (env : env) (span : Ast.span) : bool =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> symbol.Symbol.entry_point
  | None -> false

let lookup_func (env : env) (span : Ast.span) : func_sig =
  match Symbol.Table.find_opt env.funcs (key_at env span) with
  | Some s -> s
  | None ->
      emit env (Diagnostic.undefined_name span "function");
      { param_tys = []; ret_ty = TVoid; variadic = false; abi = Types.Ripe }

let is_const_global (env : env) (key : Symbol.key) : bool =
  match Symbol.Table.find_opt env.globals key with
  | Some (_, (Let | Comptime)) -> true
  | _ -> false

let is_comptime_global (env : env) (key : Symbol.key) : bool =
  match Symbol.Table.find_opt env.globals key with
  | Some (_, Comptime) -> true
  | _ -> false

let check_const_scalar (env : env) (span : Ast.span) (t : ty) : unit =
  if not (is_scalar t) then
    emit env
      (Diagnostic.with_type span "comptime must be a scalar" (show_ty env t)
      |> Diagnostic.help "use let for values that need storage")

let lookup_struct (env : env) (span : Ast.span) (name : Qname.t) : struct_info =
  match Symbol.Table.find_opt env.types (Qname.key name) with
  | Some (DStruct s) -> s
  | _ ->
      emit env (Diagnostic.undefined_name span "struct");
      { field_tys = [] }

let lift_ty (f : ty -> ty) (ty : ty) : ty =
  match ty with TError -> TError | ty -> f ty

(* A signature without an ABI written on it is a plain Ripe function *)
let resolve_abi (env : env) (a : Ast.abi) : Types.func_abi =
  match a with
  | NoAbi -> Types.Ripe
  | AbiError -> Types.AbiError
  | NamedAbi (name, span) -> (
      match Types.func_abi_of_name name with
      | Some abi -> abi
      | None ->
          emit env (Diagnostic.unsupported_abi span);
          Types.AbiError)

let rec ty_of_ast (env : env) (t : typ) : ty =
  match t.tdesc with
  | ErrorType -> TError
  | Named (path, name) -> (
      let shown = Ast.show_named path name in
      match Symbol.Table.find_opt env.types (key_at env t.tspan) with
      (* Never is the return type of a function that can't return so no value ever has it *)
      | Some (DBuiltin (BTy TNever)) ->
          emit env
            Diagnostic.(
              error "never is only valid as a function return type"
              |> at t.tspan
              |> help "a value of type never cannot exist");
          TError
      | Some (DBuiltin BOpaque) ->
          emit env
            Diagnostic.(
              error "opaque is only valid as a pointee"
              |> at t.tspan
              |> help "use *opaque for an untyped pointer");
          TError
      | Some (DBuiltin (BTy ty)) -> ty
      | Some (DStruct _) -> TStruct (qname_at env t.tspan shown, [])
      | Some (DNewtype base) -> TNewtype (qname_at env t.tspan shown, base)
      | Some (DAlias aliased) -> TAlias (qname_at env t.tspan shown, aliased)
      | None -> (
          match Resolve.sym_at_opt env.uses t.tspan with
          | Some { Symbol.kind = Symbol.Error; _ } -> TError
          | _ -> Diagnostic.ice ~span:t.tspan "type name escaped the resolver"))
  | Pointer inner when builtin_at env inner.tspan = Some BOpaque -> TOpaquePtr
  | Pointer t -> lift_ty (fun ty -> TPointer ty) (ty_of_ast env t)
  | Array (e, t) -> (
      match ty_of_ast env t with
      | TError -> TError
      | ty ->
          if e.desc = ErrorExpr then TError
          else TArray (ty, eval_array_size env e))
  | Slice t -> lift_ty (fun ty -> TSlice ty) (ty_of_ast env t)
  | FuncPtr (abi, ps, ret) -> (
      let pts = List.map (ty_of_ast env) ps in
      let rt =
        match ret with Some t -> return_ty_of_ast env t | None -> TVoid
      in
      match (resolve_abi env abi, rt, List.mem TError pts) with
      | Types.AbiError, _, _ -> TError
      | abi, rt, false when rt <> TError -> TFunc (pts, rt, abi)
      | _ -> TError)

and return_ty_of_ast (env : env) (t : typ) : ty =
  match builtin_at env t.tspan with
  | Some (BTy TNever) -> TNever
  | Some (BTy _) | Some BOpaque | None -> ty_of_ast env t

and lookup_var (env : env) (span : Ast.span) : ty =
  match lookup_var_opt env span with
  | Some t -> t
  | None -> (
      let key = key_at env span in
      (* Locals shadow globals shadow functions *)
      match Symbol.Table.find_opt env.globals key with
      | Some (t, _) -> t
      | None -> (
          (* An array size may name a global not collected yet so type it now *)
          match Symbol.Table.find_opt env.g_state key with
          (* A global whose own size names it would loop forever *)
          | Some st when st.busy ->
              emit env (Diagnostic.error_at span "cyclic constant");
              TError
          | Some st ->
              st.busy <- true;
              let t = ty_of_ast env st.def.typ in
              st.busy <- false;
              Symbol.Table.replace env.globals key (t, st.def.kind);
              t
          | None -> (
              (* Fall back to function table so function names can be used as values *)
              match Symbol.Table.find_opt env.funcs key with
              | Some sg -> TFunc (sg.param_tys, sg.ret_ty, sg.abi)
              | None ->
                  emit env (Diagnostic.undefined_name span "variable");
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
        emit env
          (Diagnostic.int_out_of_range e.span ~ty:(show_ty env (TInt kind)));
      T.mk (TInt kind) (T.TInt n)
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      synth env { operand with span = e.span }
  | UnOp (Neg, { desc = Int (n, Some s); _ }) ->
      let kind = suffix_kind s in
      if Int64.unsigned_compare n (int_kind_neg_limit kind) > 0 then
        emit env
          (Diagnostic.int_out_of_range e.span ~ty:(show_ty env (TInt kind)));
      T.mk (TInt kind) (T.TInt (Int64.neg n))
  | Float f -> T.mk (TFloat F64) (T.TFloat f)
  | Bool b -> T.mk TBool (T.TBool b)
  | Null -> T.mk TNull T.TNull
  | String s -> T.mk (TPointer (TInt I8)) (T.TCStr s)
  | Char c -> T.mk TChar (T.TChar c)
  | Ident _ ->
      let s = sym env e.span in
      if s.Symbol.kind = Symbol.Error then dummy_texpr
      else if s.Symbol.kind = Symbol.Module then (
        emit env (Diagnostic.error_at e.span "module requires a member");
        dummy_texpr)
      else
        let t = lookup_var env e.span in
        T.mk t (T.TIdent s)
  | Call (callee, args) -> synth_call env e.span callee args
  | BinOp (op, l, r) -> synth_binop env op l r
  | UnOp (op, e) -> synth_unop env op e
  (* A qualified name is one symbol so the module in front of it isn't a value *)
  | FieldAccess (inner_e, fname, fspan) -> (
      match Resolve.sym_at_opt env.uses e.span with
      | Some s when s.Symbol.kind = Symbol.Error -> dummy_texpr
      | Some s
        when Symbol.is_func s.Symbol.kind || Symbol.is_global s.Symbol.kind ->
          T.mk (lookup_var env e.span) (T.TIdent s)
      | _ -> synth_field env e.span inner_e fname fspan)
  | Cast (operand, t, kind) ->
      let te = synth env operand in
      let ty = ty_of_ast env t in
      if not (cast_ok te.T.ty ty) then begin
        let d =
          Diagnostic.error "invalid cast"
          |> Diagnostic.at e.span
          |> Diagnostic.label
               (Printf.sprintf "cannot cast %s to %s" (show_ty env te.T.ty)
                  (show_ty env ty))
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
        | TError, _ | _, TError -> ()
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
      if te.T.ty = TError || ty = TError then dummy_texpr
      else T.mk ty (T.TCast (te, kind))
  | SizeOf t -> (
      match ty_of_ast env t with
      | TError -> dummy_texpr
      | ty -> T.mk (TInt Usize) (T.TSizeOf ty))
  (* Ranges are not first-class values and only work as for-loop iterators or slice bounds *)
  | Range _ | RangeInclusive _ | RangeFrom _ | RangeTo _ | RangeToInclusive _
  | RangeFull ->
      add_error env e.span "range is only valid in a for loop or slice";
      dummy_texpr
  | ArrayLit [] ->
      add_error env e.span "cannot infer type of empty array literal";
      dummy_texpr
  | ArrayLit (e0 :: rest) ->
      let te0 = synth env e0 in
      let elem =
        match te0.T.ty with
        | (TVoid | TNever) as t ->
            add_error env e0.span
              (Printf.sprintf "array element cannot have type %s"
                 (show_ty env t));
            TError
        | t -> t
      in
      let tes = te0 :: List.map (fun e -> check env e elem) rest in
      T.mk (TArray (elem, List.length tes)) (T.TArrayLit tes)
  | Index (base, idx) -> synth_index env e.span base idx
  | Undefined ->
      add_error env e.span "cannot infer type of undefined";
      dummy_texpr
  | StructLit (path, name, name_span, inits) -> (
      match Symbol.Table.find_opt env.types (key_at env name_span) with
      | Some (DStruct info) ->
          let seen = Hashtbl.create 4 in
          List.iter
            (fun (fname, fspan, _) ->
              if not (List.mem_assoc fname info.field_tys) then
                emit env (Diagnostic.error_at fspan "no field")
              else if Hashtbl.mem seen fname then
                emit env (Diagnostic.error_at fspan "duplicate field")
              else Hashtbl.replace seen fname ())
            inits;
          (* Omitted fields are zero-initialized *)
          let tfields =
            List.mapi
              (fun field_id (fname, ft) ->
                match
                  List.find_map
                    (fun (n, _, e) -> if n = fname then Some e else None)
                    inits
                with
                | Some e -> (field_id, check env e ft)
                | None -> (field_id, T.mk ft T.TZero))
              info.field_tys
          in
          let qname = qname_at env name_span (Ast.show_named path name) in
          T.mk (TStruct (qname, [])) (T.TStructLit (qname, tfields))
      | _ ->
          emit env (Diagnostic.undefined_name name_span "struct");
          dummy_texpr)
  | Block body ->
      let tb, ty = check_scoped_block env e.span body Infer in
      T.mk ty (T.TBlock tb)
  | If (branches, else_body) -> check_if env e.span branches else_body None
  | While (label, cond, body) ->
      let tc = check env cond TBool in
      let loop = new_loop label ~valued:false in
      let tb, _ = check_scoped_block ~loop env e.span body Discard in
      (* A while true with no break loops forever so it never yields control *)
      let diverges =
        match cond.desc with
        | Bool true -> not (Reachability.loop_has_break ?label body)
        | _ -> false
      in
      T.mk (if diverges then TNever else TVoid) (T.TWhile (label, tc, tb))
  | For (label, name, nspan, iter, body) ->
      synth_for env e.span label name nspan iter body
  | Binding (kind, name, nspan, ann, init) ->
      snd (check_binding env kind name nspan ann init)
  | Return init -> synth_return env e.span init
  | Break (label, value) -> synth_break env e.span label value
  | Continue label ->
      ignore (check_loop_target env e.span "`continue` outside a loop" label);
      T.mk TNever (T.TContinue label)
  | PairAssign (ft, st, fv, sv) -> synth_pair_assign env ft st fv sv
  | Loop (label, body) ->
      let loop = new_loop label ~valued:true in
      let tb, _ = check_scoped_block ~loop env e.span body Discard in
      let ty =
        match (loop.result, loop.bare_break) with
        | Some (t, _), _ -> t
        | None, Some _ -> TVoid
        | None, None -> TNever
      in
      T.mk ty (T.TLoop (label, tb))

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
      && List.for_all
           (fun (_, { Ast.value = body; _ }) -> block_is_flexible body)
           branches
      && Option.fold ~none:false
           ~some:(fun { Ast.value = body; _ } -> block_is_flexible body)
           else_body
  | _ -> false

and block_is_flexible (body : block) : bool =
  match List.rev body with
  | Expr last :: _ -> arm_is_flexible last
  | Decl _ :: _ | [] -> false

(* Probe a block's result type with diagnostics muted so a sibling can anchor it *)
and block_result_ty (env : env) (body : block) : ty =
  let quiet = { env with diags = Diagnostic.sink () } in
  let inner = push_scope quiet in
  let _, tb = check_block inner Ast.dummy_span body Infer in
  tblock_ty tb

and reconcile_if_result (env : env) (branches : (expr * block Ast.spanned) list)
    (else_b : block) : ty =
  let arms =
    List.map (fun (_, { Ast.value; _ }) -> value) branches @ [ else_b ]
  in
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

and is_unused_operation (e : expr) : bool =
  match e.desc with
  | BinOp (op, _, _) -> not (Ast.is_assignment_op op)
  | UnOp _ | Cast _ -> true
  | _ -> false

and warn_discarded_operation (env : env) (e : expr) (te : T.texpr) : unit =
  if
    (not env.suppress_warnings)
    && (not (Diagnostic.has_errors env.diags))
    && is_unused_operation e && te.T.ty <> TVoid && te.T.ty <> TNever
    && te.T.ty <> TError
  then
    emit env
      (Diagnostic.warning "discarded operation result"
      |> Diagnostic.at te.T.span
      |> Diagnostic.help "use `let _ = ...` when this is intentional")

and check_void_result (env : env) (span : Ast.span) : result_use -> unit =
  function
  | Expect want when strip_alias want <> TVoid ->
      emit env
        (Diagnostic.type_mismatch span ~expected:(show_ty env want)
           ~found:(show_ty env TVoid))
  | Infer | Discard | Expect _ -> ()

and check_value_for_use (env : env) (e : expr) : result_use -> T.texpr =
  function
  | Infer -> synth env e
  | Expect want -> check env e want
  | Discard -> (
      match e.desc with
      (* Nothing reads the result so the arms have no reason to agree *)
      | If (branches, else_body) ->
          {
            (check_if_discarded env e.span branches else_body) with
            T.span = e.span;
          }
      | _ ->
          let te = synth env e in
          warn_discarded_operation env e te;
          te)

(* Thread env so a binding is visible to later elements *)
and check_block (env : env) (span : Ast.span) (body : block) (use : result_use)
    : env * T.tblock =
  let rec go env diverged acc (elems : block_item list) =
    match elems with
    | [] ->
        check_void_result env span use;
        (env, List.rev acc)
    | [ last ] ->
        (* A dead tail keeps only its warning and its type need not match *)
        let tail_use = if diverged then Infer else use in
        if diverged then
          add_warning env (block_item_span last) "unreachable code";
        let env, te = check_elem env last tail_use in
        (env, List.rev (te :: acc))
    | e :: rest ->
        if diverged then add_warning env (block_item_span e) "unreachable code";
        let elem_use = if diverged then Infer else Discard in
        let env, te = check_elem env e elem_use in
        go env (diverged || te.T.ty = TNever) (te :: acc) rest
  in
  go env false [] body

and block_item_span = function
  | Expr e -> e.span
  | Decl d -> decl_span (decl_of_local d)

and check_elem (env : env) (item : block_item) (use : result_use) :
    env * T.texpr =
  match item with
  | Decl _ ->
      check_void_result env (block_item_span item) use;
      (env, T.mk TVoid T.TLocalDecl)
  | Expr ({ desc = Binding (kind, name, nspan, ann, init); _ } as e) ->
      let env', tb = check_binding env kind name nspan ann init in
      check_void_result env e.span use;
      (env', tb)
  | Expr e -> (env, check_value_for_use env e use)

(* Push a scope for the block then flag any leftover bindings *)
and check_scoped_block ?loop (env : env) (span : Ast.span) (body : block)
    (use : result_use) : T.tblock * ty =
  let base =
    match loop with
    | Some lc -> { env with loops = lc :: env.loops }
    | None -> env
  in
  let inner = push_scope base in
  let final_inner, tb = check_block inner span body use in
  warn_unused_in_scope final_inner;
  (tb, tblock_ty tb)

and check_binding (env : env) (kind : Ast.binding_kind) (name : Ast.name)
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
          emit env (Diagnostic.error_at e.span "cannot bind void value");
          (TError, te))
        else (te.T.ty, te)
    | Some a, None ->
        let want = ty_of_ast env a in
        (want, T.mk want T.TZero)
    | None, None ->
        emit env (Diagnostic.error_at nspan "cannot infer type");
        (* A real type here makes every later use mismatch against a type nobody wrote *)
        (TError, dummy_texpr)
  in
  if kind = Comptime then (
    check_const_scalar env nspan t;
    Symbol.Table.replace env.l_vals
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

and new_loop (label : Ast.loop_label option) ~(valued : bool) : loop_ctx =
  {
    lbl = Option.map (fun (l : Ast.loop_label) -> l.Ast.value) label;
    valued;
    result = None;
    bare_break = None;
  }

and find_loop (env : env) (label : Ast.loop_label option) : loop_ctx option =
  match label with
  | None -> ( match env.loops with lc :: _ -> Some lc | [] -> None)
  | Some l -> List.find_opt (fun lc -> lc.lbl = Some l.Ast.value) env.loops

and check_loop_target (env : env) (span : Ast.span) (headline : string)
    (label : Ast.loop_label option) : loop_ctx option =
  let found = find_loop env label in
  (match (found, label) with
  | None, None -> add_error env span headline
  | None, Some l -> emit env (Diagnostic.undefined_name l.Ast.span "loop label")
  | Some _, _ -> ());
  found

and check_bare_break (env : env) (span : Ast.span) (lc : loop_ctx) : unit =
  if lc.bare_break = None then lc.bare_break <- Some span;
  match lc.result with
  | None -> ()
  | Some (t, first) ->
      emit env
        (Diagnostic.error "`break` values disagree"
        |> Diagnostic.at span
        |> Diagnostic.label "no value here"
        |> Diagnostic.secondary first
             (Printf.sprintf "breaks with %s" (show_ty env t)))

(* The first valued break fixes the type and the rest have to match it *)
and check_valued_break (env : env) (lc : loop_ctx) (ve : expr) : T.texpr =
  match lc.result with
  | Some (want, _) -> check env ve want
  | None ->
      let te = synth env ve in
      let disagrees first =
        emit env
          (Diagnostic.error "`break` values disagree"
          |> Diagnostic.at ve.span
          |> Diagnostic.label
               (Printf.sprintf "breaks with %s" (show_ty env te.T.ty))
          |> Diagnostic.secondary first "no value here")
      in
      Option.iter disagrees lc.bare_break;
      lc.result <- Some (te.T.ty, ve.span);
      te

and synth_break (env : env) (span : Ast.span) (label : Ast.loop_label option)
    (value : expr option) : T.texpr =
  let target = check_loop_target env span "`break` outside a loop" label in
  let tv =
    match (value, target) with
    | None, None -> None
    | None, Some lc ->
        check_bare_break env span lc;
        None
    | Some ve, None -> Some (synth env ve)
    | Some ve, Some lc when not lc.valued ->
        emit env
          (Diagnostic.error "`break` with a value outside a `loop`"
          |> Diagnostic.at ve.span
          |> Diagnostic.help "use `loop` when the loop produces a value");
        Some (synth env ve)
    | Some ve, Some lc -> Some (check_valued_break env lc ve)
  in
  T.mk TNever (T.TBreak (label, tv))

and synth_for (env : env) (span : Ast.span) (label : Ast.loop_label option)
    (name : Ast.name) (nspan : Ast.span) (iter : expr) (body : block) : T.texpr
    =
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
              (Diagnostic.with_type iter.span "cannot iterate" (show_ty env t));
            (ti, TInt I32))
  in
  let loop = new_loop label ~valued:false in
  let inner = push_scope { env with loops = loop :: env.loops } in
  let inner = extend_var inner nspan name elem_ty in
  let final_inner, tb = check_block inner span body Discard in
  warn_unused_in_scope final_inner;
  T.mk TVoid (T.TFor (label, sym env nspan, elem_ty, titer, tb))

(* An arm can end in a call whose value nobody wanted so each one is checked on its own *)
and check_if_discarded (env : env) (span : Ast.span)
    (branches : (expr * block Ast.spanned) list)
    (else_body : block Ast.spanned option) : T.texpr =
  let arm body = fst (check_scoped_block env span body Discard) in
  let tbranches =
    List.map
      (fun (c, { Ast.value = body; _ }) -> (check env c TBool, arm body))
      branches
  in
  let telse = Option.map (fun { Ast.value = body; _ } -> arm body) else_body in
  let arm_tys =
    (match telse with Some tb -> [ tblock_ty tb ] | None -> [])
    @ List.map (fun (_, tb) -> tblock_ty tb) tbranches
  in
  (* Every arm diverges so whatever follows is unreachable *)
  let ty =
    if Option.is_some telse && List.for_all (fun t -> t = TNever) arm_tys then
      TNever
    else TVoid
  in
  T.mk ty (T.TIf (tbranches, telse))

(* One if handles both a value and a plain statement and want None means synthesize *)
and check_if (env : env) (span : Ast.span)
    (branches : (expr * block Ast.spanned) list)
    (else_body : block Ast.spanned option) (want : ty option) : T.texpr =
  match (want, else_body) with
  | None, None ->
      let tbranches =
        List.map
          (fun (c, { Ast.value = body; _ }) ->
            (check env c TBool, fst (check_scoped_block env span body Discard)))
          branches
      in
      T.mk TVoid (T.TIf (tbranches, None))
  | None, Some { Ast.value = else_b; _ } ->
      let rty = reconcile_if_result env branches else_b in
      check_if env span branches else_body (Some rty)
  | Some w, _ ->
      (* Each arm answers for its own value so the caret lands on the one that missed *)
      let tbranches =
        List.map
          (fun (c, { Ast.value = body; span = bspan }) ->
            ( check env c TBool,
              fst (check_scoped_block env bspan body (Expect w)) ))
          branches
      in
      let telse =
        match else_body with
        | Some { Ast.value = body; span = bspan } ->
            Some (fst (check_scoped_block env bspan body (Expect w)))
        | None ->
            if strip_alias w <> TVoid then
              emit env
                (Diagnostic.type_mismatch span ~expected:(show_ty env w)
                   ~found:(show_ty env TVoid));
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
        Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
          ~found:(show_ty env got)
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
      (* The suffix already picked the type so a wrong target is an error not a quiet coercion *)
      let te = synth_desc env e in
      if not (strict_eq (strip_alias want) te.T.ty) then
        emit env
          (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
             ~found:(show_ty env te.T.ty));
      te
  | Int (n, None) -> (
      (* An untyped literal adopts a newtype over an int and checks its base *)
      match target with
      | TInt kind ->
          if Int64.unsigned_compare n (int_kind_pos_limit kind) > 0 then
            emit env
              (Diagnostic.int_out_of_range e.span
                 ~ty:(show_ty env (resolve_ty want)));
          T.mk want (T.TInt n)
      | TError -> T.mk want (T.TInt n)
      (* Want is not an integer type at all e.g. let y: bool = 20 *)
      | _ ->
          emit env
            (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
               ~found:"i32");
          T.mk (TInt I32) (T.TInt n))
  | Float f -> (
      match target with
      | TFloat _ -> T.mk want (T.TFloat f)
      | TError -> T.mk want (T.TFloat f)
      | _ ->
          emit env
            (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
               ~found:"f64");
          T.mk (TFloat F64) (T.TFloat f))
  (* The whole source file is already checked so a literal can't hold bad UTF 8 *)
  | String str -> (
      match target with
      | TStr -> T.mk want (T.TStr str)
      | _ -> check_by_synth ())
  | UnOp (Neg, { desc = Int (n, None); _ }) -> (
      match target with
      | TInt kind ->
          if Int64.unsigned_compare n (int_kind_neg_limit kind) > 0 then
            emit env
              (Diagnostic.int_out_of_range e.span
                 ~ty:(show_ty env (resolve_ty want)));
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
              (Diagnostic.arity e.span
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
      let tb, ty = check_scoped_block env e.span body (Expect want) in
      T.mk ty (T.TBlock tb)
  | If (branches, else_body) ->
      check_if env e.span branches else_body (Some want)
  | Undefined -> T.mk want T.TUndef
  | _ -> check_by_synth ()

and check_matching_operands (env : env) (l : expr) (r : expr) :
    T.texpr * T.texpr * ty =
  if is_int_literal l && not (is_int_literal r) then
    let tr = synth env r in
    let t = tr.T.ty in
    (check env l t, tr, t)
  else
    let tl = synth env l in
    let t = tl.T.ty in
    (tl, check env r t, t)

and check_range_bounds (env : env) (lo : expr) (hi : expr) =
  let tlo, thi, t = check_matching_operands env lo hi in
  if not (is_integer t) then
    add_error env lo.span "range bounds must be integers";
  (tlo, thi, t)

and check_args (env : env) (span : Ast.span) (sig_ : func_sig)
    (args : expr list) : T.texpr list =
  let n_params = List.length sig_.param_tys in
  let n_args = List.length args in
  let expected_arguments =
    Printf.sprintf "%d argument%s" n_params (if n_params = 1 then "" else "s")
  in
  if sig_.variadic then
    if n_args < n_params then (
      emit env
        (Diagnostic.arity span
           ~expected:("expected at least " ^ expected_arguments)
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
      (Diagnostic.arity span
         ~expected:("expected " ^ expected_arguments)
         ~found:n_args);
    [])
  else List.map2 (check env) args sig_.param_tys

and synth_binop (env : env) (op : binop) (l : expr) (r : expr) : T.texpr =
  match op with
  | Add | Sub | Mul | Div | Mod ->
      let tl, tr, t = check_matching_operands env l r in
      if not (is_numeric t) then
        emit env
          (Diagnostic.bad_operand l.span ~op:(show_binop_sym op)
             ~ty:(show_ty env t));
      (* QBE has no float remainder instruction *)
      if op = Mod && match strip_alias t with TFloat _ -> true | _ -> false
      then emit env (Diagnostic.bad_operand l.span ~op:"%" ~ty:(show_ty env t));
      T.mk t (T.TBinOp (op, tl, tr))
  | Eq | Neq ->
      let tl = synth env l in
      let t = if tl.T.ty = TNull then (synth env r).T.ty else tl.T.ty in
      if not (is_comparable t) then
        emit env
          (Diagnostic.bad_operand l.span ~op:(show_binop_sym op)
             ~ty:(show_ty env t));
      let tl = if tl.T.ty = TNull then check env l t else tl in
      let tr = check env r t in
      T.mk TBool (T.TBinOp (op, tl, tr))
  | Lt | Gt | Lte | Gte ->
      let tl = synth env l in
      let t = if tl.T.ty = TNull then (synth env r).T.ty else tl.T.ty in
      if not (is_ordered t) then
        emit env
          (Diagnostic.bad_operand l.span ~op:(show_binop_sym op)
             ~ty:(show_ty env t));
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
          (Diagnostic.bad_operand l.span ~op:(show_binop_sym op)
             ~ty:(show_ty env t));
      let tr =
        match op with
        (* The count keeps its own integer type since it is only a number of positions *)
        | Lshift | Rshift ->
            let tr = synth env r in
            if not (is_integer tr.T.ty) then
              add_error env r.span
                (Printf.sprintf "shift count must be an integer, found %s"
                   (show_ty env tr.T.ty));
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
  check_param_copy_write env l.span tl;
  (match tl.T.desc with
  | T.TIdent s when Symbol.is_func s.Symbol.kind ->
      emit env (Diagnostic.error_at l.span "cannot assign to function")
  | T.TIdent _ | T.TFieldAccess _ | T.TIndex _ -> (
      (* This catches assignment to an immutable binding whether it's local or global. *)
      match root_binding tl with
      | Some s
        when Symbol.is_immutable s.Symbol.kind
             || Symbol.is_global s.Symbol.kind
                && is_const_global env (Symbol.key s) ->
          emit env (Diagnostic.error_at l.span "cannot assign to immutable")
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
    emit env
      (Diagnostic.bad_operand l.span ~op:(show_binop_sym op) ~ty:(show_ty env t));
  let tr =
    match op with
    (* The count keeps its own type since it's just how far to shift *)
    | LshiftAssign | RshiftAssign ->
        let tr = synth env r in
        if not (is_integer tr.T.ty) then
          add_error env r.span
            (Printf.sprintf "shift count must be an integer, found %s"
               (show_ty env tr.T.ty));
        tr
    | _ -> check env r t
  in
  (tl, tr)

(* An array or struct parameter arrives as a copy so the caller never sees the write *)
and check_param_copy_write (env : env) (span : Ast.span) (tl : T.texpr) : unit =
  match root_lvalue tl with
  | Some { T.desc = T.TIdent s; ty; _ }
    when s.Symbol.kind = Symbol.Param
         && match resolve_ty ty with TArray _ | TStruct _ -> true | _ -> false
    ->
      emit env
        (Diagnostic.error_at span "cannot assign to a by value parameter"
        |> Diagnostic.label "the caller keeps its own copy"
        |> Diagnostic.help
             (Printf.sprintf "take a pointer to write through it: %s: *%s"
                s.Symbol.name (show_ty env ty)))
  | Some _ | None -> ()

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
        emit env (Diagnostic.bad_operand e.span ~op:"+" ~ty:(show_ty env t));
      T.mk t (T.TUnOp (op, te))
  | Neg ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_numeric t) then
        emit env (Diagnostic.bad_operand e.span ~op:"-" ~ty:(show_ty env t));
      T.mk t (T.TUnOp (op, te))
  | Not ->
      let te = check env e TBool in
      T.mk TBool (T.TUnOp (op, te))
  | BitNot ->
      let te = synth env e in
      let t = te.T.ty in
      if not (is_integer t) then
        emit env (Diagnostic.bad_operand e.span ~op:"~" ~ty:(show_ty env t));
      T.mk t (T.TUnOp (op, te))
  | Deref -> (
      let te = synth env e in
      match strip_alias te.T.ty with
      | TPointer inner -> T.mk inner (T.TUnOp (op, te))
      | TError -> dummy_texpr
      | TOpaquePtr ->
          emit env (Diagnostic.opaque_operation e.span "dereference");
          dummy_texpr
      | t ->
          emit env
            (Diagnostic.with_type e.span "cannot dereference" (show_ty env t));
          dummy_texpr)
  | AddressOf ->
      let te = synth env e in
      (match te.T.desc with
      | T.TIdent s
        when Symbol.is_comptime s.Symbol.kind
             || Symbol.is_global s.Symbol.kind
                && is_comptime_global env (Symbol.key s) ->
          emit env
            (Diagnostic.error_at e.span "cannot take address of a constant"
            |> Diagnostic.help "a const has no storage, use let")
      | _ ->
          (* TODO(abe2): In the future allow &(a + b) once I figure out the memory and what to do about temporary lifetimes *)
          if not (is_lvalue te) then
            add_error env e.span "cannot take address of expression");
      T.mk (TPointer te.T.ty) (T.TUnOp (op, te))

(* This figures out the type of a field access *)
and synth_field (env : env) (span : Ast.span) (e : expr) (fname : Ast.name)
    (fspan : Ast.span) : T.texpr =
  let te = synth env e in
  let ty = te.T.ty in
  match strip_alias ty with
  | TStr -> (
      match fname with
      | n when n = len_name -> T.mk (TInt Usize) (T.TLen te)
      | _ ->
          emit env
            (Diagnostic.error_at fspan "no field"
            |> Diagnostic.label (Printf.sprintf "on %s" (show_ty env ty)));
          dummy_texpr)
  | TArray (elem, _) | TSlice elem -> (
      match fname with
      | n when n = len_name -> T.mk (TInt Usize) (T.TLen te)
      | n when n = ptr_name -> T.mk (TPointer elem) (T.TDataPtr te)
      | _ ->
          emit env
            (Diagnostic.error_at fspan "no field"
            |> Diagnostic.label (Printf.sprintf "on %s" (show_ty env ty)));
          dummy_texpr)
  | TOpaquePtr ->
      emit env (Diagnostic.opaque_operation span "access a field of");
      dummy_texpr
  | _ -> synth_struct_field env span te ty fname fspan

and synth_struct_field (env : env) (span : Ast.span) (te : T.texpr) (ty : ty)
    (fname : Ast.name) (fspan : Ast.span) : T.texpr =
  let rec peel depth = function
    | TStruct (sname, _) -> Some (sname, depth)
    | TAlias (_, base) -> peel depth base
    | TPointer t -> peel (depth + 1) t
    | _ -> None
  in
  match peel 0 ty with
  | None when strip_alias ty = TError -> dummy_texpr
  | None ->
      emit env
        (Diagnostic.error_at span "type has no fields"
        |> Diagnostic.label (Printf.sprintf "on %s" (show_ty env ty)));
      dummy_texpr
  | Some (_, depth) when depth > 1 ->
      let hint =
        Printf.sprintf "dereference first: `(*p).%s`" (Interner.text fname)
      in
      emit env
        (Diagnostic.error_at span "too many pointer levels"
        |> Diagnostic.help hint);
      dummy_texpr
  | Some (sname, _) -> (
      let info = lookup_struct env span sname in
      match
        List.find_mapi
          (fun field_id (name, ft) ->
            if name = fname then Some (field_id, ft) else None)
          info.field_tys
      with
      | Some (field_id, ft) -> T.mk ft (T.TFieldAccess (te, field_id))
      | None ->
          emit env
            (Diagnostic.error_at fspan "no field"
            |> Diagnostic.label
                 (Printf.sprintf "on struct %s" (Qname.show sname)));
          dummy_texpr)

and synth_call (env : env) (span : Ast.span) (callee : expr) (args : expr list)
    : T.texpr =
  (* A qualified callee is one symbol so it still calls direct *)
  let direct_callee =
    match callee.desc with
    | Ident _ | FieldAccess _ -> (
        match Resolve.sym_at_opt env.uses callee.span with
        | Some s when Symbol.is_func s.Symbol.kind -> Some s
        | _ -> None)
    | _ -> None
  in
  match direct_callee with
  | Some fn_sym ->
      let sig_ = lookup_func env callee.span in
      let targs = check_args env span sig_ args in
      let fixed_count =
        if sig_.variadic then Some (List.length sig_.param_tys) else None
      in
      let callee_texpr =
        T.mk (TFunc (sig_.param_tys, sig_.ret_ty, sig_.abi)) (T.TIdent fn_sym)
      in
      T.mk sig_.ret_ty (T.TCall (callee_texpr, targs, fixed_count))
  | _ -> (
      (* The callee is a value holding a fn ptr so call through it *)
      let callee_texpr = synth env callee in
      match resolve_ty callee_texpr.T.ty with
      | TError -> dummy_texpr
      | TFunc (param_tys, ret_ty, abi) ->
          let sig_ = { param_tys; ret_ty; variadic = false; abi } in
          let targs = check_args env span sig_ args in
          T.mk ret_ty (T.TCall (callee_texpr, targs, None))
      | _ ->
          emit env
            (Diagnostic.error "not callable"
            |> Diagnostic.at callee.span
            |> Diagnostic.label
                 (Printf.sprintf "this has type %s"
                    (show_ty env callee_texpr.T.ty)));
          dummy_texpr)

and synth_index (env : env) (span : Ast.span) (base : expr) (idx : expr) :
    T.texpr =
  let tbase = synth env base in
  match strip_alias tbase.T.ty with
  | TArray (elem, _) | TSlice elem -> (
      (* A missing low end reads as zero and a missing high end reads as the length *)
      let zero = T.mk (TInt Usize) (T.TInt 0L) in
      let whole_length = T.mk (TInt Usize) (T.TLen tbase) in
      match idx.desc with
      (* A slice borrows into the same storage and an inclusive end just
         desugars to one past *)
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
      | RangeFrom lo ->
          let tlo = check env lo (TInt Usize) in
          T.mk (TSlice elem) (T.TSliceExpr (tbase, tlo, whole_length))
      | RangeTo hi ->
          let thi = check env hi (TInt Usize) in
          T.mk (TSlice elem) (T.TSliceExpr (tbase, zero, thi))
      | RangeToInclusive hi ->
          let thi = check env hi (TInt Usize) in
          let one_past =
            T.mk (TInt Usize)
              (T.TBinOp (Ast.Add, thi, T.mk (TInt Usize) (T.TInt 1L)))
          in
          T.mk (TSlice elem) (T.TSliceExpr (tbase, zero, one_past))
      | RangeFull ->
          T.mk (TSlice elem) (T.TSliceExpr (tbase, zero, whole_length))
      | _ ->
          let tidx = synth env idx in
          if not (is_integer tidx.T.ty) then
            add_error env idx.span "array index must be an integer";
          T.mk elem (T.TIndex (tbase, tidx)))
  | TError -> dummy_texpr
  | TOpaquePtr ->
      emit env (Diagnostic.opaque_operation span "index");
      dummy_texpr
  | t ->
      emit env (Diagnostic.with_type span "cannot index" (show_ty env t));
      dummy_texpr

(* An array size can want a const before that decl is checked so values
   resolve on demand *)
and fold_num (env : env) (te : T.texpr) : Const_eval.const_num =
  Const_eval.fold_const_num
    ~sizeof:(ty_size env.struct_fields)
    ~resolve:(resolve_const env) te

and resolve_const (env : env) (s : Symbol.t) (_ : ty) (span : Ast.span) :
    Const_eval.const_num =
  match s.Symbol.kind with
  | Symbol.Local Ast.Comptime -> (
      match Symbol.Table.find_opt env.l_vals (Symbol.key s) with
      | Some v -> v
      | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ]))
  | Symbol.Global when Symbol.Table.mem env.g_state (Symbol.key s) ->
      global_const_num env span (Symbol.key s)
  | _ -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])

and global_const_num (env : env) (span : Ast.span) (key : Symbol.key) :
    Const_eval.const_num =
  let st =
    match Symbol.Table.find_opt env.g_state key with
    | Some st -> st
    | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
  in
  match st.value with
  | Some v -> v
  | None ->
      if st.busy then
        raise (Diagnostic.Errors [ Diagnostic.error_at span "cyclic constant" ]);
      let te =
        match global_typed_init env span key with
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
and global_typed_init (env : env) (span : Ast.span) (key : Symbol.key) : T.texpr
    =
  let st =
    match Symbol.Table.find_opt env.g_state key with
    | Some st -> st
    | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
  in
  match st.typed with
  | Some te -> te
  | None ->
      if st.busy then
        raise (Diagnostic.Errors [ Diagnostic.error_at span "cyclic constant" ]);
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
(* FIXME(80e8): The default keeps main working until return types are inferred *)
let ret_ty_of (env : env) (fd : func_def) : ty =
  match fd.ret with
  | Some t -> return_ty_of_ast env t
  | None -> if is_entry env fd.func_span then TInt I32 else TVoid

(* First pass collecting signatures so that the compiler can handle forward references *)
let collect_func (env : env) (fd : func_def) : unit =
  let abi = resolve_abi env fd.extern_abi in
  let param_tys =
    List.map (fun (p : param) -> ty_of_ast env p.param_typ) fd.params
  in
  let ret_ty = ret_ty_of env fd in
  Symbol.Table.replace env.funcs (key_at env fd.func_span)
    { param_tys; ret_ty; variadic = fd.variadic; abi }

(* A repeat name is already reported with both spans by the resolver *)
let type_name_taken (env : env) (span : Ast.span) : bool =
  Symbol.Table.mem env.types (key_at env span)

(* The name goes in first so a field can name this struct or one defined later *)
let reserve_struct_name (env : env) (sd : struct_def) : unit =
  if not (type_name_taken env sd.struct_span) then (
    let seen = Hashtbl.create 8 in
    List.iter
      (fun (f : field) ->
        if Hashtbl.mem seen f.field_name then
          emit env (Diagnostic.error_at f.field_span "duplicate field")
        else Hashtbl.add seen f.field_name ())
      sd.fields;
    Symbol.Table.replace env.types
      (key_at env sd.struct_span)
      (DStruct { field_tys = [] });
    Symbol.Table.replace env.struct_fields (key_at env sd.struct_span) [])

let resolve_struct_fields (env : env) (sd : struct_def) : unit =
  match Symbol.Table.find_opt env.types (key_at env sd.struct_span) with
  | Some (DStruct _) ->
      let field_tys =
        List.map
          (fun (f : field) -> (f.field_name, ty_of_ast env f.field_typ))
          sd.fields
      in
      Symbol.Table.replace env.types
        (key_at env sd.struct_span)
        (DStruct { field_tys });
      Symbol.Table.replace env.struct_fields
        (key_at env sd.struct_span)
        (List.map snd field_tys)
  | _ -> ()

(* A pointer or slice field is just an address so it can't grow the struct *)
let check_struct_cycle (env : env) (sd : struct_def) : unit =
  let fields_of name =
    Option.value ~default:[] (Symbol.Table.find_opt env.struct_fields name)
  in
  let on_path = Hashtbl.create 8 in
  let rec reaches (target : Symbol.key) (t : ty) : bool =
    match resolve_ty t with
    | TStruct (name, _) when Qname.key name = target -> true
    | TStruct (name, _) when Hashtbl.mem on_path (Qname.key name) -> false
    | TStruct (name, _) ->
        let name = Qname.key name in
        Hashtbl.add on_path name ();
        let hit = List.exists (reaches target) (fields_of name) in
        Hashtbl.remove on_path name;
        hit
    | TArray (elem, _) -> reaches target elem
    | _ -> false
  in
  let key = key_at env sd.struct_span in
  if List.exists (reaches key) (fields_of key) then
    emit env
      (Diagnostic.error_at sd.struct_name_span
         "recursive struct has infinite size")

(* A fake error type goes in the table first and it just means the real body hasn't been read yet *)
let reserve_alias_name (env : env) (td : type_alias_def) : unit =
  if not (type_name_taken env td.alias_span) then
    Symbol.Table.replace env.types (key_at env td.alias_span) (DAlias TError)

let reserve_newtype_name (env : env) (td : type_alias_def) : unit =
  if not (type_name_taken env td.alias_span) then
    Symbol.Table.replace env.types (key_at env td.alias_span) (DNewtype TError)

let collect_alias (env : env) (td : type_alias_def) : unit =
  match Symbol.Table.find_opt env.types (key_at env td.alias_span) with
  | Some (DAlias TError) ->
      Symbol.Table.replace env.types (key_at env td.alias_span)
        (DAlias (ty_of_ast env td.alias_typ))
  | _ -> ()

let collect_newtype (env : env) (td : type_alias_def) : unit =
  match Symbol.Table.find_opt env.types (key_at env td.alias_span) with
  | Some (DNewtype TError) ->
      Symbol.Table.replace env.types (key_at env td.alias_span)
        (DNewtype (ty_of_ast env td.alias_typ))
  | _ -> ()

let rec named_type_spans (t : typ) : Ast.span list =
  match t.tdesc with
  | Named _ -> [ t.tspan ]
  | ErrorType -> []
  | Pointer inner | Slice inner | Array (_, inner) -> named_type_spans inner
  | FuncPtr (_, params, ret) ->
      List.concat_map named_type_spans params
      @ Option.value ~default:[] (Option.map named_type_spans ret)

(* An alias is only a second name for what it points at. A pointer in the
   middle doesn't save it the way it saves a struct field *)
let collect_type_bodies (env : env) (decls : decl list) : unit =
  let defs = Hashtbl.create 16 in
  let remember (decl : decl) : unit =
    match decl with
    | TypeAlias td | Newtype td ->
        (* Only the first one counts because a repeat name already got turned down *)
        (* An unresolved name shares one key so two broken types would look mutually recursive *)
        let key = key_at env td.alias_span in
        if key <> unresolved_key && not (Hashtbl.mem defs key) then
          Hashtbl.add defs key decl
    | Func _ | Extern _ | Global _ | Struct _ -> ()
  in
  let unfilled (decl : decl) : bool =
    match decl with
    | TypeAlias td ->
        Symbol.Table.find_opt env.types (key_at env td.alias_span)
        = Some (DAlias TError)
    | Newtype td ->
        Symbol.Table.find_opt env.types (key_at env td.alias_span)
        = Some (DNewtype TError)
    | Func _ | Extern _ | Global _ | Struct _ -> false
  in
  let fill (decl : decl) : unit =
    match decl with
    | TypeAlias td -> collect_alias env td
    | Newtype td -> collect_newtype env td
    | Func _ | Extern _ | Global _ | Struct _ -> ()
  in
  let on_path = Hashtbl.create 8 in
  let rec force (key : Symbol.key) : unit =
    match Hashtbl.find_opt defs key with
    | None -> ()
    | Some ((TypeAlias td | Newtype td) as decl) ->
        if Hashtbl.mem on_path key then
          emit env (Diagnostic.error_at td.alias_name_span "recursive type")
        else if unfilled decl then begin
          Hashtbl.add on_path key ();
          let step span = force (key_at env span) in
          List.iter step (named_type_spans td.alias_typ);
          Hashtbl.remove on_path key;
          if unfilled decl then fill decl
        end
    | Some (Func _ | Extern _ | Global _ | Struct _) -> ()
  in
  List.iter remember decls;
  (* The order here follows the file so the same type gets blamed every time *)
  List.iter
    (function
      | TypeAlias td | Newtype td -> force (key_at env td.alias_span)
      | Func _ | Extern _ | Global _ | Struct _ -> ())
    decls

let collect_global (env : env) (gd : global_def) : unit =
  (if gd.init = None then
     match gd.kind with
     | Var -> ()
     | Let ->
         emit env (Diagnostic.error_at gd.name_span "let without initializer")
     | Comptime ->
         emit env
           (Diagnostic.error_at gd.name_span "comptime without initializer"));
  let t = ty_of_ast env gd.typ in
  Symbol.Table.replace env.globals (key_at env gd.span) (t, gd.kind)

let fill_struct_fields_decl (env : env) (decl : decl) : unit =
  match decl with Struct sd -> resolve_struct_fields env sd | _ -> ()

let check_cycle_decl (env : env) (decl : decl) : unit =
  match decl with Struct sd -> check_struct_cycle env sd | _ -> ()

(* Every type name lands first so a signature can name a type written later *)
let reserve_type_name (env : env) (decl : decl) : unit =
  let env = reading env decl in
  match decl with
  | Struct sd -> reserve_struct_name env sd
  | TypeAlias td -> reserve_alias_name env td
  | Newtype td -> reserve_newtype_name env td
  | Func _ | Extern _ | Global _ -> ()

let collect_decl (env : env) (decl : decl) : unit =
  let env = reading env decl in
  match decl with
  | Func fd | Extern fd -> collect_func env fd
  | Global gd -> collect_global env gd
  | Struct _ | TypeAlias _ | Newtype _ -> ()

let check_func ?(is_extern = false) (env : env) (fd : func_def) : T.tfunc_def =
  (* The collected signature is reused so a bad array size errors once *)
  let collected = Symbol.Table.find_opt env.funcs (key_at env fd.func_span) in
  let param_tys =
    match collected with
    | Some s when List.length s.param_tys = List.length fd.params -> s.param_tys
    | _ -> List.map (fun (p : param) -> ty_of_ast env p.param_typ) fd.params
  in
  let params_typed =
    List.map2
      (fun (p : param) t -> (p.param_name, t, p.param_span))
      fd.params param_tys
  in
  let params = List.map (fun (_, t, span) -> (sym env span, t)) params_typed in

  let ret_ty =
    match collected with Some s -> s.ret_ty | None -> ret_ty_of env fd
  in

  (* Main always returns a 32 bit integer so any other type the user writes is rejected *)
  if is_entry env fd.func_span && ret_ty <> TError && ret_ty <> TInt I32 then begin
    let span = match fd.ret with Some t -> t.tspan | None -> fd.func_span in
    emit env
      (Diagnostic.type_mismatch span ~expected:(show_ty env (TInt I32))
         ~found:(show_ty env ret_ty))
  end;

  let func_env =
    push_scope { env with ret_ty; in_main = is_entry env fd.func_span }
  in
  (* An extern has no body so its params can't be used and stay quiet *)
  let param_env =
    List.fold_left
      (fun e (name, t, span) ->
        extend_var ~used:is_extern ~deduplicate:true e span name t)
      func_env params_typed
  in

  let is_entry_point = is_entry env fd.func_span in
  (* An unwritten i32 on main comes from the runtime and not from the user *)
  let implicit_return =
    (not is_extern) && ret_ty <> TVoid
    && ((not is_entry_point) || fd.ret <> None)
  in
  let body_use =
    if not implicit_return then Discard
    else if is_entry_point then
      (* The main function can fall off the end with 0 so a void tail stays fine *)
      Infer
    else Expect ret_ty
  in
  (* The declared return type is what the empty body failed to produce *)
  let body_span =
    match fd.ret with Some t -> t.tspan | None -> fd.func_name_span
  in
  let final_env, tbody0 = check_block param_env body_span fd.body body_use in
  warn_unused_in_scope final_env;
  let tbody =
    match (implicit_return, List.rev tbody0) with
    (* A live value tail returns while a diverging tail already left on its own *)
    | true, last :: rest when last.T.ty = ret_ty && ret_ty <> TNever ->
        let ret = T.mk ~span:last.T.span TNever (T.TReturn (Some last)) in
        List.rev (ret :: rest)
    | _ -> tbody0
  in

  {
    T.key = key_at env fd.func_span;
    name = link_name_at env fd.func_span (Interner.text fd.func_name);
    (* A bare name reads the same in two modules so a panic report qualifies it *)
    source_name =
      String.concat "." (env.reader_path @ [ Interner.text fd.func_name ]);
    entry_point = is_entry env fd.func_span;
    params;
    ret_ty;
    body = tbody;
    modifiers = fd.func_modifiers;
    variadic = fd.variadic;
  }

let rec is_const_texpr (env : env) (te : T.texpr) : bool =
  match te.T.desc with
  (* Already reported once so there's nothing more to say here *)
  | T.TErrorExpr -> true
  | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TChar _ | T.TCStr _
  | T.TSizeOf _ ->
      true
  (* A function address is a link time constant *)
  | T.TIdent s ->
      Symbol.is_func s.Symbol.kind || is_const_global env (Symbol.key s)
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
  | T.TStr _ | T.TCall _ | T.TFieldAccess _ | T.TRange _ | T.TRangeInclusive _
  | T.TIndex _ | T.TLen _ | T.TToSlice _ | T.TSliceExpr _ | T.TDataPtr _
  | T.TBlock _ | T.TIf _ | T.TWhile _ | T.TFor _ | T.TBinding _ | T.TReturn _
  | T.TBreak _ | T.TContinue _ | T.TLocalDecl | T.TLoop _ ->
      false
  | T.TUndef -> true
  | T.TPairAssign _ -> false

let check_global (env : env) (gd : global_def) : T.tglobal_def =
  (* The collected type is reused so a bad array size errors once *)
  let t =
    match Symbol.Table.find_opt env.globals (key_at env gd.span) with
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
          if Symbol.Table.mem env.g_state (key_at env gd.span) then
            global_typed_init env e.span (key_at env gd.span)
          else check env e t
        in
        if not (is_const_texpr env te) then (
          emit env (Diagnostic.error_at e.span "initializer must be constant");
          None)
        else Some te
  in
  {
    T.key = key_at env gd.span;
    name = link_name_at env gd.span (Interner.text gd.name);
    ty = t;
    init = tinit;
    kind = gd.kind;
    modifiers = gd.modifiers;
  }

let typed_struct_decl (env : env) (sd : struct_def) (fields : ty list) : T.tdecl
    =
  let name = qname_at env sd.struct_span (Interner.text sd.struct_name) in
  let is_local =
    Option.fold ~none:false
      ~some:(fun symbol -> symbol.Symbol.kind = Symbol.LocalType)
      (Resolve.sym_at_opt env.uses sd.struct_span)
  in
  if is_local then T.TLocalStruct (name, fields)
  else T.TStruct (name, fields, sd.struct_modifiers)

let check_decl (env : env) (decl : decl) : T.tdecl =
  let env = reading env decl in
  match decl with
  | Func fd ->
      let tfd = check_func env fd in
      T.TFunc tfd
  | Extern fd ->
      let tfd = check_func ~is_extern:true env fd in
      T.TExtern tfd
  | Struct sd ->
      (* A rejected duplicate never landed in the table and its fields are read directly *)
      let field_tys =
        match Symbol.Table.find_opt env.types (key_at env sd.struct_span) with
        | Some (DStruct info) -> info.field_tys
        | _ ->
            List.map
              (fun (f : field) -> (f.field_name, ty_of_ast env f.field_typ))
              sd.fields
      in
      typed_struct_decl env sd (List.map snd field_tys)
  | Global gd -> T.TGlobal (check_global env gd)
  (* A rejected duplicate never landed in the table and falls back to the written type *)
  | TypeAlias td ->
      let t =
        match Symbol.Table.find_opt env.types (key_at env td.alias_span) with
        | Some (DAlias t) -> t
        | _ -> ty_of_ast env td.alias_typ
      in
      T.TTypeAlias (qname_at env td.alias_span (Interner.text td.alias_name), t)
  | Newtype td ->
      let t =
        match Symbol.Table.find_opt env.types (key_at env td.alias_span) with
        | Some (DNewtype t) -> t
        | _ -> ty_of_ast env td.alias_typ
      in
      T.TNewtype (qname_at env td.alias_span (Interner.text td.alias_name), t)

let fold_consts (env : env) (tdecls : T.tdecl list) : T.tdecl list =
  Const_fold.run ~emit:(emit env)
    ~force_const:(fun span key -> ignore (global_const_num env span key))
    ~local_value:(fun symbol ->
      Symbol.Table.find_opt env.l_vals (Symbol.key symbol))
    ~global_value:(fun symbol ->
      let key = Symbol.key symbol in
      if is_comptime_global env key then
        match Symbol.Table.find_opt env.g_state key with
        | Some { value = Some v; _ } -> Some v
        | _ -> None
      else None)
    ~fold_num:(fold_num env) tdecls

(* The partial tree stays available so later checks can still run *)
let typecheck ~(diags : Diagnostic.sink) (uses : Resolve.t) (decls : decl list)
    : T.tdecl list =
  let env = make_env diags uses in
  let decls = decls @ Resolve.local_decls uses in
  (* An early array size can demand any later const so defs go in first *)
  List.iter
    (function
      | Global ({ kind = Let | Comptime; _ } as gd) ->
          Symbol.Table.replace env.g_state (key_at env gd.span)
            { def = gd; typed = None; value = None; busy = false }
      | _ -> ())
    decls;
  List.iter (reserve_type_name env) decls;
  collect_type_bodies env decls;
  List.iter (collect_decl env) decls;
  List.iter (fill_struct_fields_decl env) decls;
  List.iter (check_cycle_decl env) decls;
  let tdecls = List.map (check_decl env) decls in
  fold_consts env tdecls
