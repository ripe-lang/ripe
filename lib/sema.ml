(* SPDX-License-Identifier: Apache-2.0 *)

open Ast
open Types
open Typred
open! Tast

type func_sig = {
  param_tys : ty list;
  ret_ty : ty;
  variadic : bool;
  abi : Types.func_abi;
}

type struct_info = {
  field_tys : (Ast.name * ty) list;
  field_index : (Ast.name, int * ty) Hashtbl.t option;
}

type enum_info = { variants_by_name : (Ast.name, int64) Hashtbl.t }

type 'a deferred = Unstarted | Running | Completed of 'a

(* Structs aliases and builtins share one namespace of type names *)
type type_def =
  | Struct_type of struct_info deferred ref
  | Alias_type of ty deferred ref
  | Builtin_type of Types.builtin
  | Enum_type of enum_info deferred ref

type result_use = Infer | Expect of ty | Discard
type var_info = { name : Ast.name; ty : ty; used : bool ref; span : Ast.span }

let cyclic_constant span = Diagnostic.Errors [ Diagnostic.cyclic_constant span ]

(* The running mark catches a value that asks for itself *)
let force_deferred span cell ~(on_error : 'a) (compute : unit -> 'a) =
  match !cell with
  | Completed v -> v
  | Running -> raise (cyclic_constant span)
  | Unstarted -> (
      cell := Running;
      try
        let v = compute () in
        cell := Completed v;
        v
      with e ->
        cell := Completed on_error;
        raise e)

type global_fact = {
  declaration : global_def;
  declared_ty : ty deferred ref;
  typed : Tast.texpr deferred ref;
  folded : Constant.value deferred ref;
}

type loop_result =
  (* var value = loop {
       continue
     } *)
  | InferLoopResult
  (* var value: i64 = loop {
       break 5
     } *)
  | ExpectLoopResult of ty
  (* var value = loop {
       if condition { break 1 }
       break 2
     } *)
  | FlexibleLoopResult of ty * Ast.span * expr list
  (* var value = loop {
       if condition { break known_i32 }
       break known_i64
     } *)
  | RigidLoopResult of ty * Ast.span * expr list

type loop_ctx = {
  lbl : Ast.name option;
  valued : bool;
  mutable result : loop_result;
  mutable bare_break : Ast.span option;
}

type ctx = {
  declarations : decl list;
  func_sigs : func_sig Symbol.Table.t;
  type_defs : type_def Symbol.Table.t;
  (* The layout table mirrors struct definitions for size queries *)
  layouts : Layout.structs;
  globals : (ty * Ast.binding_kind) Symbol.Table.t;
  (* Constants evaluate on demand so an array size may name a later const *)
  global_facts : global_fact Symbol.Table.t;
  const_values : Constant.value Symbol.Table.t;
  diags : Diagnostic.sink;
  symbols : Resolve.t;
  initial_errors : bool;
}

type local_env = {
  ctx : ctx;
  scopes : (Symbol.key * var_info) list list;
  mutable local_values : Constant.value Symbol.Table.t option;
  ret_ty : ty;
  loops : loop_ctx list;
  entry_function : bool;
  in_declaration : bool;
  suppress_warnings : bool;
  (* Whoever reads the message is inside this module so its path drops out *)
  reader_path : string list;
  probe_diags : Diagnostic.sink option;
}

type coercion_input = Contextual of expr | Typed of expr * Tast.texpr

let make_ctx diags symbols declarations =
  let type_defs = Symbol.Table.create 16 in
  let seed (key, builtin) =
    Symbol.Table.replace type_defs key (Builtin_type builtin)
  in
  List.iter seed (Resolve.builtins symbols);
  {
    declarations;
    func_sigs = Symbol.Table.create 16;
    type_defs;
    layouts = Layout.make_structs ();
    globals = Symbol.Table.create 16;
    global_facts = Symbol.Table.create 16;
    const_values = Symbol.Table.create 16;
    diags;
    symbols;
    initial_errors = Diagnostic.has_errors diags;
  }

let make_env ?(reader_path = []) ctx =
  {
    ctx;
    scopes = [];
    local_values = None;
    ret_ty = Types.TUnit;
    loops = [];
    entry_function = false;
    in_declaration = true;
    suppress_warnings = ctx.initial_errors;
    reader_path;
    probe_diags = None;
  }

let show_ty env t = Types.show_ty_in env.reader_path t

let decl_span = function
  | Func fd | Extern fd -> fd.func_span
  | Global gd -> gd.span
  | Struct sd -> sd.struct_span
  | TypeAlias td -> td.alias_span
  | Enum ed -> ed.enum_span

let env_at ctx span =
  make_env ctx ~reader_path:(Resolve.module_path_at ctx.symbols span)

let env_for_decl ctx decl = env_at ctx (decl_span decl)

let decl_env_at env span =
  let reader_path = Resolve.module_path_at env.ctx.symbols span in
  if env.in_declaration && env.reader_path = reader_path then env
  else make_env env.ctx ~reader_path

(* The two fields every slice and string answers to, interned once *)
let len_name = Interner.intern "len"
let ptr_name = Interner.intern "ptr"
let dummy_value = Constant.VInt (Constant.zero, Types.I32)
let sym env span = Resolve.sym_at env.ctx.symbols span

let diagnostic_sink env = Option.value env.probe_diags ~default:env.ctx.diags

let emit env d = Diagnostic.emit (diagnostic_sink env) d

let add_error env span msg =
  Diagnostic.emit_error_at (diagnostic_sink env) span msg

let add_error_in ctx span msg = Diagnostic.emit_error_at ctx.diags span msg

let dummy_texpr = Tast.mk Types.TError Tast.TErrorExpr

let add_warning env span msg =
  if not env.suppress_warnings then
    Diagnostic.emit_warn_at (diagnostic_sink env) span msg

(* An unsigned literal past i64 max is stored as a negative bit pattern *)
let unsigned_to_float n =
  let two_pow_64 = 18446744073709551616.0 in
  if Int64.compare n 0L >= 0 then Int64.to_float n
  else Int64.to_float n +. two_pow_64

let round_to_float_kind kind f =
  match kind with
  | F32 -> Int32.float_of_bits (Int32.bits_of_float f)
  | F64 -> f

let push_scope env =
  { env with scopes = [] :: env.scopes; in_declaration = false }

let is_probing env = Option.is_some env.probe_diags

(* A probe gets its own copy so a thrown away walk can't record a break *)
let clone_loop loop =
  {
    lbl = loop.lbl;
    valued = loop.valued;
    result = loop.result;
    bare_break = loop.bare_break;
  }

let probing env =
  {
    env with
    loops = List.map clone_loop env.loops;
    probe_diags = Some (Diagnostic.sink ());
  }

let local_consts env =
  match env.local_values with
  | Some values -> values
  | None ->
      let values = Symbol.Table.create 16 in
      env.local_values <- Some values;
      values

let warn_unused_in_scope env =
  match env.scopes with
  | scope :: _ when not env.suppress_warnings ->
      List.iter
        (fun (_, (info : var_info)) ->
          let shown = Interner.text info.name in
          if (not !(info.used)) && shown.[0] <> '_' then
            emit env
              (Diagnostic.warning (Printf.sprintf "unused variable: %s" shown)
              |> Diagnostic.at info.span
              |> Diagnostic.help
                   (Printf.sprintf "prefix with an underscore: _%s" shown)))
        scope
  | _ -> ()

let extend_var ?(used = false) ?(deduplicate = false) env span name t =
  let key = Symbol.key (sym env span) in
  let info = { name; ty = t; used = ref used; span } in
  match env.scopes with
  | scope :: _ when deduplicate && List.mem_assoc key scope -> env
  | scope :: rest -> { env with scopes = ((key, info) :: scope) :: rest }
  | [] -> assert false

let lookup_var_opt env span =
  let key = Symbol.key (sym env span) in
  let rec find = function
    | [] -> None
    | scope :: rest -> (
        match List.assoc_opt key scope with
        | Some _ as found -> found
        | None -> find rest)
  in
  match find env.scopes with
  | Some info ->
      info.used := true;
      Some info.ty
  | None -> None

(* A span the resolver never bound falls back so checking can carry on *)
let symbol_in ctx span ~(missing : 'a) (read : Symbol.t -> 'a) =
  match Resolve.sym_at_opt ctx.symbols span with
  | Some symbol -> read symbol
  | None -> missing

let symbol_at env = symbol_in env.ctx

let key_in ctx span =
  symbol_in ctx span ~missing:Symbol.unresolved_key Symbol.key

(* Two declarations can go by one name so lookups key on which one it is *)
let key_at env span = key_in env.ctx span

let builtin_at env span =
  match Symbol.Table.find_opt env.ctx.type_defs (key_at env span) with
  | Some (Builtin_type b) -> Some b
  | Some (Struct_type _ | Alias_type _ | Enum_type _) | None -> None

(* The symbol path qualifies types in diagnostics *)
let qname_at env span fallback =
  symbol_at env span
    ~missing:(Qname.unresolved fallback)
    (Resolve.qname_of env.ctx.symbols)

let qname_in ctx span fallback =
  symbol_in ctx span
    ~missing:(Qname.unresolved fallback)
    (Resolve.qname_of ctx.symbols)

(* What the linker calls this declaration was worked out once by the resolver *)
let link_name_at env span fallback =
  symbol_at env span ~missing:fallback (fun s -> s.Symbol.link_name)

let is_entry env span =
  symbol_at env span ~missing:false (fun s -> s.Symbol.entry_point)

let lookup_func env span =
  match Symbol.Table.find_opt env.ctx.func_sigs (key_at env span) with
  | Some s -> s
  | None ->
      emit env (Diagnostic.undefined_name span "function");
      {
        param_tys = [];
        ret_ty = Types.TUnit;
        variadic = false;
        abi = Types.Ripe;
      }

let is_comptime_global env key =
  match Symbol.Table.find_opt env.ctx.globals key with
  | Some (_, Comptime) -> true
  | _ -> false

let is_const_symbol env s =
  Symbol.is_comptime s.Symbol.kind
  || (Symbol.is_global s.Symbol.kind && is_comptime_global env (Symbol.key s))

(* TODO: This needs aggregate constants for global copies *)
let verify_const_scalar env span t =
  if not (is_scalar t) then
    emit env
      (Diagnostic.with_type span "comptime must be a scalar" (show_ty env t)
      |> Diagnostic.help "use var for values that need storage")

let lookup_struct env span name =
  match Symbol.Table.find_opt env.ctx.type_defs (Qname.key name) with
  | Some (Struct_type { contents = Completed info }) -> info
  | Some (Struct_type { contents = Unstarted | Running }) | _ ->
      emit env (Diagnostic.undefined_name span "struct");
      { field_tys = []; field_index = None }

let find_field info name =
  match info.field_index with
  | Some index -> Hashtbl.find_opt index name
  | None ->
      List.find_mapi
        (fun field_id (field_name, ty) ->
          if field_name = name then Some (field_id, ty) else None)
        info.field_tys

let lift_ty (f : ty -> ty) ty =
  match ty with Types.TError -> Types.TError | ty -> f ty

let resolve_named_abi env name span =
  match Types.func_abi_of_name name with
  | Some abi -> abi
  | None ->
      emit env (Diagnostic.unsupported_abi span);
      Types.AbiError

(* A signature without an ABI written on it is a plain Ripe function *)
let resolve_abi env a =
  match a with
  | NoAbi -> Types.Ripe
  | AbiError -> Types.AbiError
  | NamedAbi (name, span) -> resolve_named_abi env name span

let unresolved_named_ty env span =
  match Resolve.sym_at_opt env.ctx.symbols span with
  | Some { Symbol.kind = Symbol.Error; _ } -> Types.TError
  | _ -> Diagnostic.ice ~span "type name escaped the resolver"

let named_ty env span shown =
  match Symbol.Table.find_opt env.ctx.type_defs (key_at env span) with
  | Some (Builtin_type (BTy Types.TNever)) ->
      emit env
        Diagnostic.(
          error "never is only valid as a function return type"
          |> at span
          |> help "a value of type never cannot exist");
      Types.TError
  | Some (Builtin_type BOpaque) ->
      emit env
        Diagnostic.(
          error "opaque is only valid as a pointee"
          |> at span
          |> help "use *opaque for an untyped pointer");
      Types.TError
  | Some (Builtin_type (BTy ty)) -> ty
  | Some (Struct_type _) -> Types.TStruct (qname_at env span shown, [])
  | Some (Alias_type { contents = Completed aliased }) ->
      Types.TAlias (qname_at env span shown, aliased)
  | Some (Alias_type { contents = Unstarted | Running }) -> Types.TError
  | Some (Enum_type _) -> Types.TEnum (qname_at env span shown)
  | None -> unresolved_named_ty env span

(* Only the biggest folded subtree reports so a wide intermediate stays legal *)
let rec report_const_range env te =
  match (te.const, resolve_ty te.ty, te.desc) with
  | Some _, _, Tast.TSizeOf _ -> ()
  | Some v, Types.TInt kind, _ ->
      if not (Constant.representable kind (Constant.exact_of v)) then
        emit env
          (Diagnostic.int_out_of_range te.span
             ~ty:(show_ty env (Types.TInt kind)))
  | _, _, (Tast.TCast operand | Tast.TUnOp (_, operand)) ->
      report_const_range env operand
  | _, _, Tast.TBinOp (_, l, r) ->
      report_const_range env l;
      report_const_range env r
  | _ -> ()

(* The value of a block is its last element and unit when the block is empty *)
let tblock_ty (tb : Tast.tblock) =
  match List.rev tb with te :: _ -> te.ty | [] -> Types.TUnit

let if_result_ty (tbranches : (Tast.texpr * Tast.tblock) list) telse
    ~fallback_ty =
  let diverges tb = tblock_ty tb = Types.TNever in
  match telse with
  | Some tb
    when diverges tb && List.for_all (fun (_, tb) -> diverges tb) tbranches ->
      Types.TNever
  | _ -> fallback_ty

(* A literal or diverging tail bends to a sibling so it can't anchor the type *)
let rec arm_is_flexible (e : expr) =
  match e.desc with
  | Int (_, None) | Float (_, None) -> true
  | UnOp ((Pos | Neg), inner) -> arm_is_flexible inner
  | Block body -> block_is_flexible body
  | If (branches, else_body) ->
      Option.is_some else_body
      && List.for_all
           (fun (_, { Ast.value = body; _ }) -> block_is_flexible body)
           branches
      && Option.exists
           (fun { Ast.value = body; _ } -> block_is_flexible body)
           else_body
  | _ -> false

and block_is_flexible body =
  match List.rev body with
  | Expr last :: _ -> arm_is_flexible last
  | Decl _ :: _ | [] -> false

let common_ty current candidate =
  match (current, candidate) with
  | Types.TNever, candidate -> candidate
  | current, Types.TNever -> current
  | Types.TNull, candidate -> candidate
  | current, Types.TNull -> current
  | current, candidate ->
      Option.value (common_numeric_ty current candidate) ~default:current

let is_unused_operation (e : expr) =
  match e.desc with BinOp _ | UnOp _ | BitCast _ -> true | _ -> false

let warn_discarded_operation env e (te : Tast.texpr) =
  if
    (not env.suppress_warnings)
    && (not (Diagnostic.has_errors (diagnostic_sink env)))
    && is_unused_operation e && te.ty <> Types.TUnit && te.ty <> Types.TNever
    && te.ty <> Types.TError
  then
    emit env
      (Diagnostic.warning "discarded operation result"
      |> Diagnostic.at te.span
      |> Diagnostic.help "use `var _ = ...` when this is intentional")

let verify_unit_result env span = function
  | Expect want when resolve_ty want <> Types.TUnit ->
      emit env
        (Diagnostic.type_mismatch span ~expected:(show_ty env want)
           ~found:(show_ty env Types.TUnit))
  | Infer | Discard | Expect _ -> ()

let block_item_span = function
  | Expr e -> e.span
  | Decl d -> decl_span (decl_of_local d)

let new_loop label ~valued =
  {
    lbl = Option.map (fun l -> l.Ast.value) label;
    valued;
    result = InferLoopResult;
    bare_break = None;
  }

let find_loop env label =
  match label with
  | None -> ( match env.loops with lc :: _ -> Some lc | [] -> None)
  | Some l -> List.find_opt (fun lc -> lc.lbl = Some l.Ast.value) env.loops

let find_loop_or_error env span headline label =
  let found = find_loop env label in
  (match (found, label) with
  | None, None -> add_error env span headline
  | None, Some l -> emit env (Diagnostic.undefined_name l.Ast.span "loop label")
  | Some _, _ -> ());
  found

let verify_bare_break env span lc =
  if lc.bare_break = None then lc.bare_break <- Some span;
  match lc.result with
  | InferLoopResult | ExpectLoopResult _ -> ()
  | FlexibleLoopResult (t, first, _) | RigidLoopResult (t, first, _) ->
      emit env
        (Diagnostic.break_disagree span "no value here" ~other:first
           ~other_message:(Printf.sprintf "breaks with %s" (show_ty env t)))

(* An array already holds the data needed by a slice *)
let adopt_slice want (te : Tast.texpr) =
  match (resolve_ty want, resolve_ty te.ty) with
  | Types.TSlice _, Types.TArray _ ->
      let zero = Tast.mk (Types.TInt Usize) (Tast.TInt 0L) in
      let len = Tast.mk (Types.TInt Usize) (Tast.TLen te) in
      Tast.mk want (Tast.TSliceExpr (te, zero, len))
  | _ -> te

(* The count keeps its own type since it is only a number of positions *)
let verify_shift_count env span (tr : Tast.texpr) =
  if not (is_integer tr.ty) then
    emit env
      (Diagnostic.with_found span "shift count must be an integer"
         (show_ty env tr.ty))

let no_such_field env span ty =
  emit env (Diagnostic.with_type span "no field" (show_ty env ty));
  dummy_texpr

let verify_operands env span op t =
  if not (binop_accepts op t) then
    emit env
      (Diagnostic.bad_operand span ~op:(show_binop_sym op) ~ty:(show_ty env t))

(* An aggregate parameter is copied before a write *)
let verify_param_copy_write env span tl =
  match root_lvalue tl with
  | Some { desc = Tast.TIdent s; ty; _ }
    when s.Symbol.kind = Symbol.Param
         &&
         match resolve_ty ty with
         | Types.TArray _ | Types.TStruct _ -> true
         | _ -> false ->
      emit env
        (Diagnostic.error_at span "cannot assign to a by value parameter"
        |> Diagnostic.label "the caller keeps its own copy"
        |> Diagnostic.help
             (Printf.sprintf "take a pointer to write through it: %s: *%s"
                s.Symbol.name (show_ty env ty)))
  | Some _ | None -> ()

let find_global_fact env span key =
  match Symbol.Table.find_opt env.ctx.global_facts key with
  | Some st -> st
  | None -> raise (Diagnostic.Errors [ Constant.unsupported_const span ])

(* A failed fold reports and hands back a dummy so checking continues *)
let const_value_or env default (te : Tast.texpr) =
  if not (is_scalar te.ty && te.ty <> Types.TError) then default
  else
    match te.const with
    | Some v -> v
    | None ->
        emit env (Constant.unsupported_const te.span);
        default

let adopt_int_literal env span want target ~neg n =
  let signed = if neg then Int64.neg n else n in
  match target with
  | Types.TInt kind ->
      let exact = Constant.of_magnitude ~neg n in
      Some
        {
          (Tast.mk want (Tast.TInt signed)) with
          const = Some (Constant.VInt (exact, kind));
        }
  | Types.TFloat kind ->
      let magnitude = unsigned_to_float n in
      let exact = if neg then -.magnitude else magnitude in
      if Int64.unsigned_compare n (float_kind_exact_limit kind) > 0 then
        emit env
          (Diagnostic.error "integer literal loses precision"
          |> Diagnostic.at span
          |> Diagnostic.label
               (Printf.sprintf "becomes %.0f" (round_to_float_kind kind exact))
          |> Diagnostic.help
               (Printf.sprintf "write %s%Lu.0 to accept the rounding"
                  (if neg then "-" else "")
                  n));
      Some (Tast.mk want (Tast.TFloat exact))
  | Types.TError -> Some (Tast.mk want (Tast.TInt signed))
  | _ -> None

(* A variant is a compile time constant so nothing of the enum survives here *)
let synth_variant env (inner : expr) info fname fspan =
  let shown = Interner.text fname in
  let name = qname_at env inner.span shown in
  match Hashtbl.find_opt info.variants_by_name fname with
  | Some value -> Tast.mk (Types.TEnum name) (Tast.TVariant (name, value))
  | None ->
      emit env
        (Diagnostic.error_at fspan "no variant"
        |> Diagnostic.label
             (Printf.sprintf "on enum %s" (show_ty env (Types.TEnum name))));
      dummy_texpr

let synth_struct_field env span te ty fname fspan =
  let rec peel depth = function
    | Types.TStruct (sname, _) -> Some (sname, depth)
    | Types.TAlias (_, base) -> peel depth base
    | Types.TPointer t -> peel (depth + 1) t
    | _ -> None
  in
  match peel 0 ty with
  | None when resolve_ty ty = Types.TError -> dummy_texpr
  | None ->
      let shown = show_ty env ty in
      emit env (Diagnostic.with_type span "type has no fields" shown);
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
      match find_field info fname with
      | Some (field_id, ft) -> Tast.mk ft (Tast.TFieldAccess (te, field_id))
      | None ->
          emit env
            (Diagnostic.error_at fspan "no field"
            |> Diagnostic.label
                 (Printf.sprintf "on struct %s" (Qname.show sname)));
          dummy_texpr)

let synth_conversion env span (te : Tast.texpr) ty =
  if not (cast_ok te.ty ty) then begin
    let d =
      Diagnostic.error "invalid conversion"
      |> Diagnostic.at span
      |> Diagnostic.label
           (Printf.sprintf "cannot convert %s to %s" (show_ty env te.ty)
              (show_ty env ty))
    in
    let d =
      if resolve_ty ty = Types.TBool then
        Diagnostic.help "compare with zero instead e.g. `x != 0`" d
      else d
    in
    emit env d
  end
  else if te.ty = ty then
    emit env
      (Diagnostic.warning "cast has no effect"
      |> Diagnostic.at span
      |> Diagnostic.label (Printf.sprintf "already %s" (show_ty env ty))
      |> Diagnostic.help "remove the cast");
  if te.ty = Types.TError || ty = Types.TError then dummy_texpr
  else Tast.mk ty (Tast.TCast te)

let const_binop env span result_ty op left right =
  match (left.const, right.const) with
  | Some a, Some b ->
      begin try Constant.binop span op ~result_ty a b
      with Diagnostic.Errors ds ->
        List.iter (emit env) ds;
        None
      end
  | None, _ | _, None -> None

(* An untyped literal takes the wanted type and checks its base *)
let check_int_literal env (e : expr) want target value =
  match adopt_int_literal env e.span want target ~neg:false value with
  | Some typed -> typed
  | None ->
      emit env
        (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
           ~found:"i32");
      Tast.mk (Types.TInt I32) (Tast.TInt value)

let check_float_literal env (e : expr) want target value =
  match target with
  | Types.TFloat _ | Types.TError -> Tast.mk want (Tast.TFloat value)
  | _ ->
      emit env
        (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
           ~found:"f64");
      Tast.mk (Types.TFloat F64) (Tast.TFloat value)

(* This figures out the type of a field access *)
let synth_typed_field env span (te : Tast.texpr) fname fspan =
  let ty = te.ty in
  match (resolve_ty ty, fname) with
  | Types.TStr, name when name = len_name ->
      Tast.mk (Types.TInt Usize) (Tast.TLen te)
  | Types.TStr, _ -> no_such_field env fspan ty
  | (Types.TArray _ | Types.TSlice _), name when name = len_name ->
      Tast.mk (Types.TInt Usize) (Tast.TLen te)
  | (Types.TArray (elem, _) | Types.TSlice elem), name when name = ptr_name ->
      Tast.mk (Types.TPointer elem) (Tast.TDataPtr te)
  | (Types.TArray _ | Types.TSlice _), _ -> no_such_field env fspan ty
  | Types.TOpaquePtr, _ ->
      emit env (Diagnostic.opaque_operation span "access a field of");
      dummy_texpr
  | _, _ -> synth_struct_field env span te ty fname fspan

(* A qualified callee is one symbol so it still calls directly *)
let direct_callee env (callee : expr) =
  match callee.desc with
  | Ident _ | Path _ -> (
      match Resolve.sym_at_opt env.ctx.symbols callee.span with
      | Some symbol when Symbol.is_func symbol.Symbol.kind -> Some symbol
      | Some _ | None -> None)
  | _ -> None

let rec ty_of_ast env t =
  match t.tdesc with
  | ErrorType -> Types.TError
  | Named (path, name) -> named_ty env t.tspan (Ast.show_named path name)
  | Pointer inner when builtin_at env inner.tspan = Some BOpaque ->
      Types.TOpaquePtr
  | Pointer t -> lift_ty (fun ty -> Types.TPointer ty) (ty_of_ast env t)
  | Array (e, t) -> array_ty_of_ast env e t
  | Slice t -> lift_ty (fun ty -> Types.TSlice ty) (ty_of_ast env t)
  | FuncPtr (abi, ps, ret) -> (
      let pts = List.map (ty_of_ast env) ps in
      let rt =
        match ret with Some t -> return_ty_of_ast env t | None -> Types.TUnit
      in
      match (resolve_abi env abi, rt, List.mem Types.TError pts) with
      | Types.AbiError, _, _ -> Types.TError
      | abi, rt, false when rt <> Types.TError -> Types.TFunc (pts, rt, abi)
      | _ -> Types.TError)
  | UnitType -> Types.TUnit

and array_ty_of_ast env size element =
  match (ty_of_ast env element, size.desc) with
  | Types.TError, _ | _, ErrorExpr -> Types.TError
  | ty, _ -> Types.TArray (ty, eval_array_size env size)

and return_ty_of_ast env t =
  match builtin_at env t.tspan with
  | Some (BTy Types.TNever) -> Types.TNever
  | Some (BTy _) | Some BOpaque | None -> ty_of_ast env t

(* An array size may name a global not collected yet so type it now *)
and pending_global_ty env span key fact =
  try
    force_deferred span fact.declared_ty ~on_error:Types.TError (fun () ->
        let decl_env = decl_env_at env fact.declaration.span in
        let t = global_ty decl_env fact.declaration in
        Symbol.Table.replace env.ctx.globals key (t, fact.declaration.kind);
        t)
  with Diagnostic.Errors ds ->
    List.iter (emit env) ds;
    Types.TError

and lookup_var env span =
  match lookup_var_opt env span with
  | Some t -> t
  | None -> lookup_non_local env span

(* A name outside the scopes is a global or hasn't been collected yet *)
and lookup_non_local env span =
  let key = key_at env span in
  match Symbol.Table.find_opt env.ctx.globals key with
  | Some (t, _) -> t
  | None -> (
      match Symbol.Table.find_opt env.ctx.global_facts key with
      | Some fact -> pending_global_ty env span key fact
      | None -> (
          match Symbol.Table.find_opt env.ctx.func_sigs key with
          | Some fsig -> Types.TFunc (fsig.param_tys, fsig.ret_ty, fsig.abi)
          | None ->
              emit env (Diagnostic.undefined_name span "variable");
              Types.TInt I32))

(* This pass does the bidirectional type checking *)

(* Stamp the source span here so the Tast.mk sites underneath stay span free *)
and synth env e =
  let typed = synth_operand env e in
  report_const_range env typed;
  typed

(* An operand folds into its parent so the parent reports the range *)
and synth_operand env (e : expr) = stamp env e.span (synth_desc env e)

and stamp env span te =
  if Option.is_none te.const then
    { te with span; const = const_of env ~span te }
  else { te with span }

(* A node takes its value from children that already carry theirs so no
   expression gets walked twice *)
and const_of env ~span te =
  match te.desc with
  | Tast.TInt n -> Some (Constant.of_literal te.ty n)
  | Tast.TBool b -> Some (Constant.VBool b)
  | Tast.TChar cp -> Some (Constant.VChar cp)
  | Tast.TFloat f -> Some (Constant.of_float (float_kind_of te.ty) f)
  | Tast.TSizeOf t ->
      Some
        (Constant.of_literal te.ty
           (Int64.of_int (Layout.ty_size env.ctx.layouts t)))
  | Tast.TIdent s -> const_of_ident env span te.ty s
  | Tast.TCast operand -> Option.map (Constant.cast te.ty) operand.const
  | Tast.TUnOp (op, operand) -> (
      try Option.bind operand.const (Constant.unop span op ~result_ty:te.ty)
      with Diagnostic.Errors ds ->
        List.iter (emit env) ds;
        None)
  | Tast.TBinOp (op, l, r) -> const_binop env span te.ty op l r
  | _ -> None

and const_of_ident env span ty symbol =
  match resolve_ty ty with
  | Types.TInt _ | Types.TFloat _ | Types.TBool | Types.TChar ->
      const_of_symbol env symbol span
  | _ -> None

and synth_desc env e =
  match e.desc with
  | ErrorExpr -> dummy_texpr
  | Int (n, suf) ->
      let kind = match suf with Some s -> suffix_kind s | None -> I32 in
      Tast.mk (Types.TInt kind) (Tast.TInt n)
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      synth_desc env { operand with span = e.span }
  | UnOp (Neg, { desc = Int (n, Some s); _ }) ->
      let kind = suffix_kind s in
      Tast.mk (Types.TInt kind) (Tast.TInt (Int64.neg n))
  | Float (f, suf) ->
      let kind = match suf with Some s -> float_suffix_kind s | None -> F64 in
      Tast.mk (Types.TFloat kind) (Tast.TFloat f)
  | Bool b -> Tast.mk Types.TBool (Tast.TBool b)
  | Null -> Tast.mk Types.TNull Tast.TNull
  | String s -> Tast.mk (Types.TPointer (Types.TInt I8)) (Tast.TCStr s)
  | Char c -> Tast.mk Types.TChar (Tast.TChar c)
  | Ident _ ->
      let s = sym env e.span in
      if s.Symbol.kind = Symbol.Error then dummy_texpr
      else if s.Symbol.kind = Symbol.Module then (
        add_error env e.span "module requires a member";
        dummy_texpr)
      else
        let t = lookup_var env e.span in
        Tast.mk t (Tast.TIdent s)
  | Call (callee, args) -> synth_call env e.span callee args
  | BinOp (op, l, r) -> synth_binop env op l r
  | Assign (base, l, r) -> synth_assign env base l r
  | UnOp (op, e) -> synth_unop env op e
  | Path p -> synth_path env e.span p
  | FieldAccess (inner_e, fname, fspan) ->
      synth_field env e.span inner_e fname fspan
  | BitCast (operand, t) ->
      let te = synth_operand env operand in
      let ty = ty_of_ast env t in
      if
        te.ty <> Types.TError && ty <> Types.TError && not (bitcast_ok te.ty ty)
      then
        emit env
          (Diagnostic.error "invalid bitcast"
          |> Diagnostic.at e.span
          |> Diagnostic.label
               (Printf.sprintf "cannot reinterpret %s as %s" (show_ty env te.ty)
                  (show_ty env ty))
          |> Diagnostic.help
               "both sides need the same width and neither may be a float");
      if te.ty = Types.TError || ty = Types.TError then dummy_texpr
      else Tast.mk ty (Tast.TCast te)
  | SizeOf t -> synth_size_of env t
  | Range _ | RangeInclusive _ | RangeFrom _ | RangeTo _ | RangeToInclusive _
  | RangeFull ->
      add_error env e.span "range is only valid in a for loop or slice";
      dummy_texpr
  | ArrayLit [] ->
      add_error env e.span "cannot infer type of empty array literal";
      dummy_texpr
  | ArrayLit (first :: rest) -> synth_array_lit env first rest
  | Index (base, idx) -> synth_index env e.span base idx
  | Undefined ->
      add_error env e.span "cannot infer type of undefined";
      dummy_texpr
  | StructLit (path, name, name_span, inits) ->
      synth_struct_lit env e.span path name name_span inits
  | Block body ->
      let tb, ty = check_scoped_block env e.span body Infer in
      Tast.mk ty (Tast.TBlock tb)
  | If (branches, else_body) -> check_if env e.span branches else_body None
  | Match (scrutinee, arms) -> check_match env scrutinee arms Infer
  | While (label, cond, body) ->
      let tc = check env cond Types.TBool in
      let loop = new_loop label ~valued:false in
      let tb, _ = check_scoped_block ~loop env e.span body Discard in
      (* A while true with no break loops forever so it never yields control *)
      let diverges =
        match cond.desc with
        | Bool true -> not (Reachability.loop_has_break ?label body)
        | _ -> false
      in
      Tast.mk
        (if diverges then Types.TNever else Types.TUnit)
        (Tast.TWhile (label, tc, tb))
  | For (label, name, nspan, iter, body) ->
      synth_for env e.span label name nspan iter body
  | Binding (kind, name, nspan, ann, init) ->
      snd (check_binding env kind name nspan ann init)
  | Return init -> synth_return env e.span init
  | Break (label, value) -> synth_break env e.span label value
  | Continue label ->
      ignore (find_loop_or_error env e.span "`continue` outside a loop" label);
      Tast.mk Types.TNever (Tast.TContinue label)
  | PairAssign (ft, st, fv, sv) -> synth_pair_assign env ft st fv sv
  | Loop (label, body) -> check_loop_expr env e.span label body None
  | Unit -> Tast.mk Types.TUnit Tast.TUnit

and synth_path_type env span p =
  let inner = Ast.owner_expr p in
  let name, name_span = p.Ast.member in
  match Symbol.Table.find_opt env.ctx.type_defs (key_at env inner.span) with
  | Some (Enum_type { contents = Completed info }) ->
      synth_variant env inner info name name_span
  | Some (Enum_type { contents = Unstarted | Running }) -> dummy_texpr
  | Some (Struct_type _ | Alias_type _ | Builtin_type _) ->
      emit env
        (Diagnostic.error_at inner.span "expected a value"
        |> Diagnostic.label "this names a type");
      dummy_texpr
  | None -> synth_field env span inner name name_span

and synth_path env span p =
  match Resolve.sym_at_opt env.ctx.symbols span with
  | Some symbol when symbol.Symbol.kind = Symbol.Error -> dummy_texpr
  | Some symbol
    when Symbol.is_func symbol.Symbol.kind
         || Symbol.is_global symbol.Symbol.kind ->
      Tast.mk (lookup_var env span) (Tast.TIdent symbol)
  | Some _ | None -> (
      match synth_value_path env p with
      | Some typed -> typed
      | None -> synth_path_type env span p)

and synth_size_of env typ =
  match ty_of_ast env typ with
  | Types.TError -> dummy_texpr
  | ty -> Tast.mk (Types.TInt Usize) (Tast.TSizeOf ty)

and synth_struct_lit env span path name name_span inits =
  match Symbol.Table.find_opt env.ctx.type_defs (key_at env name_span) with
  | Some (Struct_type { contents = Completed info }) ->
      let fields =
        match inits with
        | (None, _, _) :: _ -> positional_fields env span info inits
        | _ -> named_fields env info inits
      in
      let qname = qname_at env name_span (Ast.show_named path name) in
      Tast.mk (Types.TStruct (qname, [])) (Tast.TStructLit (qname, fields))
  | Some
      ( Struct_type { contents = Unstarted | Running }
      | Builtin_type _ | Alias_type _ | Enum_type _ )
  | None ->
      emit env (Diagnostic.undefined_name name_span "struct");
      dummy_texpr

(* A quiet probe lets a sibling anchor the block type *)
and block_result_ty env body =
  let quiet = probing env in
  let inner = push_scope quiet in
  let _, tb = check_block inner Ast.dummy_span body Infer in
  tblock_ty tb

and coerce_common env first rest =
  let add_typed common = function
    | Typed (_, typed) -> common_ty common typed.ty
    | Contextual _ -> common
  in
  let common = List.fold_left add_typed (add_typed Types.TNever first) rest in
  let common, first =
    if common <> Types.TNever then (common, first)
    else
      match first with
      | Contextual source ->
          let typed = synth env source in
          (typed.ty, Typed (source, typed))
      | Typed (_, typed) -> (typed.ty, first)
  in
  let coerce = function
    | Contextual source -> check env source common
    | Typed (source, typed) -> coerce_expr env source common typed
  in
  (coerce first :: List.map coerce rest, common)

and coerce_common_pair env ~(contextual : expr -> bool) left right =
  if contextual left && not (contextual right) then
    let typed_right = synth_operand env right in
    let common = typed_right.ty in
    (check_operand env left common, typed_right, common)
  else if contextual right then
    let typed_left = synth_operand env left in
    let common = typed_left.ty in
    (typed_left, check_operand env right common, common)
  else
    let typed_left = synth_operand env left in
    let typed_right = synth_operand env right in
    let common = common_ty typed_left.ty typed_right.ty in
    ( coerce_expr env left common typed_left,
      coerce_expr env right common typed_right,
      common )

and synth_array_lit env first rest =
  let probe e =
    if arm_is_flexible e then Contextual e else Typed (e, synth env e)
  in
  let tes, elem = coerce_common env (probe first) (List.map probe rest) in
  let elem =
    match elem with
    | (Types.TUnit | Types.TNever) as t ->
        emit env
          (Diagnostic.with_type first.span "array element cannot have this type"
             (show_ty env t));
        Types.TError
    | t -> t
  in
  Tast.mk (Types.TArray (elem, List.length tes)) (Tast.TArrayLit tes)

and named_fields env info (inits : (Ast.name option * Ast.span * expr) list) =
  let seen = Hashtbl.create 4 in
  let check_named_field (fname, fspan, e) =
    match fname with
    | None -> None
    | Some fname -> (
        match find_field info fname with
        | None ->
            emit env (Diagnostic.error_at fspan "no field");
            None
        | Some _ when Hashtbl.mem seen fname ->
            emit env (Diagnostic.error_at fspan "duplicate field");
            None
        | Some (field_id, ft) ->
            Hashtbl.replace seen fname ();
            Some (field_id, check env e ft))
  in
  let written_fields = List.filter_map check_named_field inits in
  (* When you omit a fields they're 0 init *)
  let default_field field_id (fname, ft) =
    if Hashtbl.mem seen fname then None
    else Some (field_id, Tast.mk ft Tast.TZero)
  in
  let rec defaults field_id fields =
    match fields with
    | [] -> []
    | field :: rest -> (
        let rest = defaults (field_id + 1) rest in
        match default_field field_id field with
        | Some entry -> entry :: rest
        | None -> rest)
  in
  (* The written field order controls value evaluation *)
  written_fields @ defaults 0 info.field_tys

(* A positional literal must define every field *)
and positional_fields env span info
    (inits : (Ast.name option * Ast.span * expr) list) =
  let expected = List.length info.field_tys in
  let found = List.length inits in
  if found <> expected then
    emit env
      (Diagnostic.error "wrong number of fields"
      |> Diagnostic.at span
      |> Diagnostic.label
           (Printf.sprintf "expected %d, found %d" expected found));
  let rec zip field_id fields inits =
    match (fields, inits) with
    | [], inits ->
        walk_unpaired_args env (List.map (fun (_, _, init) -> init) inits);
        []
    | (_, ft) :: fields, [] ->
        (field_id, Tast.mk ft Tast.TZero) :: zip (field_id + 1) fields []
    | (_, ft) :: fields, (_, _, init) :: inits ->
        (field_id, check env init ft) :: zip (field_id + 1) fields inits
  in
  zip 0 info.field_tys inits

and reconcile_if_result env (branches : (expr * block Ast.spanned) list) else_b
    =
  reconcile_arms env
    (List.map (fun (_, { Ast.value; _ }) -> value) branches @ [ else_b ])

(* Literals bend to the common rigid type so they don't anchor it *)
and reconcile_arms env bodies =
  let add_candidate (rigid, flexible) body =
    let candidate = block_result_ty env body in
    if block_is_flexible body then (rigid, common_ty flexible candidate)
    else (common_ty rigid candidate, flexible)
  in
  let rigid, flexible =
    List.fold_left add_candidate (Types.TNever, Types.TNever) bodies
  in
  if rigid = Types.TNever then flexible else rigid

and check_value_for_use env (e : expr) use =
  match (use, e.desc) with
  | Infer, _ -> synth env e
  | Expect want, _ -> check env e want
  | Discard, If (branches, else_body) ->
      { (check_if_discarded env e.span branches else_body) with span = e.span }
  | Discard, Match (scrutinee, arms) ->
      { (check_match env scrutinee arms Discard) with span = e.span }
  | Discard, _ ->
      let te = synth env e in
      warn_discarded_operation env e te;
      te

(* Thread env so a binding is visible to later elements *)
and check_block env span body use =
  let rec go env diverged acc elems =
    match elems with
    | [] ->
        verify_unit_result env span use;
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
        go env (diverged || te.ty = Types.TNever) (te :: acc) rest
  in
  go env false [] body

and check_elem env item use : local_env * Tast.texpr =
  match item with
  | Decl _ ->
      verify_unit_result env (block_item_span item) use;
      (env, Tast.mk Types.TUnit Tast.TLocalDecl)
  | Expr ({ desc = Binding (kind, name, nspan, ann, init); _ } as e) ->
      let env', tbind = check_binding env kind name nspan ann init in
      verify_unit_result env e.span use;
      (env', tbind)
  | Expr e -> (env, check_value_for_use env e use)

(* Push a scope for the block then flag any leftover bindings *)
and check_scoped_block ?loop env span body use =
  let base =
    match loop with
    | Some lc -> { env with loops = lc :: env.loops }
    | None -> env
  in
  let inner = push_scope base in
  let final_inner, tb = check_block inner span body use in
  warn_unused_in_scope final_inner;
  (tb, tblock_ty tb)

and check_binding env kind name nspan ann init =
  let t, te =
    match (ann, init) with
    | Some a, Some e ->
        let want = ty_of_ast env a in
        let te = check env e want in
        (want, te)
    | None, Some e ->
        let te = synth env e in
        (te.ty, te)
    | Some a, None ->
        let want = ty_of_ast env a in
        (want, Tast.mk want Tast.TZero)
    | None, None ->
        emit env (Diagnostic.cannot_infer nspan);
        (* A real type here would cause unrelated later mismatches *)
        (Types.TError, dummy_texpr)
  in
  if kind = Comptime then (
    verify_const_scalar env nspan t;
    let value = const_value_or env dummy_value te in
    Symbol.Table.replace (local_consts env) (Symbol.key (sym env nspan)) value);
  ( extend_var env nspan name t,
    Tast.mk Types.TUnit (Tast.TBinding (kind, sym env nspan, t, te)) )

and synth_return env span init =
  if env.ret_ty = Types.TNever then
    add_error env span "a never function cannot return";
  match init with
  | None ->
      if
        env.ret_ty <> Types.TNever && env.ret_ty <> Types.TUnit
        && not env.entry_function
      then add_error env span "empty return in non-unit function";
      Tast.mk Types.TNever (Tast.TReturn None)
  | Some e when env.ret_ty = Types.TNever ->
      Tast.mk Types.TNever (Tast.TReturn (Some (synth env e)))
  | Some e ->
      let te = check env e env.ret_ty in
      Tast.mk Types.TNever (Tast.TReturn (Some te))

and check_loop_expr env span label body want =
  let loop = new_loop label ~valued:true in
  loop.result <-
    Option.fold ~none:InferLoopResult ~some:(fun t -> ExpectLoopResult t) want;
  let tb, _ = check_scoped_block ~loop env span body Discard in
  let ty =
    match (loop.result, loop.bare_break) with
    | FlexibleLoopResult (t, _, _), _ | RigidLoopResult (t, _, _), _ -> t
    | (InferLoopResult | ExpectLoopResult _), None -> Types.TNever
    | InferLoopResult, Some _ -> Types.TUnit
    | ExpectLoopResult want, Some break_span ->
        emit env
          (Diagnostic.type_mismatch break_span ~expected:(show_ty env want)
             ~found:"()");
        Types.TUnit
  in
  Tast.mk ty (Tast.TLoop (label, tb))

and check_valued_break env lc ve =
  let flexible = arm_is_flexible ve in
  let check_flexible_values want values =
    List.iter (fun e -> ignore (check env e want)) values
  in
  let report_bare ty =
    let report first =
      emit env
        (Diagnostic.break_disagree ve.span
           (Printf.sprintf "breaks with %s" (show_ty env ty))
           ~other:first ~other_message:"no value here")
    in
    Option.iter report lc.bare_break
  in
  match (lc.result, flexible) with
  | InferLoopResult, _ ->
      let typed = synth env ve in
      if typed.ty <> Types.TNever then begin
        report_bare typed.ty;
        lc.result <-
          (if flexible then FlexibleLoopResult (typed.ty, ve.span, [ ve ])
           else RigidLoopResult (typed.ty, ve.span, []))
      end;
      typed
  | ExpectLoopResult want, _ ->
      let typed = check env ve want in
      if typed.ty <> Types.TNever then begin
        report_bare want;
        lc.result <-
          RigidLoopResult (want, ve.span, if flexible then [ ve ] else [])
      end;
      typed
  | FlexibleLoopResult (want, first, values), true ->
      lc.result <- FlexibleLoopResult (want, first, ve :: values);
      check env ve want
  | RigidLoopResult (want, first, values), true ->
      lc.result <- RigidLoopResult (want, first, ve :: values);
      check env ve want
  (* The first value that can't bend decides the type for the rest *)
  | FlexibleLoopResult (_, first, values), false ->
      let typed = synth env ve in
      if typed.ty = Types.TNever then typed
      else begin
        check_flexible_values typed.ty values;
        lc.result <- RigidLoopResult (typed.ty, first, values);
        typed
      end
  | RigidLoopResult (want, first, values), false ->
      let typed = synth env ve in
      let common = common_ty want typed.ty in
      if not (ty_equal common want) then begin
        check_flexible_values common values;
        lc.result <- RigidLoopResult (common, first, values)
      end;
      coerce_expr env ve common typed

and synth_break env span label value =
  let target = find_loop_or_error env span "`break` outside a loop" label in
  let tv =
    match (value, target) with
    | None, None -> None
    | None, Some lc ->
        verify_bare_break env span lc;
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
  Tast.mk Types.TNever (Tast.TBreak (label, tv))

and synth_for env span label name nspan iter body =
  let titer, elem_ty =
    match iter.desc with
    | Range (lo, hi) ->
        let tlo, thi, t = check_range_bounds env lo hi in
        (Tast.mk t (Tast.TRange (tlo, thi)), t)
    | RangeInclusive (lo, hi) ->
        let tlo, thi, t = check_range_bounds env lo hi in
        (Tast.mk t (Tast.TRangeInclusive (tlo, thi)), t)
    | _ -> (
        let ti = synth env iter in
        match resolve_ty ti.ty with
        | Types.TError -> (ti, Types.TError)
        | Types.TArray (elem, _) | Types.TSlice elem -> (ti, elem)
        | t ->
            emit env
              (Diagnostic.with_type iter.span "cannot iterate" (show_ty env t));
            (ti, Types.TInt I32))
  in
  let loop = new_loop label ~valued:false in
  let inner = push_scope { env with loops = loop :: env.loops } in
  let inner = extend_var inner nspan name elem_ty in
  let final_inner, tb = check_block inner span body Discard in
  warn_unused_in_scope final_inner;
  Tast.mk Types.TUnit (Tast.TFor (label, sym env nspan, elem_ty, titer, tb))

(* Each discarded arm checks its own trailing value *)
and check_if_discarded env span (branches : (expr * block Ast.spanned) list)
    else_body =
  let arm body = fst (check_scoped_block env span body Discard) in
  let tbranches =
    List.map
      (fun (c, { Ast.value = body; _ }) -> (check env c Types.TBool, arm body))
      branches
  in
  let telse = Option.map (fun { Ast.value = body; _ } -> arm body) else_body in
  let ty = if_result_ty tbranches telse ~fallback_ty:Types.TUnit in
  Tast.mk ty (Tast.TIf (tbranches, telse))

(* A missing expected type asks the whole if to synthesize *)
and check_if env span (branches : (expr * block Ast.spanned) list) else_body
    want =
  match (want, else_body) with
  | None, None ->
      let tbranches =
        List.map
          (fun (c, { Ast.value = body; _ }) ->
            ( check env c Types.TBool,
              fst (check_scoped_block env span body Discard) ))
          branches
      in
      Tast.mk Types.TUnit (Tast.TIf (tbranches, None))
  | None, Some { Ast.value = else_b; _ } ->
      let rty = reconcile_if_result env branches else_b in
      (* A probe only wants the type so don't build the tree twice *)
      if is_probing env then Tast.mk rty Tast.TErrorExpr
      else check_if env span branches else_body (Some rty)
  | Some w, _ ->
      (* Each arm owns its mismatch location *)
      let tbranches =
        List.map
          (fun (c, { Ast.value = body; span = bspan }) ->
            ( check env c Types.TBool,
              fst (check_scoped_block env bspan body (Expect w)) ))
          branches
      in
      let telse =
        match else_body with
        | Some { Ast.value = body; span = bspan } ->
            Some (fst (check_scoped_block env bspan body (Expect w)))
        | None ->
            if resolve_ty w <> Types.TUnit then
              emit env
                (Diagnostic.type_mismatch span ~expected:(show_ty env w)
                   ~found:(show_ty env Types.TUnit));
            None
      in
      let ty = if_result_ty tbranches telse ~fallback_ty:w in
      Tast.mk ty (Tast.TIf (tbranches, telse))

(* A binding lands in the scope of the arm so the env comes back out *)
and check_pattern env sty pat =
  match pat.pdesc with
  | PatWild -> (env, Some Tast.TPatWild)
  | PatBind name ->
      let symbol = sym env pat.pspan in
      (* The resolver already decided so a comptime name reads as a constant *)
      if Symbol.is_comptime symbol.Symbol.kind then
        check_pattern env sty
          { pat with pdesc = PatValue { desc = Ident name; span = pat.pspan } }
      else
        (extend_var env pat.pspan name sty, Some (Tast.TPatBind (symbol, sty)))
  | PatValue e -> (
      let te = check env e sty in
      (* TODO: comparing these needs more than the integer test an arm emits *)
      let not_comparable () =
        emit env
          (Diagnostic.error_at pat.pspan "pattern is not comparable"
          |> Diagnostic.label ("cannot test " ^ show_ty env te.ty));
        (env, None)
      in
      (* TODO: a named constant and a range should both work as patterns *)
      let not_a_literal () =
        emit env
          (Diagnostic.error_at pat.pspan "pattern is not a literal"
          |> Diagnostic.help "an arm names a literal or an enum variant");
        (env, None)
      in
      match (te.desc, te.const) with
      | Tast.TVariant (_, value), _ -> (env, Some (Tast.TPatConst value))
      | Tast.TInt n, _ -> (env, Some (Tast.TPatConst n))
      | Tast.TBool b, _ -> (env, Some (Tast.TPatConst (if b then 1L else 0L)))
      | Tast.TChar c, _ -> (env, Some (Tast.TPatConst (Int64.of_int c)))
      | Tast.TErrorExpr, _ -> (env, None)
      | (Tast.TFloat _ | Tast.TStr _ | Tast.TCStr _), _ -> not_comparable ()
      | _, Some (Constant.VFloat _) -> not_comparable ()
      | _, Some value -> (env, Some (Tast.TPatConst (Constant.int_of value)))
      | _, None when is_integer te.ty ->
          let value = const_value_or env dummy_value te in
          (env, Some (Tast.TPatConst (Constant.int_of value)))
      | _, None -> not_a_literal ())

and check_match env scrutinee arms use =
  let ts = synth env scrutinee in
  let bodies = List.map (fun a -> a.arm_body.Ast.value) arms in
  match use with
  | Infer when is_probing env ->
      (* A probe only wants the type so don't build the tree twice *)
      Tast.mk (reconcile_arms env bodies) Tast.TErrorExpr
  | Infer -> check_match_arms env ts arms (Some (reconcile_arms env bodies))
  | Expect w -> check_match_arms env ts arms (Some w)
  | Discard -> check_match_arms env ts arms None

and check_match_arms env ts arms want =
  let arm_use = match want with Some w -> Expect w | None -> Discard in
  (* A match without a catch all can fall through *)
  let seen = Hashtbl.create 8 in
  let caught_all = ref false in
  let record pat tpat =
    if !caught_all then add_error env pat.pspan "arm never runs"
    else
      match tpat with
      | Tast.TPatWild | Tast.TPatBind _ -> caught_all := true
      | Tast.TPatConst n ->
          if Hashtbl.mem seen n then add_error env pat.pspan "duplicate pattern"
          else Hashtbl.add seen n ()
  in
  let check_arm a =
    let arm_env, tpat = check_pattern (push_scope env) ts.ty a.pat in
    Option.iter (record a.pat) tpat;
    (* A broken pattern still checks its body so the errors inside show up *)
    let tbody, _ =
      check_scoped_block arm_env a.arm_body.Ast.span a.arm_body.Ast.value
        arm_use
    in
    Option.map (fun tpat -> { tpat; tbody }) tpat
  in
  let tarms = List.filter_map check_arm arms in
  (* A catch all with every arm diverging is the only way nothing falls past *)
  let diverges =
    !caught_all
    && List.for_all (fun a -> tblock_ty a.tbody = Types.TNever) tarms
  in
  let ty =
    match (diverges, want) with
    | true, _ -> Types.TNever
    | false, Some w -> w
    | false, None -> Types.TUnit
  in
  Tast.mk ty (Tast.TMatch (ts, tarms))

(* This has to be this type *)
and check env e want =
  let typed = check_operand env e want in
  report_const_range env typed;
  typed

and check_operand env e want = stamp env e.span (check_desc env e want)

(* This synthesizes then checks the result against want *)
and check_by_synth env e want =
  let typed = synth_operand env e in
  coerce_expr env e want typed

and check_size_literal env (e : expr) want target typ =
  match ty_of_ast env typ with
  | Types.TError -> dummy_texpr
  | ty ->
      let size = Int64.of_int (Layout.ty_size env.ctx.layouts ty) in
      let kind = int_kind_of want in
      if not (Constant.representable kind (Constant.of_magnitude size)) then
        emit env
          (Diagnostic.error "size does not fit"
          |> Diagnostic.at e.span
          |> Diagnostic.label
               (Printf.sprintf "%Ld does not fit in %s" size
                  (show_ty env target)));
      Tast.mk want (Tast.TSizeOf ty)

and check_neg_int_literal env (e : expr) want target value =
  match adopt_int_literal env e.span want target ~neg:true value with
  | Some typed -> typed
  | None -> check_operand env { e with desc = Int (Int64.neg value, None) } want

and check_array_literal env (e : expr) want elements =
  match resolve_ty want with
  | Types.TArray (element, length) ->
      if List.compare_length_with elements length <> 0 then
        emit env
          (Diagnostic.arity e.span
             ~expected:(Printf.sprintf "expected %d elements" length)
             ~found:(List.length elements));
      let typed = List.map (fun source -> check env source element) elements in
      Tast.mk (Types.TArray (element, length)) (Tast.TArrayLit typed)
  | _ -> check_by_synth env e want

and check_desc env e want =
  let target = resolve_ty want in
  match e.desc with
  | ErrorExpr -> dummy_texpr
  | Int (value, None) -> check_int_literal env e want target value
  | Float (value, None) -> check_float_literal env e want target value
  | String value when target = Types.TStr -> Tast.mk want (Tast.TStr value)
  | String _ -> check_by_synth env e want
  | SizeOf typ when is_integer want -> check_size_literal env e want target typ
  | UnOp (Neg, { desc = Int (value, None); _ }) ->
      check_neg_int_literal env e want target value
  | UnOp (Neg, { desc = Float (f, suf); _ }) ->
      check_operand env { e with desc = Float (-.f, suf) } want
  | UnOp (Neg, { desc = Int (_, Some _); _ }) -> check_by_synth env e want
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      check_operand env { operand with span = e.span } want
  | UnOp (((Neg | Pos | BitNot) as op), operand) when unop_accepts op want ->
      Tast.mk want (Tast.TUnOp (op, check_operand env operand want))
  | ArrayLit elements -> check_array_literal env e want elements
  | BinOp (((Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor) as op), l, r)
    when binop_accepts op want ->
      Tast.mk want
        (Tast.TBinOp (op, check_operand env l want, check_operand env r want))
  | BinOp (((Lshift | Rshift) as op), l, r) when is_integer want ->
      let base = check_operand env l want in
      let count = synth env r in
      verify_shift_count env r.span count;
      Tast.mk want (Tast.TBinOp (op, base, count))
  | Block body ->
      let tb, ty = check_scoped_block env e.span body (Expect want) in
      Tast.mk ty (Tast.TBlock tb)
  | If (branches, else_body) ->
      check_if env e.span branches else_body (Some want)
  | Match (scrutinee, arms) -> check_match env scrutinee arms (Expect want)
  | Loop (label, body) -> check_loop_expr env e.span label body (Some want)
  | Undefined -> Tast.mk want Tast.TUndef
  | _ -> check_by_synth env e want

and coerce_expr env e want te =
  let got = te.ty in
  if compatible want got then adopt_slice want te
  else if widens_to got want then
    let widened = Tast.mk ~span:e.span want (Tast.TCast te) in
    { widened with const = const_of env ~span:e.span widened }
  else begin
    let mismatch =
      Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
        ~found:(show_ty env got)
    in
    let mismatch =
      match (e.desc, resolve_ty want) with
      | Assign (None, _, _), Types.TBool ->
          Diagnostic.help "did you mean `==` to compare?" mismatch
      | _ -> mismatch
    in
    emit env mismatch;
    te
  end

and check_matching_operands env l r =
  coerce_common_pair env ~contextual:is_num_literal l r

and check_range_bounds env lo hi =
  let tlo, thi, t = check_matching_operands env lo hi in
  if not (is_integer t) then
    add_error env lo.span "range bounds must be integers";
  (* A bound never reaches the walk since a range is not a value *)
  report_const_range env tlo;
  report_const_range env thi;
  (tlo, thi, t)

(* A bad count drops the args so walk them or their errors never show *)
and walk_unpaired_args env args = List.iter (fun a -> ignore (synth env a)) args

and check_args env span fsig args =
  let n_params = List.length fsig.param_tys in
  let n_args = List.length args in
  let expected_args =
    Printf.sprintf "%d argument%s" n_params (if n_params = 1 then "" else "s")
  in
  if fsig.variadic then
    if n_args < n_params then (
      emit env
        (Diagnostic.arity span
           ~expected:("expected at least " ^ expected_args)
           ~found:n_args);
      walk_unpaired_args env args;
      [])
    else
      let fixed = List.take n_params args in
      let rest = List.drop n_params args in
      (* C reads a float vararg as a double so widen it first *)
      let promote_vararg e =
        let te = synth env e in
        match resolve_ty te.ty with
        | Types.TFloat F32 ->
            Tast.mk ~span:e.span (Types.TFloat F64) (Tast.TCast te)
        | _ -> te
      in
      List.map2 (check env) fixed fsig.param_tys @ List.map promote_vararg rest
  else if n_params <> n_args then (
    emit env
      (Diagnostic.arity span
         ~expected:("expected " ^ expected_args)
         ~found:n_args);
    walk_unpaired_args env args;
    [])
  else List.map2 (check env) args fsig.param_tys

and synth_binop env op l r =
  match op with
  | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor ->
      let tl, tr, t = check_matching_operands env l r in
      verify_operands env l.span op t;
      Tast.mk t (Tast.TBinOp (op, tl, tr))
  | Lshift | Rshift ->
      let tl = synth_operand env l in
      let tr = synth_operand env r in
      verify_operands env l.span op tl.ty;
      verify_shift_count env r.span tr;
      Tast.mk tl.ty (Tast.TBinOp (op, tl, tr))
  | Eq | Neq | Lt | Gt | Lte | Gte ->
      let tl, tr, t = check_comparison_operands env l r in
      verify_operands env l.span op t;
      Tast.mk Types.TBool (Tast.TBinOp (op, tl, tr))
  | And | Or ->
      let tl = check_operand env l Types.TBool in
      let tr = check_operand env r Types.TBool in
      Tast.mk Types.TBool (Tast.TBinOp (op, tl, tr))

and synth_assign env base l r =
  let tl, tr = check_assign_operands env base l r in
  Tast.mk Types.TUnit (Tast.TAssign (base, tl, tr))

and check_comparison_operands env l r =
  let is_contextual_literal e =
    is_num_literal e || match e.desc with String _ -> true | _ -> false
  in
  coerce_common_pair env ~contextual:is_contextual_literal l r

and check_assign_operands env base l r =
  let tl = synth env l in
  if tl.ty <> Types.TError && not (is_lvalue tl) then
    add_error env l.span "cannot assign to expression";
  verify_param_copy_write env l.span tl;
  (match tl.desc with
  | Tast.TIdent s when Symbol.is_func s.Symbol.kind ->
      add_error env l.span "cannot assign to function"
  | Tast.TIdent _ | Tast.TFieldAccess _ | Tast.TIndex _ -> (
      (* This catches writes to any immutable binding *)
      match root_binding tl with
      | Some s when Symbol.is_immutable s.Symbol.kind || is_const_symbol env s
        ->
          add_error env l.span "cannot assign to immutable"
      | _ -> ())
  | _ -> ());
  let t = tl.ty in
  Option.iter (fun op -> verify_operands env l.span op t) base;
  let tr =
    match base with
    | Some (Lshift | Rshift) ->
        let tr = synth env r in
        verify_shift_count env r.span tr;
        tr
    | _ -> check env r t
  in
  (tl, tr)

and synth_pair_assign env ft st fv sv =
  let ft, fv = check_assign_operands env None ft fv in
  let st, sv = check_assign_operands env None st sv in
  Tast.mk Types.TUnit (Tast.TPairAssign (ft, st, fv, sv))

and synth_unop env op e =
  match op with
  | Pos | Neg | BitNot ->
      let te = synth_operand env e in
      let t = te.ty in
      if not (unop_accepts op t) then
        emit env
          (Diagnostic.bad_operand e.span ~op:(show_unop_sym op)
             ~ty:(show_ty env t));
      Tast.mk t (Tast.TUnOp (op, te))
  | Not ->
      let te = check env e Types.TBool in
      Tast.mk Types.TBool (Tast.TUnOp (op, te))
  | Deref -> (
      let te = synth env e in
      match resolve_ty te.ty with
      | Types.TPointer inner -> Tast.mk inner (Tast.TUnOp (op, te))
      | Types.TError -> dummy_texpr
      | Types.TOpaquePtr ->
          emit env (Diagnostic.opaque_operation e.span "dereference");
          dummy_texpr
      | t ->
          emit env
            (Diagnostic.with_type e.span "cannot dereference" (show_ty env t));
          dummy_texpr)
  | AddressOf ->
      let te = synth env e in
      (match te.desc with
      | Tast.TIdent s when is_const_symbol env s ->
          emit env
            (Diagnostic.error_at e.span "cannot take address of a constant"
            |> Diagnostic.help "a const has no storage, use var")
      | _ ->
          if te.ty <> Types.TError && not (is_lvalue te) then
            add_error env e.span "cannot take address of expression");
      Tast.mk (Types.TPointer te.ty) (Tast.TUnOp (op, te))

and synth_field env span e fname fspan =
  synth_typed_field env span (synth env e) fname fspan

and synth_value_path env p =
  let _, root_span = Nonempty.hd p.owner in
  match Resolve.sym_at_opt env.ctx.symbols root_span with
  | Some { Symbol.kind = Symbol.Module | Symbol.Type | Symbol.LocalType; _ }
  | None ->
      None
  | Some symbol ->
      let root =
        match symbol.Symbol.kind with
        | Symbol.Error -> stamp env root_span dummy_texpr
        | _ ->
            stamp env root_span
              (Tast.mk (lookup_var env root_span) (Tast.TIdent symbol))
      in
      let fields = List.tl (Nonempty.to_list p.owner) @ [ p.member ] in
      let step typed (field_name, field_span) =
        let span = Span.make (Span.lo root_span) (Span.hi field_span) in
        stamp env span (synth_typed_field env span typed field_name field_span)
      in
      Some (List.fold_left step root fields)

(* A type in call position converts its one argument *)
and synth_type_call env span (callee : expr) args =
  let sym = Resolve.sym_at env.ctx.symbols callee.span in
  let ty = named_ty env callee.span sym.Symbol.name in
  match args with
  | [ arg ] -> synth_conversion env span (synth_operand env arg) ty
  | _ ->
      emit env
        (Diagnostic.arity span ~expected:"expected 1 argument"
           ~found:(List.length args));
      List.iter (fun a -> ignore (synth env a)) args;
      Tast.mk ty Tast.TErrorExpr

and synth_call env span callee args =
  match direct_callee env callee with
  | Some fn_sym ->
      let fsig = lookup_func env callee.span in
      let targs = check_args env span fsig args in
      let fixed_count =
        if fsig.variadic then Some (List.length fsig.param_tys) else None
      in
      let callee_texpr =
        Tast.mk
          (Types.TFunc (fsig.param_tys, fsig.ret_ty, fsig.abi))
          (Tast.TIdent fn_sym)
      in
      Tast.mk fsig.ret_ty (Tast.TCall (callee_texpr, targs, fixed_count))
  | None when Symbol.Table.mem env.ctx.type_defs (key_at env callee.span) ->
      synth_type_call env span callee args
  | _ -> (
      (* The callee is a value holding a fn ptr so call through it *)
      let callee_texpr = synth env callee in
      match resolve_ty callee_texpr.ty with
      | Types.TError -> dummy_texpr
      | Types.TFunc (param_tys, ret_ty, abi) ->
          let fsig = { param_tys; ret_ty; variadic = false; abi } in
          let targs = check_args env span fsig args in
          Tast.mk ret_ty (Tast.TCall (callee_texpr, targs, None))
      | _ ->
          emit env
            (Diagnostic.error "not callable"
            |> Diagnostic.at callee.span
            |> Diagnostic.label
                 (Printf.sprintf "this has type %s"
                    (show_ty env callee_texpr.ty)));
          dummy_texpr)

and synth_index env span base idx =
  let tbase = synth env base in
  match resolve_ty tbase.ty with
  | Types.TArray (elem, _) | Types.TSlice elem -> (
      (* A missing bound spans the rest of the value *)
      let zero = Tast.mk (Types.TInt Usize) (Tast.TInt 0L) in
      let whole_length = Tast.mk (Types.TInt Usize) (Tast.TLen tbase) in
      let one_past (te : Tast.texpr) =
        Tast.mk te.ty (Tast.TBinOp (Ast.Add, te, Tast.mk te.ty (Tast.TInt 1L)))
      in
      let slice tlo thi =
        Tast.mk (Types.TSlice elem) (Tast.TSliceExpr (tbase, tlo, thi))
      in
      let bound e = check env e (Types.TInt Usize) in
      match idx.desc with
      (* A slice borrows into the same storage and an inclusive end just
         desugars to one past *)
      | Range (lo, hi) ->
          let tlo, thi, _ = check_range_bounds env lo hi in
          slice tlo thi
      | RangeInclusive (lo, hi) ->
          let tlo, thi, _ = check_range_bounds env lo hi in
          slice tlo (one_past thi)
      | RangeFrom lo -> slice (bound lo) whole_length
      | RangeTo hi -> slice zero (bound hi)
      | RangeToInclusive hi -> slice zero (one_past (bound hi))
      | RangeFull -> slice zero whole_length
      | _ ->
          let tidx = synth env idx in
          if not (is_integer tidx.ty) then
            add_error env idx.span "array index must be an integer";
          Tast.mk elem (Tast.TIndex (tbase, tidx)))
  | Types.TError -> dummy_texpr
  | Types.TOpaquePtr ->
      emit env (Diagnostic.opaque_operation span "index");
      dummy_texpr
  | t ->
      emit env (Diagnostic.with_type span "cannot index" (show_ty env t));
      dummy_texpr

(* An array size can want a const before that decl is checked so values
   resolve on demand *)
and const_of_symbol env s span =
  match s.Symbol.kind with
  | Symbol.Local Ast.Comptime ->
      Option.bind env.local_values (fun values ->
          Symbol.Table.find_opt values (Symbol.key s))
  | Symbol.Global Ast.Comptime
    when Symbol.Table.mem env.ctx.global_facts (Symbol.key s) ->
      Some (global_const_num env span (Symbol.key s))
  | _ -> None

and global_const_num env span key =
  match Symbol.Table.find_opt env.ctx.const_values key with
  | Some value -> value
  | None ->
      let fact = find_global_fact env span key in
      force_deferred span fact.folded ~on_error:dummy_value (fun () ->
          let te = global_typed_init env span key in
          let value = Option.value te.const ~default:dummy_value in
          Symbol.Table.replace env.ctx.const_values key value;
          value)

(* The init is what an unannotated global gets its type from *)
and global_ty env (gd : global_def) =
  match (gd.typ, gd.init) with
  | Some t, _ -> ty_of_ast env t
  | None, Some e ->
      let te =
        try global_typed_init env e.span (key_at env gd.span)
        with Diagnostic.Errors ds ->
          List.iter (emit env) ds;
          dummy_texpr
      in
      te.ty
  | None, None ->
      emit env (Diagnostic.cannot_infer gd.name_span);
      Types.TError

(* The running initializer state catches recursive demand *)
and global_typed_init env span key =
  let fact = find_global_fact env span key in
  force_deferred span fact.typed ~on_error:dummy_texpr (fun () ->
      let decl_env = decl_env_at env fact.declaration.span in
      type_global_init decl_env span fact.declaration)

and type_global_init env span = function
  | { init = Some e; typ = Some t; _ } -> check env e (ty_of_ast env t)
  | { init = Some e; typ = None; _ } -> synth env e
  | { init = None; _ } ->
      raise (Diagnostic.Errors [ Constant.unsupported_const span ])

(* The folded size fixes dropped suffixes and silent wraps on huge counts *)
and eval_array_size env e =
  let bad msg =
    add_error env e.span msg;
    0
  in
  let te = synth env e in
  if not (is_integer te.ty) then bad "array size must be an integer"
  else
    let v = const_value_or env dummy_value te in
    let n = Constant.int_of v in
    (* The message shows the folded value for expression sizes *)
    let shown =
      if is_unsigned te.ty then Printf.sprintf "%Lu" n else Int64.to_string n
    in
    if
      Int64.compare n 0x7FFF_FFFFL > 0
      || (Int64.compare n 0L < 0 && is_unsigned te.ty)
    then bad ("array size is too large: " ^ shown)
    else if Int64.compare n 0L < 0 then bad ("array size is negative: " ^ shown)
    else Int64.to_int n

(* The C runtime gives main an implicit i32 result *)
(* FIXME(80e8): This keeps main working before return inference *)
let ret_ty_of env fd =
  match fd.ret with
  | Some t -> return_ty_of_ast env t
  | None -> if is_entry env fd.func_span then Types.TInt I32 else Types.TUnit

(* The signature pass enables forward calls *)
let collect_func env fd =
  let abi = resolve_abi env fd.extern_abi in
  let param_tys = List.map (fun p -> ty_of_ast env p.param_typ) fd.params in
  let ret_ty = ret_ty_of env fd in
  Symbol.Table.replace env.ctx.func_sigs (key_at env fd.func_span)
    { param_tys; ret_ty; variadic = fd.variadic; abi }

(* A repeat name is already reported with both spans by the resolver *)
let type_name_taken ctx span = Symbol.Table.mem ctx.type_defs (key_in ctx span)

(* The reserved name enables forward field types *)
let reserve_struct_name ctx sd =
  if not (type_name_taken ctx sd.struct_span) then (
    let seen = Hashtbl.create 8 in
    List.iter
      (fun f ->
        if Hashtbl.mem seen f.field_name then
          add_error_in ctx f.field_span "duplicate field"
        else Hashtbl.add seen f.field_name ())
      sd.fields;
    Symbol.Table.replace ctx.type_defs
      (key_in ctx sd.struct_span)
      (Struct_type (ref Unstarted));
    Layout.set_struct_fields ctx.layouts (key_in ctx sd.struct_span) [])

let fill_struct_fields env sd =
  match Symbol.Table.find_opt env.ctx.type_defs (key_at env sd.struct_span) with
  | Some (Struct_type body) ->
      let named_ty f = (f.field_name, ty_of_ast env f.field_typ) in
      let field_tys = List.map named_ty sd.fields in
      let field_index =
        if List.compare_length_with field_tys 8 <= 0 then None
        else
          let index = Hashtbl.create (List.length field_tys) in
          List.iteri
            (fun field_id (name, ty) ->
              if not (Hashtbl.mem index name) then
                Hashtbl.add index name (field_id, ty))
            field_tys;
          Some index
      in
      body := Completed { field_tys; field_index };
      Layout.set_struct_fields env.ctx.layouts
        (key_at env sd.struct_span)
        (List.map snd field_tys)
  | _ -> ()

type visit = On_path | Finished

(* A back edge closes a cycle so everything still on the path is in it *)
let cyclic_structs defs iter_edges =
  let state = Symbol.Table.create (Symbol.Table.length defs) in
  let cyclic = Symbol.Table.create 8 in
  let path = ref [] in
  let rec visit key =
    match Symbol.Table.find_opt state key with
    | Some Finished -> ()
    | Some On_path ->
        let rec mark = function
          | [] -> ()
          | node :: rest ->
              Symbol.Table.replace cyclic node ();
              if node <> key then mark rest
        in
        mark !path
    | None ->
        Symbol.Table.replace state key On_path;
        path := key :: !path;
        iter_edges key visit;
        path := List.tl !path;
        Symbol.Table.replace state key Finished
  in
  Symbol.Table.iter (fun key _ -> visit key) defs;
  cyclic

(* A pointer or slice field is just an address so it can't grow the struct *)
let verify_type_cycles ctx =
  let rec stored_struct ty =
    match resolve_ty ty with
    | Types.TStruct (name, _) -> Some (Qname.key name)
    | Types.TArray (element, _) -> stored_struct element
    | _ -> None
  in
  let edges = Symbol.Table.create 16 in
  let record_edges = function
    | Struct sd -> (
        let key = key_in ctx sd.struct_span in
        let fields = Iarray.to_list (Layout.struct_fields ctx.layouts key) in
        match List.filter_map stored_struct fields with
        | [] -> ()
        | targets -> Symbol.Table.replace edges key targets)
    | Func _ | Extern _ | Global _ | TypeAlias _ | Enum _ -> ()
  in
  let iter_edges key visit =
    Option.iter (List.iter visit) (Symbol.Table.find_opt edges key)
  in
  (* A struct can name one written later so all edges land before the walk *)
  List.iter record_edges ctx.declarations;
  let cyclic = cyclic_structs edges iter_edges in
  let report_cycle = function
    | Struct sd when Symbol.Table.mem cyclic (key_in ctx sd.struct_span) ->
        Diagnostic.emit ctx.diags
          (Diagnostic.error_at sd.struct_name_span
             "recursive struct has infinite size")
    | Struct _ | Func _ | Extern _ | Global _ | TypeAlias _ | Enum _ -> ()
  in
  (* The report follows the file so the same struct is blamed every time *)
  List.iter report_cycle ctx.declarations

(* A variant names no type so one pass settles the whole enum *)
let reserve_enum_name ctx ed =
  if not (type_name_taken ctx ed.enum_span) then begin
    let variants_by_name = Hashtbl.create (List.length ed.variants) in
    let add next v =
      if Hashtbl.mem variants_by_name v.variant_name then begin
        add_error_in ctx v.variant_span "duplicate variant";
        next
      end
      else begin
        Hashtbl.add variants_by_name v.variant_name next;
        Int64.succ next
      end
    in
    ignore (List.fold_left add 0L ed.variants);
    Symbol.Table.replace ctx.type_defs (key_in ctx ed.enum_span)
      (Enum_type (ref (Completed { variants_by_name })))
  end

let reserve_alias_name ctx td =
  if not (type_name_taken ctx td.alias_span) then
    Symbol.Table.replace ctx.type_defs (key_in ctx td.alias_span)
      (Alias_type (ref Unstarted))

let rec named_type_spans t =
  match t.tdesc with
  | Named _ -> [ t.tspan ]
  | ErrorType -> []
  | Pointer inner | Slice inner | Array (_, inner) -> named_type_spans inner
  | FuncPtr (_, params, ret) ->
      List.concat_map named_type_spans params
      @ Option.value ~default:[] (Option.map named_type_spans ret)
  | UnitType -> []

(* An alias is only a second name for what it points at. A pointer in the
   middle doesn't save it the way it saves a struct field *)
let resolve_type_bodies ctx =
  let defs = Hashtbl.create 16 in
  let remember decl =
    match decl with
    | TypeAlias td ->
        (* The resolver already rejected later definitions *)
        (* The unresolved key cannot identify a real dependency *)
        let key = key_in ctx td.alias_span in
        if key <> Symbol.unresolved_key && not (Hashtbl.mem defs key) then
          Hashtbl.add defs key decl
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ()
  in
  let rec resolve_alias key =
    match Hashtbl.find_opt defs key with
    | None -> ()
    | Some (TypeAlias td as decl) -> (
        match Symbol.Table.find_opt ctx.type_defs key with
        | Some (Alias_type body) -> (
            match !body with
            | Completed _ -> ()
            | Running -> add_error_in ctx td.alias_name_span "recursive type"
            | Unstarted ->
                body := Running;
                List.iter
                  (fun span -> resolve_alias (key_in ctx span))
                  (named_type_spans td.alias_typ);
                let env = env_for_decl ctx decl in
                body := Completed (ty_of_ast env td.alias_typ))
        | Some (Struct_type _ | Builtin_type _ | Enum_type _) | None -> ())
    | Some (Func _ | Extern _ | Global _ | Struct _ | Enum _) -> ()
  in
  List.iter remember ctx.declarations;
  (* The order here follows the file so the same type gets blamed every time *)
  let resolve = function
    | TypeAlias td -> resolve_alias (key_in ctx td.alias_span)
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ()
  in
  List.iter resolve ctx.declarations

let collect_global env (gd : global_def) =
  (if gd.init = None then
     match gd.kind with
     | Var -> ()
     | Comptime ->
         emit env
           (Diagnostic.error_at gd.name_span "comptime without initializer"));
  let key = key_at env gd.span in
  let t =
    match Symbol.Table.find_opt env.ctx.global_facts key with
    | Some fact -> pending_global_ty env gd.span key fact
    | None -> global_ty env gd
  in
  Symbol.Table.replace env.ctx.globals key (t, gd.kind)

(* Every type name lands first so a signature can name a type written later *)
let reserve_type_names ctx =
  let reserve = function
    | Struct sd -> reserve_struct_name ctx sd
    | TypeAlias td -> reserve_alias_name ctx td
    | Enum ed -> reserve_enum_name ctx ed
    | Func _ | Extern _ | Global _ -> ()
  in
  List.iter reserve ctx.declarations

let collect_decls ctx =
  let collect decl =
    match decl with
    | Func fd | Extern fd ->
        let env = env_for_decl ctx decl in
        collect_func env fd
    | Global gd ->
        let env = env_for_decl ctx decl in
        collect_global env gd
    | Struct _ | TypeAlias _ | Enum _ -> ()
  in
  List.iter collect ctx.declarations

let fill_struct_layouts ctx =
  let fill decl =
    match decl with
    | Struct sd ->
        let env = env_for_decl ctx decl in
        fill_struct_fields env sd
    | Func _ | Extern _ | Global _ | TypeAlias _ | Enum _ -> ()
  in
  List.iter fill ctx.declarations

let check_func ?(is_extern = false) env fd =
  let key = key_at env fd.func_span in
  let is_entry_point = is_entry env fd.func_span in
  (* The collected signature is reused so a bad array size errors once *)
  let collected = Symbol.Table.find_opt env.ctx.func_sigs key in
  let param_tys =
    match collected with
    | Some s when List.compare_lengths s.param_tys fd.params = 0 -> s.param_tys
    | _ -> List.map (fun p -> ty_of_ast env p.param_typ) fd.params
  in
  let params_typed =
    List.map2 (fun p t -> (p.param_name, t, p.param_span)) fd.params param_tys
  in
  let params = List.map (fun (_, t, span) -> (sym env span, t)) params_typed in

  let ret_ty =
    match collected with Some s -> s.ret_ty | None -> ret_ty_of env fd
  in

  (* The C entry point must return a 32 bit integer *)
  let invalid_entry_return =
    is_entry_point && ret_ty <> Types.TError && ret_ty <> Types.TInt I32
  in
  if invalid_entry_return then begin
    let span = match fd.ret with Some t -> t.tspan | None -> fd.func_span in
    emit env
      (Diagnostic.type_mismatch span
         ~expected:(show_ty env (Types.TInt I32))
         ~found:(show_ty env ret_ty))
  end;

  let func_env =
    push_scope { env with ret_ty; entry_function = is_entry_point }
  in
  (* An extern has no body so its params can't be used and stay quiet *)
  let param_env =
    List.fold_left
      (fun e (name, t, span) ->
        extend_var ~used:is_extern ~deduplicate:true e span name t)
      func_env params_typed
  in

  (* An unwritten i32 on main comes from the runtime and not from the user *)
  let implicit_return =
    (not is_extern) && ret_ty <> Types.TUnit
    && ((not is_entry_point) || fd.ret <> None)
  in
  let body_use =
    if not implicit_return then Discard
    else if is_entry_point then
      (* The C entry point can fall through with zero *)
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
    | true, last :: rest when last.ty = ret_ty && ret_ty <> Types.TNever ->
        let ret =
          Tast.mk ~span:last.span Types.TNever (Tast.TReturn (Some last))
        in
        List.rev (ret :: rest)
    | _ -> tbody0
  in

  {
    key;
    name = link_name_at env fd.func_span (Interner.text fd.func_name);
    (* A qualified source name disambiguates panic reports *)
    source_name =
      String.concat "." (env.reader_path @ [ Interner.text fd.func_name ]);
    entry_point = is_entry_point;
    params;
    ret_ty;
    body = tbody;
    modifiers = fd.func_modifiers;
    variadic = fd.variadic;
  }

let rec is_const_texpr env (te : Tast.texpr) =
  match te.desc with
  | Tast.TErrorExpr -> true
  | Tast.TInt _ | Tast.TFloat _ | Tast.TBool _ | Tast.TNull | Tast.TChar _
  | Tast.TCStr _ | Tast.TStr _ | Tast.TSizeOf _ | Tast.TVariant _ ->
      true
  | Tast.TIdent s ->
      Symbol.is_func s.Symbol.kind || is_comptime_global env (Symbol.key s)
  | Tast.TUnOp (Ast.AddressOf, { desc = Tast.TIdent s; _ }) ->
      Symbol.is_global s.Symbol.kind
  | Tast.TUnOp (_, e) -> is_const_texpr env e
  | Tast.TBinOp (_, l, r) -> is_const_texpr env l && is_const_texpr env r
  | Tast.TCast e -> is_const_texpr env e
  | Tast.TZero -> true
  | Tast.TArrayLit elems -> List.for_all (is_const_texpr env) elems
  | Tast.TStructLit (_, fields) ->
      List.for_all (fun (_, fe) -> is_const_texpr env fe) fields
  | Tast.TUndef -> true
  | Tast.TUnit -> true
  | _ -> false

let check_global env (gd : global_def) =
  let key = key_at env gd.span in
  (* The collected type is reused so a bad array size errors once *)
  let t =
    match Symbol.Table.find_opt env.ctx.globals key with
    | Some (t, _) -> t
    | None -> global_ty env gd
  in
  if gd.kind = Comptime then verify_const_scalar env gd.span t;
  let tinit =
    match gd.init with
    | None -> None
    | Some { desc = Undefined; span } when gd.kind = Comptime ->
        emit env
          Diagnostic.(
            error "comptime cannot be undefined"
            |> at span
            |> help "use var for values that need storage");
        None
    | Some { desc = Undefined; _ } -> None
    | Some e ->
        let te =
          if Symbol.Table.mem env.ctx.global_facts key then
            global_typed_init env e.span key
          else check env e t
        in
        if not (is_const_texpr env te) then (
          add_error env e.span "initializer must be constant";
          None)
        else Some te
  in
  {
    key;
    name = link_name_at env gd.span (Interner.text gd.name);
    ty = t;
    init = tinit;
    kind = gd.kind;
    modifiers = gd.modifiers;
  }

let typed_struct_decl ctx sd fields =
  let name = qname_in ctx sd.struct_span (Interner.text sd.struct_name) in
  let is_local =
    Option.exists
      (fun symbol -> symbol.Symbol.kind = Symbol.LocalType)
      (Resolve.sym_at_opt ctx.symbols sd.struct_span)
  in
  if is_local then Tast.TLocalStruct (name, fields)
  else Tast.TStruct (name, fields, sd.struct_modifiers)

let check_decls ctx =
  let check_declaration decl =
    match decl with
    | Func fd ->
        let env = env_for_decl ctx decl in
        let tfd = check_func env fd in
        Tast.TFunc tfd
    | Extern fd ->
        let env = env_for_decl ctx decl in
        let tfd = check_func ~is_extern:true env fd in
        Tast.TExtern tfd
    | Struct sd ->
        (* A rejected duplicate has no completed table entry *)
        let field_tys =
          match
            Symbol.Table.find_opt ctx.type_defs (key_in ctx sd.struct_span)
          with
          | Some (Struct_type { contents = Completed info }) ->
              List.map snd info.field_tys
          | _ ->
              let env = env_for_decl ctx decl in
              List.map (fun f -> ty_of_ast env f.field_typ) sd.fields
        in
        typed_struct_decl ctx sd field_tys
    | Global gd ->
        let env = env_for_decl ctx decl in
        Tast.TGlobal (check_global env gd)
    | TypeAlias td ->
        let t =
          match
            Symbol.Table.find_opt ctx.type_defs (key_in ctx td.alias_span)
          with
          | Some (Alias_type { contents = Completed t }) -> t
          | _ ->
              let env = env_for_decl ctx decl in
              ty_of_ast env td.alias_typ
        in
        Tast.TTypeAlias
          (qname_in ctx td.alias_span (Interner.text td.alias_name), t)
    | Enum ed ->
        Tast.TEnum (qname_in ctx ed.enum_span (Interner.text ed.enum_name))
  in
  List.map check_declaration ctx.declarations

(* An early array size can demand any later const so defs go in first *)
let register_globals ctx =
  let register = function
    | Global gd ->
        Symbol.Table.replace ctx.global_facts (key_in ctx gd.span)
          {
            declaration = gd;
            declared_ty = ref Unstarted;
            typed = ref Unstarted;
            folded = ref Unstarted;
          }
    | Func _ | Extern _ | Struct _ | TypeAlias _ | Enum _ -> ()
  in
  List.iter register ctx.declarations

(* The partial tree stays available so later checks can still run *)
let analyze ~diags uses decls =
  let decls = decls @ Resolve.local_decls uses in
  let ctx = make_ctx diags uses decls in
  register_globals ctx;
  reserve_type_names ctx;
  resolve_type_bodies ctx;
  collect_decls ctx;
  fill_struct_layouts ctx;
  verify_type_cycles ctx;
  check_decls ctx
