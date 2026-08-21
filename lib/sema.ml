(* SPDX-License-Identifier: Apache-2.0 *)

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
type enum_info = { variant_vals : (Ast.name * int64) list }

(* Structs aliases and builtins share one namespace of type names *)
type type_def =
  | DStruct of struct_info
  | DAlias of ty
  | DBuiltin of Types.builtin
  | DEnum of enum_info

type result_use = Infer | Expect of ty | Discard
type var_info = { name : Ast.name; ty : ty; used : bool ref; span : Ast.span }

(* The typed and value fields only ever go from None to Some so nothing rolls back *)
(* TODO: I should model the cache states explicitly without slowing global heavy code *)
type gstate = {
  def : global_def;
  mutable typed : T.texpr option;
  mutable value : Constant.value option;
  (* Busy means this global is mid evaluation so a self demand is a cycle *)
  mutable busy : bool;
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

type env = {
  vars : (Symbol.key * var_info) list list;
  funcs : func_sig Symbol.Table.t;
  types : type_def Symbol.Table.t;
  (* Struct field layouts mirror the DStruct entries in types so ty_size need not rebuild them *)
  struct_fields : ty list Symbol.Table.t;
  globals : (ty * Ast.binding_kind) Symbol.Table.t;
  (* Constants evaluate on demand so an array size may name a later const *)
  g_state : gstate Symbol.Table.t;
  l_vals : Constant.value Symbol.Table.t;
  ret_ty : ty;
  loops : loop_ctx list;
  in_main : bool;
  suppress_warnings : bool;
  (* Whoever reads the message is inside this module so its path drops out *)
  reader_path : string list;
  diags : Diagnostic.sink;
  uses : Resolve.t;
}

type coercion_input = Contextual of expr | Typed of expr * T.texpr

let make_env (diags : Diagnostic.sink) (uses : Resolve.t) =
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
    ret_ty = TUnit;
    loops = [];
    in_main = false;
    suppress_warnings = Diagnostic.has_errors diags;
    reader_path = [];
    diags;
    uses;
  }

let show_ty (env : env) (t : ty) = Types.show_ty_in env.reader_path t

let decl_span = function
  | Func fd | Extern fd -> fd.func_span
  | Global gd -> gd.span
  | Struct sd -> sd.struct_span
  | TypeAlias td -> td.alias_span
  | Enum ed -> ed.enum_span

(* The path comes off the declaration being checked not off the root module *)
let reading (env : env) (decl : decl) =
  { env with reader_path = Resolve.module_path_at env.uses (decl_span decl) }

(* The two fields every slice and string answers to, interned once *)
let len_name = Interner.intern "len"
let ptr_name = Interner.intern "ptr"
let dummy_value = Constant.VInt (Constant.zero, Types.I32)
let sym (env : env) (span : Ast.span) = Resolve.sym_at env.uses span
let emit (env : env) (d : Diagnostic.t) = Diagnostic.emit env.diags d
let add_error (env : env) span msg = Diagnostic.emit_error_at env.diags span msg
let dummy_texpr = T.mk TError T.TErrorExpr

let add_warning (env : env) (span : Ast.span) (msg : string) =
  if not env.suppress_warnings then Diagnostic.emit_warn_at env.diags span msg

(* An unsigned literal past i64 max is stored as a negative bit pattern *)
let unsigned_to_float (n : int64) =
  let two_pow_64 = 18446744073709551616.0 in
  if Int64.compare n 0L >= 0 then Int64.to_float n
  else Int64.to_float n +. two_pow_64

let round_to_float_kind (kind : float_kind) (f : float) =
  match kind with
  | F32 -> Int32.float_of_bits (Int32.bits_of_float f)
  | F64 -> f

let push_scope (env : env) = { env with vars = [] :: env.vars }

let clone_loop (loop : loop_ctx) =
  {
    lbl = loop.lbl;
    valued = loop.valued;
    result = loop.result;
    bare_break = loop.bare_break;
  }

let probing (env : env) =
  { env with loops = List.map clone_loop env.loops; diags = Diagnostic.sink () }

let warn_unused_in_scope (env : env) =
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
    (span : Ast.span) (name : Ast.name) (t : ty) =
  let key = Symbol.key (sym env span) in
  let info = { name; ty = t; used = ref used; span } in
  match env.vars with
  | [] -> assert false (* No active scope *)
  | scope :: _ when deduplicate && List.mem_assoc key scope -> env
  | scope :: rest -> { env with vars = ((key, info) :: scope) :: rest }

let lookup_var_opt (env : env) (span : Ast.span) =
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
let unresolved_key = Symbol.make_key (-1) (-1)

(* Two declarations can go by one name so lookups key on which one it is *)
let key_at (env : env) (span : Ast.span) =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> Symbol.key symbol
  | None -> unresolved_key

let builtin_at (env : env) (span : Ast.span) =
  match Symbol.Table.find_opt env.types (key_at env span) with
  | Some (DBuiltin b) -> Some b
  | Some (DStruct _ | DAlias _ | DEnum _) | None -> None

(* The path comes off the symbol so a message can say which module a type is from *)
let qname_at (env : env) (span : Ast.span) (fallback : string) =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> Resolve.qname_of env.uses symbol
  | None -> Qname.unresolved fallback

(* What the linker calls this declaration was worked out once by the resolver *)
let link_name_at (env : env) (span : Ast.span) (fallback : string) =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> symbol.Symbol.link_name
  | None -> fallback

let is_entry (env : env) (span : Ast.span) =
  match Resolve.sym_at_opt env.uses span with
  | Some symbol -> symbol.Symbol.entry_point
  | None -> false

let lookup_func (env : env) (span : Ast.span) =
  match Symbol.Table.find_opt env.funcs (key_at env span) with
  | Some s -> s
  | None ->
      emit env (Diagnostic.undefined_name span "function");
      { param_tys = []; ret_ty = TUnit; variadic = false; abi = Types.Ripe }

let is_comptime_global (env : env) (key : Symbol.key) =
  match Symbol.Table.find_opt env.globals key with
  | Some (_, Comptime) -> true
  | _ -> false

(* TODO: An aggregate global has no constant form now that let is gone so nothing can initialize one from another global *)
let verify_const_scalar (env : env) (span : Ast.span) (t : ty) =
  if not (is_scalar t) then
    emit env
      (Diagnostic.with_type span "comptime must be a scalar" (show_ty env t)
      |> Diagnostic.help "use var for values that need storage")

let lookup_struct (env : env) (span : Ast.span) (name : Qname.t) =
  match Symbol.Table.find_opt env.types (Qname.key name) with
  | Some (DStruct s) -> s
  | _ ->
      emit env (Diagnostic.undefined_name span "struct");
      { field_tys = [] }

let lift_ty (f : ty -> ty) (ty : ty) =
  match ty with TError -> TError | ty -> f ty

(* A signature without an ABI written on it is a plain Ripe function *)
let resolve_abi (env : env) (a : Ast.abi) =
  match a with
  | NoAbi -> Types.Ripe
  | AbiError -> Types.AbiError
  | NamedAbi (name, span) -> (
      match Types.func_abi_of_name name with
      | Some abi -> abi
      | None ->
          emit env (Diagnostic.unsupported_abi span);
          Types.AbiError)

let named_ty (env : env) (span : Ast.span) (shown : string) =
  match Symbol.Table.find_opt env.types (key_at env span) with
  | Some (DBuiltin (BTy TNever)) ->
      emit env
        Diagnostic.(
          error "never is only valid as a function return type"
          |> at span
          |> help "a value of type never cannot exist");
      TError
  | Some (DBuiltin BOpaque) ->
      emit env
        Diagnostic.(
          error "opaque is only valid as a pointee"
          |> at span
          |> help "use *opaque for an untyped pointer");
      TError
  | Some (DBuiltin (BTy ty)) -> ty
  | Some (DStruct _) -> TStruct (qname_at env span shown, [])
  | Some (DAlias aliased) -> TAlias (qname_at env span shown, aliased)
  | Some (DEnum _) -> TEnum (qname_at env span shown)
  | None -> (
      match Resolve.sym_at_opt env.uses span with
      | Some { Symbol.kind = Symbol.Error; _ } -> TError
      | _ -> Diagnostic.ice ~span "type name escaped the resolver")

(* Only the biggest folded subtree reports so a wide intermediate stays legal *)
let rec report_const_range (env : env) (te : T.texpr) =
  match (te.T.const, resolve_ty te.T.ty, te.T.desc) with
  | Some _, _, T.TSizeOf _ -> ()
  | Some v, TInt kind, _ ->
      if not (Constant.representable kind (Constant.exact_of v)) then
        emit env
          (Diagnostic.int_out_of_range te.T.span
             ~ty:(show_ty env (resolve_ty te.T.ty)))
  | _, _, (T.TCast operand | T.TUnOp (_, operand)) ->
      report_const_range env operand
  | _, _, T.TBinOp (_, l, r) ->
      report_const_range env l;
      report_const_range env r
  | _ -> ()

(* The value of a block is its last element and unit when the block is empty *)
let tblock_ty (tb : T.tblock) =
  match List.rev tb with te :: _ -> te.T.ty | [] -> TUnit

let if_result_ty (tbranches : (T.texpr * T.tblock) list)
    (telse : T.tblock option) ~(fallback_ty : ty) =
  let diverges tb = tblock_ty tb = TNever in
  match telse with
  | Some tb
    when diverges tb && List.for_all (fun (_, tb) -> diverges tb) tbranches ->
      TNever
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

and block_is_flexible (body : block) =
  match List.rev body with
  | Expr last :: _ -> arm_is_flexible last
  | Decl _ :: _ | [] -> false

let common_ty (current : ty) (candidate : ty) =
  match (current, candidate) with
  | TNever, candidate -> candidate
  | current, TNever -> current
  | TNull, candidate -> candidate
  | current, TNull -> current
  | current, candidate ->
      Option.value (common_numeric_ty current candidate) ~default:current

let is_unused_operation (e : expr) =
  match e.desc with BinOp _ | UnOp _ | BitCast _ -> true | _ -> false

let warn_discarded_operation (env : env) (e : expr) (te : T.texpr) =
  if
    (not env.suppress_warnings)
    && (not (Diagnostic.has_errors env.diags))
    && is_unused_operation e && te.T.ty <> TUnit && te.T.ty <> TNever
    && te.T.ty <> TError
  then
    emit env
      (Diagnostic.warning "discarded operation result"
      |> Diagnostic.at te.T.span
      |> Diagnostic.help "use `var _ = ...` when this is intentional")

let verify_unit_result (env : env) (span : Ast.span) = function
  | Expect want when resolve_ty want <> TUnit ->
      emit env
        (Diagnostic.type_mismatch span ~expected:(show_ty env want)
           ~found:(show_ty env TUnit))
  | Infer | Discard | Expect _ -> ()

let block_item_span = function
  | Expr e -> e.span
  | Decl d -> decl_span (decl_of_local d)

let new_loop (label : Ast.loop_label option) ~(valued : bool) =
  {
    lbl = Option.map (fun (l : Ast.loop_label) -> l.Ast.value) label;
    valued;
    result = InferLoopResult;
    bare_break = None;
  }

let find_loop (env : env) (label : Ast.loop_label option) =
  match label with
  | None -> ( match env.loops with lc :: _ -> Some lc | [] -> None)
  | Some l -> List.find_opt (fun lc -> lc.lbl = Some l.Ast.value) env.loops

let find_loop_or_error (env : env) (span : Ast.span) (headline : string)
    (label : Ast.loop_label option) =
  let found = find_loop env label in
  (match (found, label) with
  | None, None -> add_error env span headline
  | None, Some l -> emit env (Diagnostic.undefined_name l.Ast.span "loop label")
  | Some _, _ -> ());
  found

let verify_bare_break (env : env) (span : Ast.span) (lc : loop_ctx) =
  if lc.bare_break = None then lc.bare_break <- Some span;
  match lc.result with
  | InferLoopResult | ExpectLoopResult _ -> ()
  | FlexibleLoopResult (t, first, _) | RigidLoopResult (t, first, _) ->
      emit env
        (Diagnostic.break_disagree span "no value here" ~other:first
           ~other_message:(Printf.sprintf "breaks with %s" (show_ty env t)))

(* A slice is an address and a length so the array already holds what the view needs *)
let adopt_slice (want : ty) (te : T.texpr) =
  match (resolve_ty want, resolve_ty te.T.ty) with
  | TSlice _, TArray _ ->
      let zero = T.mk (TInt Usize) (T.TInt 0L) in
      let len = T.mk (TInt Usize) (T.TLen te) in
      T.mk want (T.TSliceExpr (te, zero, len))
  | _ -> te

(* The count keeps its own type since it is only a number of positions *)
let verify_shift_count (env : env) (span : Ast.span) (tr : T.texpr) =
  if not (is_integer tr.T.ty) then
    emit env
      (Diagnostic.with_found span "shift count must be an integer"
         (show_ty env tr.T.ty))

let no_such_field (env : env) (span : Ast.span) (ty : ty) =
  emit env (Diagnostic.with_type span "no field" (show_ty env ty));
  dummy_texpr

let verify_operands (env : env) (span : Ast.span) (op : binop) (t : ty) =
  if not (binop_accepts op t) then
    emit env
      (Diagnostic.bad_operand span ~op:(show_binop_sym op) ~ty:(show_ty env t))

(* An array or struct parameter arrives as a copy so the caller never sees the write *)
let verify_param_copy_write (env : env) (span : Ast.span) (tl : T.texpr) =
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

let global_state (env : env) (span : Ast.span) (key : Symbol.key) =
  match Symbol.Table.find_opt env.g_state key with
  | Some st -> st
  | None -> raise (Diagnostic.Errors [ Constant.unsupported_const span ])

(* A failed fold reports and hands back a dummy so checking continues *)
let const_value_or (env : env) (default : Constant.value) (te : T.texpr) =
  if not (is_scalar te.T.ty && te.T.ty <> TError) then default
  else
    match te.T.const with
    | Some v -> v
    | None ->
        emit env (Constant.unsupported_const te.T.span);
        default

let adopt_int_literal (env : env) (span : Ast.span) (want : ty) (target : ty)
    ~(neg : bool) (n : int64) =
  let signed = if neg then Int64.neg n else n in
  match target with
  | TInt kind ->
      let exact = Constant.of_magnitude ~neg n in
      Some
        {
          (T.mk want (T.TInt signed)) with
          T.const = Some (Constant.VInt (exact, kind));
        }
  | TFloat kind ->
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
      Some (T.mk want (T.TFloat exact))
  | TError -> Some (T.mk want (T.TInt signed))
  | _ -> None

(* A variant is a compile time constant so nothing of the enum survives here *)
let synth_variant (env : env) (inner : expr) (info : enum_info)
    (fname : Ast.name) (fspan : Ast.span) =
  let shown = Interner.text fname in
  let name = qname_at env inner.span shown in
  match List.assoc_opt fname info.variant_vals with
  | Some value -> T.mk (TEnum name) (T.TVariant (name, value))
  | None ->
      emit env
        (Diagnostic.error_at fspan "no variant"
        |> Diagnostic.label
             (Printf.sprintf "on enum %s" (show_ty env (TEnum name))));
      dummy_texpr

let synth_struct_field (env : env) (span : Ast.span) (te : T.texpr) (ty : ty)
    (fname : Ast.name) (fspan : Ast.span) =
  let rec peel depth = function
    | TStruct (sname, _) -> Some (sname, depth)
    | TAlias (_, base) -> peel depth base
    | TPointer t -> peel (depth + 1) t
    | _ -> None
  in
  match peel 0 ty with
  | None when resolve_ty ty = TError -> dummy_texpr
  | None ->
      emit env (Diagnostic.with_type span "type has no fields" (show_ty env ty));
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

let synth_conversion (env : env) (span : Ast.span) (te : T.texpr) (ty : ty) =
  if not (cast_ok te.T.ty ty) then begin
    let d =
      Diagnostic.error "invalid conversion"
      |> Diagnostic.at span
      |> Diagnostic.label
           (Printf.sprintf "cannot convert %s to %s" (show_ty env te.T.ty)
              (show_ty env ty))
    in
    let d =
      if resolve_ty ty = TBool then
        Diagnostic.help "compare with zero instead e.g. `x != 0`" d
      else d
    in
    emit env d
  end
  else if te.T.ty = ty then
    emit env
      (Diagnostic.warning "cast has no effect"
      |> Diagnostic.at span
      |> Diagnostic.label (Printf.sprintf "already %s" (show_ty env ty))
      |> Diagnostic.help "remove the cast");
  if te.T.ty = TError || ty = TError then dummy_texpr else T.mk ty (T.TCast te)

let rec ty_of_ast (env : env) (t : typ) =
  match t.tdesc with
  | ErrorType -> TError
  | Named (path, name) -> named_ty env t.tspan (Ast.show_named path name)
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
        match ret with Some t -> return_ty_of_ast env t | None -> TUnit
      in
      match (resolve_abi env abi, rt, List.mem TError pts) with
      | Types.AbiError, _, _ -> TError
      | abi, rt, false when rt <> TError -> TFunc (pts, rt, abi)
      | _ -> TError)
  | UnitType -> TUnit

and return_ty_of_ast (env : env) (t : typ) =
  match builtin_at env t.tspan with
  | Some (BTy TNever) -> TNever
  | Some (BTy _) | Some BOpaque | None -> ty_of_ast env t

and lookup_var (env : env) (span : Ast.span) =
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
          | Some st when st.busy ->
              emit env (Diagnostic.error_at span "cyclic constant");
              TError
          | Some st ->
              st.busy <- true;
              let t = global_ty env st.def in
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
and synth (env : env) (e : expr) =
  let typed = synth_operand env e in
  report_const_range env typed;
  typed

(* An operand folds into its parent so the parent reports the range *)
and synth_operand (env : env) (e : expr) = stamp env e.span (synth_desc env e)

and stamp (env : env) (span : Ast.span) (te : T.texpr) =
  if Option.is_none te.T.const then
    { te with T.span; T.const = const_of env ~span te }
  else { te with T.span }

(* A node takes its value from children that already carry theirs so no
   expression gets walked twice *)
and const_of (env : env) ~(span : Ast.span) (te : T.texpr) =
  match te.T.desc with
  | T.TInt n -> Some (Constant.of_literal te.T.ty n)
  | T.TBool b -> Some (Constant.VBool b)
  | T.TChar cp -> Some (Constant.VChar cp)
  | T.TFloat f -> Some (Constant.of_float (float_kind_of te.T.ty) f)
  | T.TSizeOf t ->
      Some
        (Constant.of_literal te.T.ty
           (Int64.of_int (ty_size env.struct_fields t)))
  | T.TIdent s -> (
      match resolve_ty te.T.ty with
      | TInt _ | TFloat _ | TBool | TChar -> const_of_symbol env s span
      | _ -> None)
  | T.TCast operand -> Option.map (Constant.cast te.T.ty) operand.T.const
  | T.TUnOp (op, operand) -> (
      try Option.bind operand.T.const (Constant.unop span op ~result_ty:te.T.ty)
      with Diagnostic.Errors ds ->
        List.iter (emit env) ds;
        None)
  | T.TBinOp (op, l, r) -> (
      match (l.T.const, r.T.const) with
      | Some a, Some b -> (
          try Constant.binop span op ~result_ty:te.T.ty a b
          with Diagnostic.Errors ds ->
            List.iter (emit env) ds;
            None)
      | None, _ | _, None -> None)
  | _ -> None

and synth_desc (env : env) (e : expr) =
  match e.desc with
  | ErrorExpr -> dummy_texpr
  | Int (n, suf) ->
      let kind = match suf with Some s -> suffix_kind s | None -> I32 in
      T.mk (TInt kind) (T.TInt n)
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      synth_desc env { operand with span = e.span }
  | UnOp (Neg, { desc = Int (n, Some s); _ }) ->
      let kind = suffix_kind s in
      T.mk (TInt kind) (T.TInt (Int64.neg n))
  | Float (f, suf) ->
      let kind = match suf with Some s -> float_suffix_kind s | None -> F64 in
      T.mk (TFloat kind) (T.TFloat f)
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
  | Assign (base, l, r) -> synth_assign env base l r
  | UnOp (op, e) -> synth_unop env op e
  | Path p -> (
      let inner_e = Ast.owner_expr p in
      let fname, fspan = p.member in
      match Resolve.sym_at_opt env.uses e.span with
      | Some s when s.Symbol.kind = Symbol.Error -> dummy_texpr
      | Some s
        when Symbol.is_func s.Symbol.kind || Symbol.is_global s.Symbol.kind ->
          T.mk (lookup_var env e.span) (T.TIdent s)
      | _ -> (
          match Symbol.Table.find_opt env.types (key_at env inner_e.span) with
          | Some (DEnum info) -> synth_variant env inner_e info fname fspan
          | Some (DStruct _ | DAlias _ | DBuiltin _) ->
              emit env
                (Diagnostic.error_at inner_e.span "expected a value"
                |> Diagnostic.label "this names a type");
              dummy_texpr
          | None -> synth_field env e.span inner_e fname fspan))
  | FieldAccess (inner_e, fname, fspan) ->
      synth_field env e.span inner_e fname fspan
  | BitCast (operand, t) ->
      let te = synth_operand env operand in
      let ty = ty_of_ast env t in
      if te.T.ty <> TError && ty <> TError && not (bitcast_ok te.T.ty ty) then
        emit env
          (Diagnostic.error "invalid bitcast"
          |> Diagnostic.at e.span
          |> Diagnostic.label
               (Printf.sprintf "cannot reinterpret %s as %s"
                  (show_ty env te.T.ty) (show_ty env ty))
          |> Diagnostic.help
               "both sides need the same width and neither may be a float");
      if te.T.ty = TError || ty = TError then dummy_texpr
      else T.mk ty (T.TCast te)
  | SizeOf t -> (
      match ty_of_ast env t with
      | TError -> dummy_texpr
      | ty -> T.mk (TInt Usize) (T.TSizeOf ty))
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
  | StructLit (path, name, name_span, inits) -> (
      match Symbol.Table.find_opt env.types (key_at env name_span) with
      | Some (DStruct info) ->
          let tfields =
            match inits with
            | (None, _, _) :: _ -> positional_fields env e.span info inits
            | _ -> named_fields env info inits
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
  | Match (scrutinee, arms) -> check_match env scrutinee arms Infer
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
      T.mk (if diverges then TNever else TUnit) (T.TWhile (label, tc, tb))
  | For (label, name, nspan, iter, body) ->
      synth_for env e.span label name nspan iter body
  | Binding (kind, name, nspan, ann, init) ->
      snd (check_binding env kind name nspan ann init)
  | Return init -> synth_return env e.span init
  | Break (label, value) -> synth_break env e.span label value
  | Continue label ->
      ignore (find_loop_or_error env e.span "`continue` outside a loop" label);
      T.mk TNever (T.TContinue label)
  | PairAssign (ft, st, fv, sv) -> synth_pair_assign env ft st fv sv
  | Loop (label, body) -> check_loop_expr env e.span label body None
  | Unit -> T.mk TUnit T.TUnit

(* Probe a block's result type with diagnostics muted so a sibling can anchor it *)
and block_result_ty (env : env) (body : block) =
  let quiet = probing env in
  let inner = push_scope quiet in
  let _, tb = check_block inner Ast.dummy_span body Infer in
  tblock_ty tb

and coerce_common (env : env) (first : coercion_input)
    (rest : coercion_input list) =
  let add_typed common = function
    | Typed (_, typed) -> common_ty common typed.T.ty
    | Contextual _ -> common
  in
  let common = List.fold_left add_typed (add_typed TNever first) rest in
  let common, first =
    if common <> TNever then (common, first)
    else
      match first with
      | Contextual source ->
          let typed = synth env source in
          (typed.T.ty, Typed (source, typed))
      | Typed (_, typed) -> (typed.T.ty, first)
  in
  let coerce = function
    | Contextual source -> check env source common
    | Typed (source, typed) -> coerce_expr env source common typed
  in
  (coerce first :: List.map coerce rest, common)

and coerce_common_pair (env : env) ~(contextual : expr -> bool) (left : expr)
    (right : expr) =
  if contextual left && not (contextual right) then
    let typed_right = synth_operand env right in
    let common = typed_right.T.ty in
    (check_operand env left common, typed_right, common)
  else if contextual right then
    let typed_left = synth_operand env left in
    let common = typed_left.T.ty in
    (typed_left, check_operand env right common, common)
  else
    let typed_left = synth_operand env left in
    let typed_right = synth_operand env right in
    let common = common_ty typed_left.T.ty typed_right.T.ty in
    ( coerce_expr env left common typed_left,
      coerce_expr env right common typed_right,
      common )

and synth_array_lit (env : env) (first : expr) (rest : expr list) =
  let probe e =
    if arm_is_flexible e then Contextual e else Typed (e, synth env e)
  in
  let tes, elem = coerce_common env (probe first) (List.map probe rest) in
  let elem =
    match elem with
    | (TUnit | TNever) as t ->
        emit env
          (Diagnostic.with_type first.span "array element cannot have this type"
             (show_ty env t));
        TError
    | t -> t
  in
  T.mk (TArray (elem, List.length tes)) (T.TArrayLit tes)

and named_fields (env : env) (info : struct_info)
    (inits : (Ast.name option * Ast.span * expr) list) =
  let seen = Hashtbl.create 4 in
  let find_field fname =
    List.find_mapi
      (fun field_id (name, ft) ->
        if name = fname then Some (field_id, ft) else None)
      info.field_tys
  in
  let check_named_field (fname, fspan, e) =
    match fname with
    | None -> None
    | Some fname -> (
        match find_field fname with
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
    if Hashtbl.mem seen fname then None else Some (field_id, T.mk ft T.TZero)
  in
  (* This keeps field values in the order they appear meaning pair { y: mark(1), x: mark(2) } runs mark(1) first *)
  written_fields
  @ List.filter_map
      (fun (field_id, field) -> default_field field_id field)
      (List.mapi (fun field_id field -> (field_id, field)) info.field_tys)

(* Every field in a positional argument has to be defined because of the order *)
and positional_fields (env : env) (span : Ast.span) (info : struct_info)
    (inits : (Ast.name option * Ast.span * expr) list) =
  let expected = List.length info.field_tys in
  let found = List.length inits in
  if found <> expected then
    emit env
      (Diagnostic.error "wrong number of fields"
      |> Diagnostic.at span
      |> Diagnostic.label
           (Printf.sprintf "expected %d, found %d" expected found));
  let field field_id (_, ft) =
    match List.nth_opt inits field_id with
    | Some (_, _, init) -> (field_id, check env init ft)
    | None -> (field_id, T.mk ft T.TZero)
  in
  List.mapi field info.field_tys

and reconcile_if_result (env : env) (branches : (expr * block Ast.spanned) list)
    (else_b : block) =
  reconcile_arms env
    (List.map (fun (_, { Ast.value; _ }) -> value) branches @ [ else_b ])

(* Literals bend to the common rigid type so they don't anchor it *)
and reconcile_arms (env : env) (bodies : block list) =
  let add_candidate (rigid, flexible) body =
    let candidate = block_result_ty env body in
    if block_is_flexible body then (rigid, common_ty flexible candidate)
    else (common_ty rigid candidate, flexible)
  in
  let rigid, flexible = List.fold_left add_candidate (TNever, TNever) bodies in
  if rigid = TNever then flexible else rigid

and check_value_for_use (env : env) (e : expr) = function
  | Infer -> synth env e
  | Expect want -> check env e want
  | Discard -> (
      match e.desc with
      | If (branches, else_body) ->
          {
            (check_if_discarded env e.span branches else_body) with
            T.span = e.span;
          }
      | Match (scrutinee, arms) ->
          { (check_match env scrutinee arms Discard) with T.span = e.span }
      | _ ->
          let te = synth env e in
          warn_discarded_operation env e te;
          te)

(* Thread env so a binding is visible to later elements *)
and check_block (env : env) (span : Ast.span) (body : block) (use : result_use)
    =
  let rec go env diverged acc (elems : block_item list) =
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
        go env (diverged || te.T.ty = TNever) (te :: acc) rest
  in
  go env false [] body

and check_elem (env : env) (item : block_item) (use : result_use) :
    env * T.texpr =
  match item with
  | Decl _ ->
      verify_unit_result env (block_item_span item) use;
      (env, T.mk TUnit T.TLocalDecl)
  | Expr ({ desc = Binding (kind, name, nspan, ann, init); _ } as e) ->
      let env', tb = check_binding env kind name nspan ann init in
      verify_unit_result env e.span use;
      (env', tb)
  | Expr e -> (env, check_value_for_use env e use)

(* Push a scope for the block then flag any leftover bindings *)
and check_scoped_block ?loop (env : env) (span : Ast.span) (body : block)
    (use : result_use) =
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
    (nspan : Ast.span) (ann : typ option) (init : expr option) =
  let t, te =
    match (ann, init) with
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
        emit env (Diagnostic.cannot_infer nspan);
        (* A real type here makes every later use mismatch against a type nobody wrote *)
        (TError, dummy_texpr)
  in
  if kind = Comptime then (
    verify_const_scalar env nspan t;
    let value = const_value_or env dummy_value te in
    Symbol.Table.replace env.l_vals (Symbol.key (sym env nspan)) value;
    ());
  ( extend_var env nspan name t,
    T.mk TUnit (T.TBinding (kind, sym env nspan, t, te)) )

and synth_return (env : env) (span : Ast.span) (init : expr option) =
  if env.ret_ty = TNever then
    add_error env span "a never function cannot return";
  match init with
  | None ->
      if env.ret_ty <> TNever && env.ret_ty <> TUnit && not env.in_main then
        add_error env span "empty return in non-unit function";
      T.mk TNever (T.TReturn None)
  | Some e when env.ret_ty = TNever ->
      T.mk TNever (T.TReturn (Some (synth env e)))
  | Some e ->
      let te = check env e env.ret_ty in
      T.mk TNever (T.TReturn (Some te))

and check_loop_expr (env : env) (span : Ast.span)
    (label : Ast.loop_label option) (body : block) (want : ty option) =
  let loop = new_loop label ~valued:true in
  loop.result <-
    Option.fold ~none:InferLoopResult ~some:(fun t -> ExpectLoopResult t) want;
  let tb, _ = check_scoped_block ~loop env span body Discard in
  let ty =
    match (loop.result, loop.bare_break) with
    | FlexibleLoopResult (t, _, _), _ | RigidLoopResult (t, _, _), _ -> t
    | (InferLoopResult | ExpectLoopResult _), None -> TNever
    | InferLoopResult, Some _ -> TUnit
    | ExpectLoopResult want, Some break_span ->
        emit env
          (Diagnostic.type_mismatch break_span ~expected:(show_ty env want)
             ~found:"()");
        TUnit
  in
  T.mk ty (T.TLoop (label, tb))

and check_valued_break (env : env) (lc : loop_ctx) (ve : expr) =
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
  match lc.result with
  | InferLoopResult ->
      let typed = synth env ve in
      if typed.T.ty <> TNever then begin
        report_bare typed.T.ty;
        lc.result <-
          (if flexible then FlexibleLoopResult (typed.T.ty, ve.span, [ ve ])
           else RigidLoopResult (typed.T.ty, ve.span, []))
      end;
      typed
  | ExpectLoopResult want ->
      let typed = check env ve want in
      if typed.T.ty <> TNever then begin
        report_bare want;
        lc.result <-
          RigidLoopResult (want, ve.span, if flexible then [ ve ] else [])
      end;
      typed
  | FlexibleLoopResult (want, first, values) ->
      if flexible then begin
        lc.result <- FlexibleLoopResult (want, first, ve :: values);
        check env ve want
      end
      else
        let typed = synth env ve in
        if typed.T.ty = TNever then typed
        else begin
          check_flexible_values typed.T.ty values;
          lc.result <- RigidLoopResult (typed.T.ty, first, values);
          typed
        end
  | RigidLoopResult (want, first, values) ->
      if flexible then begin
        lc.result <- RigidLoopResult (want, first, ve :: values);
        check env ve want
      end
      else
        let typed = synth env ve in
        let common = common_ty want typed.T.ty in
        if not (ty_equal common want) then begin
          check_flexible_values common values;
          lc.result <- RigidLoopResult (common, first, values)
        end;
        coerce_expr env ve common typed

and synth_break (env : env) (span : Ast.span) (label : Ast.loop_label option)
    (value : expr option) =
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
  T.mk TNever (T.TBreak (label, tv))

and synth_for (env : env) (span : Ast.span) (label : Ast.loop_label option)
    (name : Ast.name) (nspan : Ast.span) (iter : expr) (body : block) =
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
        match resolve_ty ti.T.ty with
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
  T.mk TUnit (T.TFor (label, sym env nspan, elem_ty, titer, tb))

(* An arm can end in a call whose value nobody wanted so each one is checked on its own *)
and check_if_discarded (env : env) (span : Ast.span)
    (branches : (expr * block Ast.spanned) list)
    (else_body : block Ast.spanned option) =
  let arm body = fst (check_scoped_block env span body Discard) in
  let tbranches =
    List.map
      (fun (c, { Ast.value = body; _ }) -> (check env c TBool, arm body))
      branches
  in
  let telse = Option.map (fun { Ast.value = body; _ } -> arm body) else_body in
  let ty = if_result_ty tbranches telse ~fallback_ty:TUnit in
  T.mk ty (T.TIf (tbranches, telse))

(* One if handles both a value and a plain statement and want None means synthesize *)
and check_if (env : env) (span : Ast.span)
    (branches : (expr * block Ast.spanned) list)
    (else_body : block Ast.spanned option) (want : ty option) =
  match (want, else_body) with
  | None, None ->
      let tbranches =
        List.map
          (fun (c, { Ast.value = body; _ }) ->
            (check env c TBool, fst (check_scoped_block env span body Discard)))
          branches
      in
      T.mk TUnit (T.TIf (tbranches, None))
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
            if resolve_ty w <> TUnit then
              emit env
                (Diagnostic.type_mismatch span ~expected:(show_ty env w)
                   ~found:(show_ty env TUnit));
            None
      in
      let ty = if_result_ty tbranches telse ~fallback_ty:w in
      T.mk ty (T.TIf (tbranches, telse))

(* A binding lands in the scope of the arm so the env comes back out *)
and check_pattern (env : env) (sty : ty) (pat : pattern) =
  match pat.pdesc with
  | PatWild -> (env, Some T.TPatWild)
  | PatBind name ->
      let symbol = sym env pat.pspan in
      (* The resolver already decided so a comptime name reads as a constant *)
      if Symbol.is_comptime symbol.Symbol.kind then
        check_pattern env sty
          { pat with pdesc = PatValue { desc = Ident name; span = pat.pspan } }
      else (extend_var env pat.pspan name sty, Some (T.TPatBind (symbol, sty)))
  | PatValue e -> (
      let te = check env e sty in
      (* TODO: comparing these needs more than the integer test an arm emits *)
      let not_comparable () =
        emit env
          (Diagnostic.error_at pat.pspan "pattern is not comparable"
          |> Diagnostic.label ("cannot test " ^ show_ty env te.T.ty));
        (env, None)
      in
      (* TODO: a named constant and a range should both work as patterns *)
      let not_a_literal () =
        emit env
          (Diagnostic.error_at pat.pspan "pattern is not a literal"
          |> Diagnostic.help "an arm names a literal or an enum variant");
        (env, None)
      in
      match te.T.desc with
      | T.TVariant (_, value) -> (env, Some (T.TPatConst value))
      | T.TInt n -> (env, Some (T.TPatConst n))
      | T.TBool b -> (env, Some (T.TPatConst (if b then 1L else 0L)))
      | T.TChar c -> (env, Some (T.TPatConst (Int64.of_int c)))
      | T.TErrorExpr -> (env, None)
      | T.TFloat _ | T.TStr _ | T.TCStr _ -> not_comparable ()
      | _ -> (
          match te.T.const with
          | Some (Constant.VFloat _) -> not_comparable ()
          | Some value -> (env, Some (T.TPatConst (Constant.int_of value)))
          | None when is_integer te.T.ty ->
              let value = const_value_or env dummy_value te in
              (env, Some (T.TPatConst (Constant.int_of value)))
          | None -> not_a_literal ()))

and check_match (env : env) (scrutinee : expr) (arms : arm list)
    (use : result_use) =
  let ts = synth env scrutinee in
  let bodies = List.map (fun (a : arm) -> a.arm_body.Ast.value) arms in
  let want =
    match use with
    | Infer -> Some (reconcile_arms env bodies)
    | Expect w -> Some w
    | Discard -> None
  in
  let arm_use = match want with Some w -> Expect w | None -> Discard in
  (* Nothing here checks coverage so a match that names no catch all can fall out *)
  let seen = ref [] in
  let caught_all = ref false in
  let record (pat : pattern) (tpat : T.tpattern) =
    if !caught_all then
      emit env (Diagnostic.error_at pat.pspan "arm never runs")
    else
      match tpat with
      | T.TPatWild | T.TPatBind _ -> caught_all := true
      | T.TPatConst n ->
          if List.mem n !seen then
            emit env (Diagnostic.error_at pat.pspan "duplicate pattern")
          else seen := n :: !seen
  in
  let check_arm (a : arm) =
    let arm_env, tpat = check_pattern (push_scope env) ts.T.ty a.pat in
    Option.iter (record a.pat) tpat;
    (* A broken pattern still checks its body so the errors inside show up *)
    let tbody, _ =
      check_scoped_block arm_env a.arm_body.Ast.span a.arm_body.Ast.value
        arm_use
    in
    Option.map (fun tpat -> { T.tpat; tbody }) tpat
  in
  let tarms = List.filter_map check_arm arms in
  (* A catch all with every arm diverging is the only way nothing falls past *)
  let diverges =
    !caught_all
    && List.for_all (fun (a : T.tarm) -> tblock_ty a.T.tbody = TNever) tarms
  in
  let ty =
    match (diverges, want) with
    | true, _ -> TNever
    | false, Some w -> w
    | false, None -> TUnit
  in
  T.mk ty (T.TMatch (ts, tarms))

(* This has to be this type *)
and check (env : env) (e : expr) (want : ty) =
  let typed = check_operand env e want in
  report_const_range env typed;
  typed

and check_operand (env : env) (e : expr) (want : ty) =
  stamp env e.span (check_desc env e want)

and check_desc (env : env) (e : expr) (want : ty) =
  let target = resolve_ty want in
  (* Synthesize then check the result matches want *)
  let check_by_synth () =
    let te = synth_operand env e in
    coerce_expr env e want te
  in
  match e.desc with
  | ErrorExpr -> dummy_texpr
  | Int (n, None) -> (
      (* An untyped literal takes the wanted type and checks its base *)
      match adopt_int_literal env e.span want target ~neg:false n with
      | Some te -> te
      | None ->
          emit env
            (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
               ~found:"i32");
          T.mk (TInt I32) (T.TInt n))
  | Float (f, None) -> (
      match target with
      | TFloat _ -> T.mk want (T.TFloat f)
      | TError -> T.mk want (T.TFloat f)
      | _ ->
          emit env
            (Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
               ~found:"f64");
          T.mk (TFloat F64) (T.TFloat f))
  | String str -> (
      match target with
      | TStr -> T.mk want (T.TStr str)
      | _ -> check_by_synth ())
  | SizeOf t when is_integer target -> (
      match ty_of_ast env t with
      | TError -> dummy_texpr
      | ty ->
          let size = Int64.of_int (ty_size env.struct_fields ty) in
          let kind = int_kind_of target in
          if not (Constant.representable kind (Constant.of_magnitude size)) then
            emit env
              (Diagnostic.error "size does not fit"
              |> Diagnostic.at e.span
              |> Diagnostic.label
                   (Printf.sprintf "%Ld does not fit in %s" size
                      (show_ty env (resolve_ty want))));
          T.mk want (T.TSizeOf ty))
  | UnOp (Neg, { desc = Int (n, None); _ }) -> (
      match adopt_int_literal env e.span want target ~neg:true n with
      | Some te -> te
      | None -> check_operand env { e with desc = Int (Int64.neg n, None) } want
      )
  | UnOp (Neg, { desc = Float (f, suf); _ }) ->
      check_operand env { e with desc = Float (-.f, suf) } want
  | UnOp (Neg, { desc = Int (_, Some _); _ }) -> check_by_synth ()
  | UnOp (Pos, ({ desc = Int _; _ } as operand)) ->
      check_operand env { operand with span = e.span } want
  | UnOp (((Neg | Pos | BitNot) as op), operand)
    when unop_accepts op (resolve_ty want) ->
      T.mk want (T.TUnOp (op, check_operand env operand want))
  | ArrayLit elems -> (
      match resolve_ty want with
      | TArray (elem, n) ->
          if List.compare_length_with elems n <> 0 then
            emit env
              (Diagnostic.arity e.span
                 ~expected:(Printf.sprintf "expected %d elements" n)
                 ~found:(List.length elems));
          let tes = List.map (fun e -> check env e elem) elems in
          T.mk (TArray (elem, n)) (T.TArrayLit tes)
      | _ -> check_by_synth ())
  | BinOp (((Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor) as op), l, r)
    when binop_accepts op (resolve_ty want) ->
      T.mk want
        (T.TBinOp (op, check_operand env l want, check_operand env r want))
  | BinOp (((Lshift | Rshift) as op), l, r) when is_integer (resolve_ty want) ->
      let base = check_operand env l want in
      let count = synth env r in
      verify_shift_count env r.span count;
      T.mk want (T.TBinOp (op, base, count))
  | Block body ->
      let tb, ty = check_scoped_block env e.span body (Expect want) in
      T.mk ty (T.TBlock tb)
  | If (branches, else_body) ->
      check_if env e.span branches else_body (Some want)
  | Match (scrutinee, arms) -> check_match env scrutinee arms (Expect want)
  | Loop (label, body) -> check_loop_expr env e.span label body (Some want)
  | Undefined -> T.mk want T.TUndef
  | _ -> check_by_synth ()

and coerce_expr (env : env) (e : expr) (want : ty) (te : T.texpr) =
  let got = te.T.ty in
  if compatible want got then adopt_slice want te
  else if widens_to got want then
    let widened = T.mk ~span:e.span want (T.TCast te) in
    { widened with T.const = const_of env ~span:e.span widened }
  else begin
    let mismatch =
      Diagnostic.type_mismatch e.span ~expected:(show_ty env want)
        ~found:(show_ty env got)
    in
    let mismatch =
      match (e.desc, resolve_ty want) with
      | Assign (None, _, _), TBool ->
          Diagnostic.help "did you mean `==` to compare?" mismatch
      | _ -> mismatch
    in
    emit env mismatch;
    te
  end

and check_matching_operands (env : env) (l : expr) (r : expr) =
  coerce_common_pair env ~contextual:is_num_literal l r

and check_range_bounds (env : env) (lo : expr) (hi : expr) =
  let tlo, thi, t = check_matching_operands env lo hi in
  if not (is_integer t) then
    add_error env lo.span "range bounds must be integers";
  (* A bound never reaches the walk since a range is not a value *)
  report_const_range env tlo;
  report_const_range env thi;
  (tlo, thi, t)

and check_args (env : env) (span : Ast.span) (sig_ : func_sig)
    (args : expr list) =
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
      let fixed = List.take n_params args in
      let rest = List.drop n_params args in
      (* C reads a float vararg as a double so widen it first *)
      let promote_vararg e =
        let te = synth env e in
        match resolve_ty te.T.ty with
        | TFloat F32 -> T.mk ~span:e.span (TFloat F64) (T.TCast te)
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

and synth_binop (env : env) (op : binop) (l : expr) (r : expr) =
  match op with
  | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor ->
      let tl, tr, t = check_matching_operands env l r in
      verify_operands env l.span op t;
      T.mk t (T.TBinOp (op, tl, tr))
  | Lshift | Rshift ->
      let tl = synth_operand env l in
      let tr = synth_operand env r in
      verify_operands env l.span op tl.T.ty;
      verify_shift_count env r.span tr;
      T.mk tl.T.ty (T.TBinOp (op, tl, tr))
  | Eq | Neq | Lt | Gt | Lte | Gte ->
      let tl, tr, t = check_comparison_operands env l r in
      verify_operands env l.span op t;
      T.mk TBool (T.TBinOp (op, tl, tr))
  | And | Or ->
      let tl = check_operand env l TBool in
      let tr = check_operand env r TBool in
      T.mk TBool (T.TBinOp (op, tl, tr))

and synth_assign (env : env) (base : binop option) (l : expr) (r : expr) =
  let tl, tr = check_assign_operands env base l r in
  T.mk TUnit (T.TAssign (base, tl, tr))

and check_comparison_operands (env : env) (l : expr) (r : expr) =
  let is_contextual_literal e =
    is_num_literal e || match e.desc with String _ -> true | _ -> false
  in
  coerce_common_pair env ~contextual:is_contextual_literal l r

and check_assign_operands (env : env) (base : binop option) (l : expr)
    (r : expr) =
  let tl = synth env l in
  if tl.T.ty <> TError && not (is_lvalue tl) then
    add_error env l.span "cannot assign to expression";
  verify_param_copy_write env l.span tl;
  (match tl.T.desc with
  | T.TIdent s when Symbol.is_func s.Symbol.kind ->
      emit env (Diagnostic.error_at l.span "cannot assign to function")
  | T.TIdent _ | T.TFieldAccess _ | T.TIndex _ -> (
      (* This catches assignment to an immutable binding whether it's local or global. *)
      match root_binding tl with
      | Some s
        when Symbol.is_immutable s.Symbol.kind
             || Symbol.is_global s.Symbol.kind
                && is_comptime_global env (Symbol.key s) ->
          emit env (Diagnostic.error_at l.span "cannot assign to immutable")
      | _ -> ())
  | _ -> ());
  let t = tl.T.ty in
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

and synth_pair_assign (env : env) (ft : expr) (st : expr) (fv : expr)
    (sv : expr) =
  let ft, fv = check_assign_operands env None ft fv in
  let st, sv = check_assign_operands env None st sv in
  T.mk TUnit (T.TPairAssign (ft, st, fv, sv))

and synth_unop (env : env) (op : unop) (e : expr) =
  match op with
  | Pos | Neg | BitNot ->
      let te = synth_operand env e in
      let t = te.T.ty in
      if not (unop_accepts op t) then
        emit env
          (Diagnostic.bad_operand e.span ~op:(show_unop_sym op)
             ~ty:(show_ty env t));
      T.mk t (T.TUnOp (op, te))
  | Not ->
      let te = check env e TBool in
      T.mk TBool (T.TUnOp (op, te))
  | Deref -> (
      let te = synth env e in
      match resolve_ty te.T.ty with
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
            |> Diagnostic.help "a const has no storage, use var")
      | _ ->
          if te.T.ty <> TError && not (is_lvalue te) then
            add_error env e.span "cannot take address of expression");
      T.mk (TPointer te.T.ty) (T.TUnOp (op, te))

(* This figures out the type of a field access *)
and synth_field (env : env) (span : Ast.span) (e : expr) (fname : Ast.name)
    (fspan : Ast.span) =
  let te = synth env e in
  let ty = te.T.ty in
  match resolve_ty ty with
  | TStr -> (
      match fname with
      | n when n = len_name -> T.mk (TInt Usize) (T.TLen te)
      | _ -> no_such_field env fspan ty)
  | TArray (elem, _) | TSlice elem -> (
      match fname with
      | n when n = len_name -> T.mk (TInt Usize) (T.TLen te)
      | n when n = ptr_name -> T.mk (TPointer elem) (T.TDataPtr te)
      | _ -> no_such_field env fspan ty)
  | TOpaquePtr ->
      emit env (Diagnostic.opaque_operation span "access a field of");
      dummy_texpr
  | _ -> synth_struct_field env span te ty fname fspan

(* A type in call position converts its one argument *)
and synth_type_call (env : env) (span : Ast.span) (callee : expr)
    (args : expr list) =
  let sym = Resolve.sym_at env.uses callee.span in
  let ty = named_ty env callee.span sym.Symbol.name in
  match args with
  | [ arg ] -> synth_conversion env span (synth_operand env arg) ty
  | _ ->
      emit env
        (Diagnostic.arity span ~expected:"expected 1 argument"
           ~found:(List.length args));
      List.iter (fun a -> ignore (synth env a)) args;
      T.mk ty T.TErrorExpr

and synth_call (env : env) (span : Ast.span) (callee : expr) (args : expr list)
    =
  (* A qualified callee is one symbol so it still calls direct *)
  let direct_callee =
    match callee.desc with
    | Ident _ | Path _ -> (
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
  | None when Symbol.Table.mem env.types (key_at env callee.span) ->
      synth_type_call env span callee args
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

and synth_index (env : env) (span : Ast.span) (base : expr) (idx : expr) =
  let tbase = synth env base in
  match resolve_ty tbase.T.ty with
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
and const_of_symbol (env : env) (s : Symbol.t) (span : Ast.span) =
  match s.Symbol.kind with
  | Symbol.Local Ast.Comptime -> Symbol.Table.find_opt env.l_vals (Symbol.key s)
  | Symbol.Global Ast.Comptime when Symbol.Table.mem env.g_state (Symbol.key s)
    ->
      Some (global_const_num env span (Symbol.key s))
  | _ -> None

and global_const_num (env : env) (span : Ast.span) (key : Symbol.key) =
  let st = global_state env span key in
  match st.value with
  | Some v -> v
  | None ->
      if st.busy then
        raise (Diagnostic.Errors [ Diagnostic.error_at span "cyclic constant" ]);
      let te =
        match global_typed_init env span key with
        | te -> te
        | exception e ->
            st.value <- Some dummy_value;
            raise e
      in
      st.busy <- true;
      let v =
        match Option.value te.T.const ~default:dummy_value with
        | v ->
            st.busy <- false;
            v
        | exception e ->
            st.busy <- false;
            st.value <- Some dummy_value;
            raise e
      in
      st.value <- Some v;
      Symbol.Table.replace env.l_vals key v;
      v

(* The init is what an unannotated global gets its type from *)
and global_ty (env : env) (gd : global_def) =
  match (gd.typ, gd.init) with
  | Some t, _ -> ty_of_ast env t
  | None, Some e ->
      let te =
        try global_typed_init env e.span (key_at env gd.span)
        with Diagnostic.Errors ds ->
          List.iter (emit env) ds;
          dummy_texpr
      in
      te.T.ty
  | None, None ->
      emit env (Diagnostic.cannot_infer gd.name_span);
      TError

(* Typing shares the busy flag so a self demand mid typing is a cycle *)
and global_typed_init (env : env) (span : Ast.span) (key : Symbol.key) =
  let st = global_state env span key in
  match st.typed with
  | Some te -> te
  | None ->
      if st.busy then
        raise (Diagnostic.Errors [ Diagnostic.error_at span "cyclic constant" ]);
      let e, typ =
        match st.def with
        | { init = Some e; typ; _ } -> (e, typ)
        | _ -> raise (Diagnostic.Errors [ Constant.unsupported_const span ])
      in
      st.busy <- true;
      let typed () =
        match typ with
        | Some t -> check env e (ty_of_ast env t)
        | None -> synth env e
      in
      let te =
        match typed () with
        | te -> te
        | exception ex ->
            st.busy <- false;
            raise ex
      in
      st.busy <- false;
      st.typed <- Some te;
      te

(* The folded size fixes dropped suffixes and silent wraps on huge counts *)
and eval_array_size (env : env) (e : expr) =
  let bad msg =
    add_error env e.span msg;
    0
  in
  let te = synth env e in
  if not (is_integer te.T.ty) then bad "array size must be an integer"
  else
    let v = const_value_or env dummy_value te in
    let n = Constant.int_of v in
    (* The source may be an expression so the message shows the evaluated value *)
    let shown =
      if is_unsigned te.T.ty then Printf.sprintf "%Lu" n else Int64.to_string n
    in
    if
      Int64.compare n 0x7FFF_FFFFL > 0
      || (Int64.compare n 0L < 0 && is_unsigned te.T.ty)
    then bad ("array size is too large: " ^ shown)
    else if Int64.compare n 0L < 0 then bad ("array size is negative: " ^ shown)
    else Int64.to_int n

(* Main implicitly returns i32 for the C runtime and everything else returns unit *)
(* FIXME(80e8): The default keeps main working until return types are inferred *)
let ret_ty_of (env : env) (fd : func_def) =
  match fd.ret with
  | Some t -> return_ty_of_ast env t
  | None -> if is_entry env fd.func_span then TInt I32 else TUnit

(* First pass collecting signatures so that the compiler can handle forward references *)
let collect_func (env : env) (fd : func_def) =
  let abi = resolve_abi env fd.extern_abi in
  let param_tys =
    List.map (fun (p : param) -> ty_of_ast env p.param_typ) fd.params
  in
  let ret_ty = ret_ty_of env fd in
  Symbol.Table.replace env.funcs (key_at env fd.func_span)
    { param_tys; ret_ty; variadic = fd.variadic; abi }

(* A repeat name is already reported with both spans by the resolver *)
let type_name_taken (env : env) (span : Ast.span) =
  Symbol.Table.mem env.types (key_at env span)

(* The name goes in first so a field can name this struct or one defined later *)
let reserve_struct_name (env : env) (sd : struct_def) =
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

let fill_struct_fields (env : env) (sd : struct_def) =
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
let verify_struct_cycle (env : env) (sd : struct_def) =
  let fields_of name =
    Option.value ~default:[] (Symbol.Table.find_opt env.struct_fields name)
  in
  let on_path = Hashtbl.create 8 in
  let rec reaches (target : Symbol.key) (t : ty) =
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

(* A variant names no type so one pass settles the whole enum *)
let reserve_enum_name (env : env) (ed : enum_def) =
  if not (type_name_taken env ed.enum_span) then begin
    let add (vals, next) (v : variant) =
      if List.mem_assoc v.variant_name vals then begin
        emit env (Diagnostic.error_at v.variant_span "duplicate variant");
        (vals, next)
      end
      else ((v.variant_name, next) :: vals, Int64.succ next)
    in
    let vals, _ = List.fold_left add ([], 0L) ed.variants in
    Symbol.Table.replace env.types (key_at env ed.enum_span)
      (DEnum { variant_vals = List.rev vals })
  end

(* A fake error type goes in the table first and it just means the real body hasn't been read yet *)
let reserve_alias_name (env : env) (td : type_alias_def) =
  if not (type_name_taken env td.alias_span) then
    Symbol.Table.replace env.types (key_at env td.alias_span) (DAlias TError)

let collect_alias (env : env) (td : type_alias_def) =
  match Symbol.Table.find_opt env.types (key_at env td.alias_span) with
  | Some (DAlias TError) ->
      Symbol.Table.replace env.types (key_at env td.alias_span)
        (DAlias (ty_of_ast env td.alias_typ))
  | _ -> ()

let rec named_type_spans (t : typ) =
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
let collect_type_bodies (env : env) (decls : decl list) =
  let defs = Hashtbl.create 16 in
  let remember (decl : decl) =
    match decl with
    | TypeAlias td ->
        (* Only the first one counts because a repeat name already got turned down *)
        (* An unresolved name shares one key so two broken types would look mutually recursive *)
        let key = key_at env td.alias_span in
        if key <> unresolved_key && not (Hashtbl.mem defs key) then
          Hashtbl.add defs key decl
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ()
  in
  let unfilled (decl : decl) =
    match decl with
    | TypeAlias td ->
        Symbol.Table.find_opt env.types (key_at env td.alias_span)
        = Some (DAlias TError)
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> false
  in
  let fill (decl : decl) =
    match decl with
    | TypeAlias td -> collect_alias env td
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ()
  in
  let on_path = Hashtbl.create 8 in
  let rec force (key : Symbol.key) =
    match Hashtbl.find_opt defs key with
    | None -> ()
    | Some (TypeAlias td as decl) ->
        if Hashtbl.mem on_path key then
          emit env (Diagnostic.error_at td.alias_name_span "recursive type")
        else if unfilled decl then begin
          Hashtbl.add on_path key ();
          let step span = force (key_at env span) in
          List.iter step (named_type_spans td.alias_typ);
          Hashtbl.remove on_path key;
          if unfilled decl then fill decl
        end
    | Some (Func _ | Extern _ | Global _ | Struct _ | Enum _) -> ()
  in
  List.iter remember decls;
  (* The order here follows the file so the same type gets blamed every time *)
  List.iter
    (function
      | TypeAlias td -> force (key_at env td.alias_span)
      | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ())
    decls

let collect_global (env : env) (gd : global_def) =
  (if gd.init = None then
     match gd.kind with
     | Var -> ()
     | Comptime ->
         emit env
           (Diagnostic.error_at gd.name_span "comptime without initializer"));
  let t = global_ty env gd in
  Symbol.Table.replace env.globals (key_at env gd.span) (t, gd.kind)

let fill_struct_fields_decl (env : env) (decl : decl) =
  match decl with Struct sd -> fill_struct_fields env sd | _ -> ()

let verify_cycle_decl (env : env) (decl : decl) =
  match decl with Struct sd -> verify_struct_cycle env sd | _ -> ()

(* Every type name lands first so a signature can name a type written later *)
let reserve_type_name (env : env) (decl : decl) =
  let env = reading env decl in
  match decl with
  | Struct sd -> reserve_struct_name env sd
  | TypeAlias td -> reserve_alias_name env td
  | Enum ed -> reserve_enum_name env ed
  | Func _ | Extern _ | Global _ -> ()

let collect_decl (env : env) (decl : decl) =
  let env = reading env decl in
  match decl with
  | Func fd | Extern fd -> collect_func env fd
  | Global gd -> collect_global env gd
  | Struct _ | TypeAlias _ | Enum _ -> ()

let check_func ?(is_extern = false) (env : env) (fd : func_def) =
  (* The collected signature is reused so a bad array size errors once *)
  let collected = Symbol.Table.find_opt env.funcs (key_at env fd.func_span) in
  let param_tys =
    match collected with
    | Some s when List.compare_lengths s.param_tys fd.params = 0 -> s.param_tys
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
    (not is_extern) && ret_ty <> TUnit
    && ((not is_entry_point) || fd.ret <> None)
  in
  let body_use =
    if not implicit_return then Discard
    else if is_entry_point then
      (* The main function can fall off the end with 0 so a unit tail stays fine *)
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

let rec is_const_texpr (env : env) (te : T.texpr) =
  match te.T.desc with
  | T.TErrorExpr -> true
  | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TChar _ | T.TCStr _
  | T.TStr _ | T.TSizeOf _ | T.TVariant _ ->
      true
  | T.TIdent s ->
      Symbol.is_func s.Symbol.kind || is_comptime_global env (Symbol.key s)
  | T.TUnOp (Ast.AddressOf, { T.desc = T.TIdent s; _ }) ->
      Symbol.is_global s.Symbol.kind
  | T.TUnOp (_, e) -> is_const_texpr env e
  | T.TBinOp (_, l, r) -> is_const_texpr env l && is_const_texpr env r
  | T.TCast e -> is_const_texpr env e
  | T.TZero -> true
  | T.TArrayLit elems -> List.for_all (is_const_texpr env) elems
  | T.TStructLit (_, fields) ->
      List.for_all (fun (_, fe) -> is_const_texpr env fe) fields
  | T.TUndef -> true
  | T.TUnit -> true
  | _ -> false

let check_global (env : env) (gd : global_def) =
  (* The collected type is reused so a bad array size errors once *)
  let t =
    match Symbol.Table.find_opt env.globals (key_at env gd.span) with
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

let typed_struct_decl (env : env) (sd : struct_def) (fields : ty list) =
  let name = qname_at env sd.struct_span (Interner.text sd.struct_name) in
  let is_local =
    Option.exists
      (fun symbol -> symbol.Symbol.kind = Symbol.LocalType)
      (Resolve.sym_at_opt env.uses sd.struct_span)
  in
  if is_local then T.TLocalStruct (name, fields)
  else T.TStruct (name, fields, sd.struct_modifiers)

let check_decl (env : env) (decl : decl) =
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
  | TypeAlias td ->
      let t =
        match Symbol.Table.find_opt env.types (key_at env td.alias_span) with
        | Some (DAlias t) -> t
        | _ -> ty_of_ast env td.alias_typ
      in
      T.TTypeAlias (qname_at env td.alias_span (Interner.text td.alias_name), t)
  | Enum ed -> T.TEnum (qname_at env ed.enum_span (Interner.text ed.enum_name))

let force_global_consts (env : env) (tdecls : T.tdecl list) =
  List.iter
    (function
      | T.TGlobal { T.key; init = Some init; kind = Ast.Comptime; _ } -> (
          try ignore (global_const_num env init.T.span key)
          with Diagnostic.Errors ds -> List.iter (emit env) ds)
      | _ -> ())
    tdecls

(* The partial tree stays available so later checks can still run *)
let analyze ~(diags : Diagnostic.sink) (uses : Resolve.t) (decls : decl list) =
  let env = make_env diags uses in
  let decls = decls @ Resolve.local_decls uses in
  (* An early array size can demand any later const so defs go in first *)
  List.iter
    (function
      | Global gd ->
          Symbol.Table.replace env.g_state (key_at env gd.span)
            { def = gd; typed = None; value = None; busy = false }
      | _ -> ())
    decls;
  List.iter (reserve_type_name env) decls;
  collect_type_bodies env decls;
  List.iter (collect_decl env) decls;
  List.iter (fill_struct_fields_decl env) decls;
  List.iter (verify_cycle_decl env) decls;
  let tdecls = List.map (check_decl env) decls in
  force_global_consts env tdecls;
  tdecls
