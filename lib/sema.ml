(* SPDX-License-Identifier: Apache-2.0 *)

open Ast
open Types
open Typred
open Tast
open Option.Syntax

type func_sig = { func : func_ty; param_hole : bool }

type variant_info = {
  tag : int64;
  field_tys : (name option * ty) list;
  field_index : (name, int * ty) Hashtbl.t option;
}

type enum_info = { variants_by_name : (name, int64) Hashtbl.t }
type 'a deferred = Unstarted | Running | Completed of 'a
type result_use = Infer | Expect of ty | Discard
type coverage = Covered | Missing of string list | Unbounded | Unknown

(* The running mark catches a value that asks for itself *)
let force_deferred span cell (on_error : 'a) (compute : unit -> 'a) =
  match !cell with
  | Completed v -> v
  | Running ->
      raise (Diagnostic.Errors [ Diagnostic.error span "cyclic initializer" ])
  | Unstarted -> (
      cell := Running;
      try
        let v = compute () in
        cell := Completed v;
        v
      with e ->
        cell := Completed on_error;
        raise e)

type func_def = { func_name : ident; lambda : lambda; func_span : span }

type global_fact = {
  decl : binding;
  declared_ty : ty deferred ref;
  typed : texpr deferred ref;
}

type loop_result =
  (* value := loop { continue } *)
  | InferLoopResult
  (* value : i64 = loop { break 5 } *)
  | ExpectLoopResult of ty
  (* value := loop { if condition { break 1 } break 2 } *)
  | FlexibleLoopResult of ty * span * expr list
  (* value := loop { if condition { break known_i32 } break known_i64 } *)
  | RigidLoopResult of ty * span * expr list

type loop_ctx = {
  lbl : name option;
  valued : bool;
  mutable result : loop_result;
  mutable bare_break : span option;
}

type ctx = {
  decls : item list;
  items : item Symbol.Table.t;
  lambdas : tfunc_def Symbol.Table.t;
  layouts : Layout.t;
  symbols : Resolve.t;
  initial_errors : bool;
}

type local_env = {
  ctx : ctx;
  scopes : (Symbol.key * ty) list list;
  ret_ty : ty;
  loops : loop_ctx list;
  in_probe : bool;
}

type coercion_input = Contextual of expr | Typed of expr * texpr

let make_ctx symbols decls =
  let items = Symbol.Table.create 16 in
  List.iter
    (fun (key, builtin) -> Symbol.Table.replace items key (BuiltinItem builtin))
    (Resolve.builtins symbols);
  {
    decls;
    items;
    lambdas = Symbol.Table.create 16;
    layouts = Layout.create ();
    symbols;
    initial_errors = Diagnostic.has_errors ();
  }

let make_env ctx =
  { ctx; scopes = []; ret_ty = TUnit; loops = []; in_probe = false }

let decl_span = function
  | Func fd | Extern fd -> fd.func_span
  | Global gd -> gd.span
  | Struct sd -> sd.struct_span
  | TypeAlias td -> td.alias_span
  | Enum ed -> ed.enum_span

let sym env span = Resolve.sym_at env.ctx.symbols span
let error_expr span = mk span TError TErrorExpr

let fail span d =
  Diagnostic.emit d;
  error_expr span

let is_poisoned te = has_error te.ty

(* An unsigned literal past i64 max is stored as a negative bit pattern *)
let unsigned_to_float n =
  let two_pow_64 = 18446744073709551616.0 in
  if Int64.compare n 0L >= 0 then Int64.to_float n
  else Int64.to_float n +. two_pow_64

let round_to_float_kind kind f =
  match kind with
  | F32 -> Int32.float_of_bits (Int32.bits_of_float f)
  | F64 -> f

let push_scope env = { env with scopes = [] :: env.scopes }
let is_probing env = env.in_probe

(* A probe gets its own loops so a thrown away walk can't record a break *)
let probing env =
  let copy loop =
    {
      lbl = loop.lbl;
      valued = loop.valued;
      result = loop.result;
      bare_break = loop.bare_break;
    }
  in
  { env with loops = List.map copy env.loops; in_probe = true }

let extend_var env span t =
  let key = Symbol.key (sym env span) in
  match env.scopes with
  | scope :: rest -> { env with scopes = ((key, t) :: scope) :: rest }
  | [] -> assert false

(* A span the resolver never bound falls back so checking can carry on *)
let symbol_at ctx span (missing : 'a) (read : Symbol.t -> 'a) =
  match Resolve.sym_at_opt ctx.symbols span with
  | Some symbol -> read symbol
  | None -> missing

(* A name can belong to two declarations so lookups key on which one it is *)
let key_at ctx span = symbol_at ctx span Symbol.unresolved_key Symbol.key

let keyname_at ctx span fallback =
  symbol_at ctx span (Keyname.unresolved fallback) Resolve.keyname_of

(* The resolver already worked out the link name so we just read it *)
let link_name_at env span fallback =
  symbol_at env.ctx span fallback (fun s -> s.Symbol.link_name)

let is_entry env span =
  symbol_at env.ctx span ~missing:false (fun s -> s.Symbol.entry_point)

let lookup_func env span =
  match Symbol.Table.find_opt env.ctx.func_sigs (key_at env.ctx span) with
  | Some s -> s
  | None ->
      Diagnostic.emit (Diagnostic.error span "undefined function");
      {
        param_tys = [];
        ret_ty = TError;
        variadic = false;
        abi = Ripe;
        param_hole = true;
      }

let struct_info ctx key =
  match Symbol.Table.find_opt ctx.type_defs key with
  | Some (Struct_type { contents = Completed info }) -> Some info
  | _ -> None

let lookup_struct env span key =
  match struct_info env.ctx key with
  | Some _ as found -> found
  | None ->
      Diagnostic.emit (Diagnostic.error span "undefined struct");
      None

let find_field variant name =
  match variant.field_index with
  | Some index -> Hashtbl.find_opt index name
  | None ->
      List.find_mapi
        (fun field_id (field_name, ty) ->
          if field_name = Some name then Some (field_id, ty) else None)
        variant.field_tys

let struct_variant info =
  match (info.kind, info.variants) with
  | Struct, [ (_, variant) ] -> Some variant
  | _ -> None

let lift_ty f ty = if has_error ty then TError else f ty

let resolve_named_abi name span =
  match func_abi_of_string name with
  | Some abi -> abi
  | None ->
      Diagnostic.emit
        (Diagnostic.error span "unsupported ABI"
        |> Diagnostic.label "not supported here");
      AbiError

(* A signature without an ABI written on it is a plain Ripe function *)
let resolve_abi = function
  | NoAbi -> Ripe
  | NamedAbi { value = name; span } -> resolve_named_abi name span

(* The value of a block is its last element and unit when the block is empty *)
let rec tblock_ty tb =
  match tb with [] -> TUnit | [ te ] -> te.ty | _ :: rest -> tblock_ty rest

let if_result_ty tbranches telse fallback_ty =
  let diverges tb = is_never (tblock_ty tb) in
  match telse with
  | Some tb
    when diverges tb && List.for_all (fun (_, tb) -> diverges tb) tbranches ->
      TNever
  | _ -> fallback_ty

(* A literal or diverging tail bends to a sibling so it can't anchor the type *)
let rec arm_is_flexible e =
  match e.edesc with
  | Int (_, None) | Float (_, None) -> true
  | UnOp (Neg, inner) -> arm_is_flexible inner
  | Block body -> block_is_flexible body
  | If (branches, else_body) ->
      Option.is_some else_body
      && List.for_all
           (fun (_, { value = body; _ }) -> block_is_flexible body)
           branches
      && Option.exists
           (fun { value = body; _ } -> block_is_flexible body)
           else_body
  | _ -> false

and block_is_flexible = function
  | [] -> false
  | [ last ] -> arm_is_flexible last
  | _ :: rest -> block_is_flexible rest

let common_ty current candidate =
  match (current, candidate) with
  | TNever, candidate -> candidate
  | current, TNever -> current
  | TNull, candidate -> candidate
  | current, TNull -> current
  | current, candidate ->
      Option.value (common_numeric_ty current candidate) ~default:current

let verify_unit_result span = function
  | Expect want when not (ty_equal (resolve_ty want) TUnit) ->
      Diagnostic.emit
        (Diagnostic.error span "type mismatch"
        |> Diagnostic.label "expected %s, found %s" (show_ty want)
             (show_ty TUnit))
  | Infer | Discard | Expect _ -> ()

let new_loop label source =
  {
    lbl = Option.map (fun l -> l.value) label;
    valued = source = PlainLoop;
    result = InferLoopResult;
    bare_break = None;
  }

(* A break says how many loops out it jumps so later phases never see labels *)
let find_loop_or_error env span headline label =
  let found =
    List.find_mapi
      (fun i lc ->
        match label with
        | Some l when lc.lbl <> Some l.value -> None
        | _ -> Some (i, lc))
      env.loops
  in
  match (found, label) with
  | Some (depth, lc), _ -> (depth, Some lc)
  | None, None ->
      Diagnostic.emit (Diagnostic.error span "%s" headline);
      (0, None)
  | None, Some l ->
      Diagnostic.emit (Diagnostic.error l.span "undefined loop label");
      (0, None)

let verify_bare_break span lc =
  if lc.bare_break = None then lc.bare_break <- Some span;
  match lc.result with
  | InferLoopResult | ExpectLoopResult _ -> ()
  | FlexibleLoopResult (t, first, _) | RigidLoopResult (t, first, _) ->
      Diagnostic.emit
        (Diagnostic.error span "`break` values disagree"
        |> Diagnostic.label "no value here"
        |> Diagnostic.secondary first
             (Printf.sprintf "breaks with %s" (show_ty t)))

(* An array already holds the data needed by a slice and a null or string
   literal just takes the pointer type it lands in *)
let adopt want te =
  match (resolve_ty want, resolve_ty te.ty, te.tdesc) with
  | TSlice _, TArray (_, n), _ ->
      let zero = mk te.tspan (TInt Usize) (TIntLit 0L) in
      let len = mk te.tspan (TInt Usize) (TIntLit (Int64.of_int n)) in
      mk te.tspan want (TSliceExpr (te, zero, len))
  | (TPointer _ | TPtr), TNull, TNullLit | TCStr, TPointer (TInt I8), TCStrLit _
    ->
      { te with ty = want }
  | (TPointer _ | TPtr), TNull, _
  | TCStr, TPointer (TInt I8), _
  | TPointer (TInt I8), TCStr, _ ->
      mk te.tspan want (TCast te)
  | _ -> te

(* The count keeps its own type since it is only a number of positions *)
let verify_shift_count span tr =
  if not (is_integer tr.ty || is_never tr.ty) then
    Diagnostic.emit
      (Diagnostic.error span "invalid shift count"
      |> Diagnostic.found (show_ty tr.ty))

(* A bad operand poisons the result so the next operator up stays quiet *)
let verify_operands span op t operands =
  if binop_accepts op t then t
  else begin
    let shown =
      match operands with
      | [ l; r ] when not (ty_equal l r) -> show_ty l ^ " and " ^ show_ty r
      | _ -> show_ty t
    in
    Diagnostic.emit
      (Diagnostic.error span "invalid operand"
      |> Diagnostic.label "cannot apply `%s` to %s" (show_binop_sym op) shown);
    TError
  end

let adopt_int_lit span want target neg n =
  let signed = if neg then Int64.neg n else n in
  match target with
  | TInt kind when not (int_kind_fits kind neg n) ->
      Diagnostic.emit
        (Diagnostic.error span "literal out of range"
        |> Diagnostic.label "does not fit in %s" (show_ty want));
      Some (error_expr span)
  | TInt _ -> Some (mk span want (TIntLit signed))
  | TFloat kind ->
      let magnitude = unsigned_to_float n in
      let exact = if neg then -.magnitude else magnitude in
      if Int64.unsigned_compare n (float_kind_exact_limit kind) > 0 then
        Diagnostic.emit
          (Diagnostic.error span "literal loses precision"
          |> Diagnostic.label "becomes %.0f" (round_to_float_kind kind exact)
          |> Diagnostic.help
               (Printf.sprintf "write %s%Lu.0 to round"
                  (if neg then "-" else "")
                  n));
      Some (mk span want (TFloatLit exact))
  | TError -> Some (mk span want (TIntLit signed))
  | _ -> None

(* A variant is a compile time constant so nothing of the enum survives here *)
let synth_variant env (inner : expr) info fname fspan =
  let shown = Interner.text fname in
  let name = keyname_at env.ctx inner.espan shown in
  match Hashtbl.find_opt info.variants_by_name fname with
  | Some value -> mk (TEnum name) (TVariant (name, value))
  | None ->
      fail span
        (Diagnostic.error fspan "no variant"
        |> Diagnostic.label "on enum %s" (show_ty (TEnum name)));
      dummy_texpr

let synth_conversion span te ty =
  let refused = not (cast_ok te.ty ty) in
  if refused then begin
    let d =
      Diagnostic.error span "invalid conversion"
      |> Diagnostic.label "cannot convert %s to %s" (show_ty te.ty) (show_ty ty)
    in
    let d =
      if resolve_ty ty = TBool then
        Diagnostic.help "compare with zero: `x != 0`" d
      else d
    in
    Diagnostic.emit d
  end;
  if refused || has_error ty then error_expr span else mk span ty (TCast te)

(* An untyped literal takes the wanted type and checks its base *)
let check_int_lit e want target value =
  match adopt_int_lit e.espan want target false value with
  | Some typed -> typed
  | None when has_error want -> error_expr e.espan
  | None ->
      Diagnostic.emit
        (Diagnostic.error e.espan "type mismatch"
        |> Diagnostic.label "expected %s, found %s" (show_ty want)
             (show_ty (TInt I32)));
      mk e.espan (TInt I32) (TIntLit value)

let check_float_lit e want target value =
  match target with
  | TFloat _ | TError -> mk e.espan want (TFloatLit value)
  | _ when has_error want -> error_expr e.espan
  | _ ->
      Diagnostic.emit
        (Diagnostic.error e.espan "type mismatch"
        |> Diagnostic.label "expected %s, found %s" (show_ty want)
             (show_ty (TFloat F64)));
      mk e.espan (TFloat F64) (TFloatLit value)

(* A qualified callee is one symbol so it still calls directly *)
let direct_callee env callee =
  match callee.edesc with
  | Ident _ | Path _ ->
      let* symbol = Resolve.sym_at_opt env.ctx.symbols callee.espan in
      if Symbol.is_func symbol.Symbol.kind then Some symbol else None
  | _ -> None

let rec ty_of_ast env (t : typ) =
  match t.tdesc with
  | Named name -> named_ty env t.tspan (Interner.text name)
  | Pointer t -> lift_ty (fun ty -> TPointer ty) (ty_of_ast env t)
  | Array (e, t) -> array_ty_of_ast env e t
  | Slice t -> lift_ty (fun ty -> TSlice ty) (ty_of_ast env t)
  | FuncPtr (abi, ps, ret) -> (
      let pts = List.map (ty_of_ast env) ps in
      match (resolve_abi abi, ty_of_ast env ret, List.exists has_error pts) with
      | AbiError, _, _ -> TError
      | abi, rt, false when not (has_error rt) -> TFunc (pts, rt, abi)
      | _ -> TError)
  | UnitType -> TUnit

and array_ty_of_ast env size element =
  match ty_of_ast env element with
  | ty when has_error ty -> TError
  | ty -> TArray (ty, eval_array_size env size)

(* An array size may name a global not collected yet so type it now *)
and pending_global_ty env span fact =
  try
    force_deferred span fact.declared_ty TError (fun () ->
        global_ty (make_env env.ctx) fact)
  with Diagnostic.Errors ds ->
    List.iter Diagnostic.emit ds;
    TError

and lookup_var env span =
  let key = Symbol.key (sym env span) in
  match List.find_map (List.assoc_opt key) env.scopes with
  | Some t -> t
  | None -> lookup_non_local env span

(* A name outside the scopes is a global or hasn't been collected yet *)
and lookup_non_local env span =
  let key = key_at env.ctx span in
  match Symbol.Table.find_opt env.ctx.globals key with
  | Some t -> t
  | None -> (
      match Symbol.Table.find_opt env.ctx.global_facts key with
      | Some fact -> pending_global_ty env span key fact
      | None -> (
          match Symbol.Table.find_opt env.ctx.func_sigs key with
          | Some fsig -> TFunc (fsig.param_tys, fsig.ret_ty, fsig.abi)
          | None ->
              fail span
                (Diagnostic.error fspan "no field"
                |> Diagnostic.label "on struct %s" (Keyname.show sname))))

and synth_field env span te fname fspan =
  let ty = te.ty in
  match (resolve_ty ty, Interner.text fname) with
  | (TStr | TArray _ | TSlice _), "len" -> mk span (TInt Usize) (TLen te)
  | TStr, "ptr" -> mk span (TPointer (TInt U8)) (TDataPtr te)
  | (TArray (elem, _) | TSlice elem), "ptr" ->
      mk span (TPointer elem) (TDataPtr te)
  | (TStr | TArray _ | TSlice _), _ ->
      fail span
        (Diagnostic.error fspan "no field"
        |> Diagnostic.label "on %s" (show_ty ty))
  | TPtr, _ ->
      fail span
        (Diagnostic.error span "ptr has no fields"
        |> Diagnostic.help "cast to a typed pointer")
  | _, _ -> synth_struct_field env span te ty fname fspan

(* This pass does the bidirectional type checking *)

(* A while true with no break never ends *)
and synth env e =
  match e.edesc with
  | Int (n, suf) ->
      let kind = match suf with Some s -> suffix_kind s | None -> I32 in
      check_int_literal e (TInt kind) (TInt kind) n
  | UnOp (Neg, { edesc = Int (n, Some s); _ }) ->
      let kind = suffix_kind s in
      check_neg_int_literal env e (TInt kind) (TInt kind) n
  | Float (f, suf) ->
      let kind = match suf with Some s -> float_suffix_kind s | None -> F64 in
      mk e.espan (TFloat kind) (TFloatLit f)
  | Bool b -> mk e.espan TBool (TBoolLit b)
  | Null -> mk e.espan TNull TNullLit
  | String s -> mk e.espan (TPointer (TInt I8)) (TCStrLit s)
  | Char c -> mk e.espan TChar (TCharLit c)
  | Ident _ -> (
      let s = sym env e.espan in
      if s.Symbol.kind = Symbol.Error then dummy_texpr
      else
        let t = lookup_var env e.espan in
        mk t (TIdent s)
  | Call (callee, args) -> synth_call env e.espan callee args
  | BinOp (op, l, r) -> synth_binop env e.espan op l r
  | Assign (base, l, r) -> synth_assign env e.espan base l r
  | UnOp (op, operand) -> synth_unop env e.espan op operand
  | Path p -> synth_path env e.espan p
  | FieldAccess (inner_e, { value = fname; span = fspan }) ->
      synth_field env e.espan (synth env inner_e) fname fspan
  | Cast (t, inner) ->
      synth_conversion e.espan (synth env inner) (ty_of_ast env t)
  | SizeOf t -> (
      match ty_of_ast env t with
      | TError -> error_expr e.espan
      | ty -> mk e.espan (TInt Usize) (TSizeOf ty))
  | Range _ | RangeInclusive _ | RangeFrom _ | RangeTo _ | RangeToInclusive _
  | RangeFull ->
      Diagnostic.emit
        (Diagnostic.error e.espan "range is only valid in a for loop or slice");
      dummy_texpr
  | ArrayLit [] ->
      Diagnostic.emit
        (Diagnostic.error e.espan "cannot infer type of empty array literal");
      dummy_texpr
  | ArrayLit (first :: rest) -> synth_array_lit env first rest
  | Index (base, idx) -> synth_index env e.espan base idx
  | StructLit (Some { value = name; span = name_span }, inits) ->
      synth_struct_lit env e.espan name name_span inits
  | Block body ->
      let tb, ty = check_scoped_block env e.espan body Infer in
      mk e.espan ty (TBlock tb)
  | If (branches, else_body) -> check_if env e.espan branches else_body None
  | Match (scrutinee, arms) -> check_match env e.espan scrutinee arms Infer
  | For (label, { span = nspan; _ }, iter, body) ->
      synth_for env e.espan label nspan iter body
  | Binding ({ binder = RunTime; _ } as b) -> snd (check_binding env b)
  | Binding { binder = CompileTime; _ } -> mk e.espan TUnit TLocalDecl
  | Lambda l -> check_lambda env e l None
  | StructType _ | EnumType _ -> mk e.espan TType TTypeExpr
  | PointerType _ | FuncType _ | ArrayType _ | SliceType _ ->
      if has_error (ty_of_ast env e) then error_expr e.espan
      else mk e.espan TType TTypeExpr
  | Return init -> synth_return env e.espan init
  | Break (label, value) -> synth_break env e.espan label value
  | Continue label ->
      ignore (find_loop_or_error env e.espan "`continue` outside a loop" label);
      mk TNever (TContinue label)
  | Loop (label, body) -> check_loop_expr env e.espan label body None
  | Unit -> mk TUnit TUnitLit

and synth_path_type env span p =
  let inner = owner_expr p in
  let { value = name; span = name_span } = p.member in
  match type_item env.ctx (key_at env.ctx inner.espan) with
  | Some item -> (
      let sym = Resolve.sym_at env.ctx.symbols inner.espan in
      Diagnostic.emit
        (Diagnostic.error inner.espan "expected a value"
        |> Diagnostic.label "this names %s"
             (Symbol.describe_kind sym.Symbol.kind));
      dummy_texpr
  | None -> synth_field env span inner name name_span

and synth_path env span p =
  match Resolve.sym_at_opt env.ctx.symbols span with
  | Some symbol when symbol.Symbol.kind = Symbol.Error -> error_expr span
  | Some symbol
    when Symbol.is_func symbol.Symbol.kind
         || Symbol.is_global symbol.Symbol.kind ->
      mk span (find_var env span) (TIdent symbol)
  | Some _ | None -> (
      match synth_value_path env p with
      | Some typed -> typed
      | None -> synth_path_type env span p)

and synth_struct_lit env span name name_span inits =
  match Symbol.Table.find_opt env.ctx.type_defs (key_at env.ctx name_span) with
  | Some (Struct_type { contents = Completed info }) ->
      let fields =
        match inits with
        | ({ value = None; _ }, _) :: _ -> positional_fields env span info inits
        | _ -> named_fields env span info inits
      in
      let keyname = keyname_at env.ctx name_span (Interner.text name) in
      mk (TStruct (keyname, [])) (TStructLit (keyname, fields))
  | Some (Struct_type { contents = Unstarted | Running }) ->
      Diagnostic.emit (Diagnostic.error name_span "undefined struct");
      dummy_texpr
  | Some (Enum_type _ | Builtin_type _ | Alias_type _) | None ->
      let d =
        match Resolve.sym_at_opt env.ctx.symbols name_span with
        | Some sym ->
            Diagnostic.error name_span "expected a struct"
            |> Diagnostic.label "this names %s"
                 (Symbol.describe_kind sym.Symbol.kind)
        | None -> Diagnostic.error name_span "undefined struct"
      in
      Diagnostic.emit d;
      dummy_texpr

(* A quiet probe lets a sibling anchor the block type *)
and block_result_ty env body =
  let quiet = probing env in
  let inner = push_scope quiet in
  let tb =
    Diagnostic.quietly (fun () -> check_block inner dummy_span body Infer)
  in
  tblock_ty tb

and coerce_common env first rest =
  let add_typed common = function
    | Typed (_, typed) -> common_ty common typed.ty
    | Contextual _ -> common
  in
  let common = List.fold_left add_typed (add_typed TNever first) rest in
  let common, first =
    if not (is_never common) then (common, first)
    else
      match first with
      | Contextual source ->
          let typed = synth env source in
          (typed.ty, Typed (source, typed))
      | Typed (_, typed) -> (typed.ty, first)
  in
  let coerce = function
    | Contextual source -> check env source common
    | Typed (source, typed) -> coerce_expr source common typed
  in
  (coerce first :: List.map coerce rest, common)

(* The operator alone reports a type it rejects *)
and coerce_common_pair env contextual accepts left right =
  let against typed e =
    if is_never typed.ty || not (accepts typed.ty) then synth env e
    else check env e typed.ty
  in
  if contextual left && not (contextual right) then
    let typed_right = synth env right in
    let typed_left = against typed_right left in
    (typed_left, typed_right, common_ty typed_right.ty typed_left.ty)
  else if contextual right then
    let typed_left = synth env left in
    let typed_right = against typed_left right in
    (typed_left, typed_right, common_ty typed_left.ty typed_right.ty)
  else
    let typed_left = synth env left in
    let typed_right = synth env right in
    let common = common_ty typed_left.ty typed_right.ty in
    if accepts common then
      ( coerce_expr left common typed_left,
        coerce_expr right common typed_right,
        common )
    else (typed_left, typed_right, common)

and synth_array_lit env span first rest =
  let probe e =
    if arm_is_flexible e then Contextual e else Typed (e, synth env e)
  in
  let tes, elem = coerce_common env (probe first) (List.map probe rest) in
  let elem =
    match elem with
    | (TUnit | TNever) as t ->
        Diagnostic.emit
          (Diagnostic.error first.espan "invalid element type"
          |> Diagnostic.label "on %s" (show_ty t));
        TError
    | t -> t
  in
  let ty = lift_ty (fun t -> TArray (t, List.length tes)) elem in
  mk span ty (TArrayLit tes)

(* A field left out is zero init *)
and named_fields env span variant inits =
  let seen = Hashtbl.create 4 in
  let check_named_field ({ value = fname; span = fspan }, e) =
    let* fname = fname in
    match find_field variant fname with
    | None ->
        Diagnostic.emit (Diagnostic.error fspan "no field");
        None
    | Some _ when Hashtbl.mem seen fname ->
        Diagnostic.emit (Diagnostic.error fspan "duplicate field");
        None
    | Some (field_id, ft) ->
        Hashtbl.replace seen fname ();
        Some (field_id, check env e ft)
  in
  let written_fields = List.filter_map check_named_field inits in
  let default_field field_id (fname, ft) =
    if Option.exists (Hashtbl.mem seen) fname then None
    else Some (field_id, mk span (zero_init_ty env span ft) TZero)
  in
  let rec defaults field_id = function
    | [] -> []
    | field :: rest -> (
        let rest = defaults (field_id + 1) rest in
        match default_field field_id field with
        | Some entry -> entry :: rest
        | None -> rest)
  in
  written_fields @ defaults 0 variant.field_tys

(* A positional literal must define every field *)
and positional_fields env span variant inits =
  let expected = List.length variant.field_tys in
  let found = List.length inits in
  if found <> expected then
    Diagnostic.emit
      (Diagnostic.error span "wrong number of fields"
      |> Diagnostic.label "expected %d, found %d" expected found);
  let rec zip field_id fields inits =
    match (fields, inits) with
    | [], inits ->
        walk_unpaired_args env (List.map (fun (_, init) -> init) inits);
        []
    | (_, ft) :: fields, [] ->
        (field_id, mk span (zero_init_ty env span ft) TZero)
        :: zip (field_id + 1) fields []
    | (_, ft) :: fields, (_, init) :: inits ->
        (field_id, check env init ft) :: zip (field_id + 1) fields inits
  in
  zip 0 variant.field_tys inits

(* A literal bends to the common rigid type so it doesn't anchor it *)
and reconcile_arms bodies =
  let add_candidate (rigid, flexible) (env, body) =
    let candidate = block_result_ty env body in
    if block_is_flexible body then (rigid, common_ty flexible candidate)
    else (common_ty rigid candidate, flexible)
  in
  let rigid, flexible = List.fold_left add_candidate (TNever, TNever) bodies in
  if is_never rigid then flexible else rigid

and check_value_for_use env e use =
  match (use, e.edesc) with
  | Infer, _ -> synth env e
  | Expect want, _ -> check env e want
  | Discard, If (branches, else_body) ->
      check_if_discarded env e.espan branches else_body
  | Discard, Match (scrutinee, arms) ->
      check_match env e.espan scrutinee arms Discard
  | Discard, _ -> synth env e

(* The env threads through so later lines see a binding *)
and check_block env span body use =
  let rec go env diverged acc = function
    | [] ->
        verify_unit_result span use;
        List.rev acc
    | [ last ] ->
        let tail_use = if diverged then Infer else use in
        let _, te = check_elem env last tail_use in
        List.rev (te :: acc)
    | e :: rest ->
        let elem_use = if diverged then Infer else Discard in
        let env, te = check_elem env e elem_use in
        go env (diverged || is_never te.ty) (te :: acc) rest
  in
  go env false [] body

and check_elem env e use : local_env * texpr =
  match e.edesc with
  | Binding { binder = CompileTime; _ } ->
      verify_unit_result e.espan use;
      (env, mk e.espan TUnit TLocalDecl)
  | Binding ({ binder = RunTime; _ } as b) ->
      let env', tbind = check_binding env b in
      if not (is_never tbind.ty) then verify_unit_result e.espan use;
      (env', tbind)
  | _ -> (env, check_value_for_use env e use)

and check_scoped_block env span body use =
  let tb = check_block (push_scope env) span body use in
  (tb, tblock_ty tb)

(* An init that diverges makes the binding itself dead so it carries never *)
and check_binding env b =
  let nspan = b.bind_name.span in
  let t, te =
    match b.bind_form with
    | Typed (a, e) ->
        let want = written_want env a e in
        if want = TType then (TError, type_as_value e)
        else (want, check env e want)
    | Inferred e ->
        let te = synth env e in
        let te = if te.ty = TType then type_as_value e else te in
        (te.ty, te)
    | Declared a ->
        let want = ty_of_ast env a in
        let want =
          if want = TType then (type_as_value a).ty
          else zero_init_ty env a.espan want
        in
        (want, mk a.espan want TZero)
  in
  let node_ty = if is_never te.ty then TNever else TUnit in
  ( extend_var env nspan t,
    mk b.bind_span node_ty (TBinding (sym env nspan, t, te)) )

and synth_return env span init =
  if is_never env.ret_ty then
    Diagnostic.emit (Diagnostic.error span "return in a never function");
  match init with
  | None ->
      if not (is_never env.ret_ty || is_unit env.ret_ty) then
        Diagnostic.emit (Diagnostic.error span "missing return value");
      mk span TNever (TReturn None)
  | Some e when is_never env.ret_ty ->
      mk span TNever (TReturn (Some (synth env e)))
  | Some e ->
      let te = check env e env.ret_ty in
      mk span TNever (TReturn (Some te))

and check_loop_expr env span label source body want =
  let loop = new_loop label source in
  loop.result <-
    (match want with Some t -> ExpectLoopResult t | None -> InferLoopResult);
  let tb, _ =
    check_scoped_block { env with loops = loop :: env.loops } span body Discard
  in
  let ty =
    match (loop.result, loop.bare_break) with
    | FlexibleLoopResult (t, _, _), _ | RigidLoopResult (t, _, _), _ -> t
    | (InferLoopResult | ExpectLoopResult _), None -> TNever
    | InferLoopResult, Some _ -> TUnit
    | ExpectLoopResult want, Some _ when has_error want -> TError
    | ExpectLoopResult want, Some break_span ->
        Diagnostic.emit
          (Diagnostic.error break_span "type mismatch"
          |> Diagnostic.label "expected %s, found %s" (show_ty want)
               (show_ty TUnit));
        TUnit
  in
  mk span ty (TLoop tb)

(* The first value that can't bend decides the type for the rest *)
and check_valued_break env lc ve =
  let flexible = arm_is_flexible ve in
  let check_flexible_values want values =
    List.iter (fun e -> ignore (check env e want)) values
  in
  let report_bare ty =
    let report first =
      Diagnostic.emit
        (Diagnostic.error ve.espan "`break` values disagree"
        |> Diagnostic.label "breaks with %s" (show_ty ty)
        |> Diagnostic.secondary first "no value here")
    in
    Option.iter report lc.bare_break
  in
  match (lc.result, flexible) with
  | InferLoopResult, _ ->
      let typed = synth env ve in
      if not (is_never typed.ty) then begin
        report_bare typed.ty;
        lc.result <-
          (if flexible then FlexibleLoopResult (typed.ty, ve.espan, [ ve ])
           else RigidLoopResult (typed.ty, ve.espan, []))
      end;
      typed
  | ExpectLoopResult want, _ ->
      let typed = check env ve want in
      if not (is_never typed.ty) then begin
        report_bare want;
        lc.result <-
          RigidLoopResult (want, ve.espan, if flexible then [ ve ] else [])
      end;
      typed
  | FlexibleLoopResult (want, first, values), true ->
      lc.result <- FlexibleLoopResult (want, first, ve :: values);
      check env ve want
  | RigidLoopResult (want, first, values), true ->
      lc.result <- RigidLoopResult (want, first, ve :: values);
      check env ve want
  | FlexibleLoopResult (_, first, values), false ->
      let typed = synth env ve in
      if is_never typed.ty then typed
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
      coerce_expr ve common typed

and synth_break env span label value =
  let depth, target =
    find_loop_or_error env span "`break` outside a loop" label
  in
  let tv =
    match (value, target) with
    | None, None -> None
    | None, Some lc ->
        verify_bare_break span lc;
        None
    | Some ve, None -> Some (synth env ve)
    | Some ve, Some lc when not lc.valued ->
        Diagnostic.emit
          (Diagnostic.error ve.espan "`break` value outside `loop`"
          |> Diagnostic.help "use `loop` to break with a value");
        Some (synth env ve)
    | Some ve, Some lc -> Some (check_valued_break env lc ve)
  in
  mk span TNever (TBreak (depth, tv))

and synth_for env span label nspan iter body =
  let titer, elem_ty =
    match iter.edesc with
    | Range (lo, hi) ->
        let tlo, thi, t = check_range_bounds env lo hi in
        (mk iter.espan t (TRange (tlo, thi)), t)
    | RangeInclusive (lo, hi) ->
        let tlo, thi, t = check_range_bounds env lo hi in
        (mk iter.espan t (TRangeInclusive (tlo, thi)), t)
    | _ -> (
        let ti = synth env iter in
        match resolve_ty ti.ty with
        | TError -> (ti, TError)
        | TArray (elem, _) | TSlice elem -> (ti, elem)
        | t ->
            Diagnostic.emit
              (Diagnostic.error iter.espan "cannot iterate"
              |> Diagnostic.label "on %s" (show_ty t));
            (ti, TInt I32))
  in
  let loop = new_loop label ForLoop in
  let inner = push_scope { env with loops = loop :: env.loops } in
  let inner = extend_var inner nspan elem_ty in
  let tb = check_block inner span body Discard in
  mk span TUnit (TFor (sym env nspan, elem_ty, titer, tb))

(* A discarded arm checks its own trailing value *)
and check_if_discarded env span branches else_body =
  let arm body = fst (check_scoped_block env span body Discard) in
  let tbranches =
    List.map
      (fun (c, { value = body; _ }) -> (check env c TBool, arm body))
      branches
  in
  let telse = Option.map (fun { value = body; _ } -> arm body) else_body in
  let ty = if_result_ty tbranches telse TUnit in
  mk span ty (TIf (tbranches, telse))

(* A probe only wants the type so we don't build the tree twice *)
and check_if env span branches else_body want =
  match (want, else_body) with
  | None, None -> check_if_discarded env span branches None
  | None, Some { value = else_b; _ } ->
      let rty =
        reconcile_arms
          (List.map (fun (_, { value; _ }) -> (env, value)) branches
          @ [ (env, else_b) ])
      in
      if is_probing env then mk span rty TErrorExpr
      else check_if env span branches else_body (Some rty)
  | Some w, _ ->
      let tbranches =
        List.map
          (fun (c, { value = body; span = bspan }) ->
            ( check env c TBool,
              fst (check_scoped_block env bspan body (Expect w)) ))
          branches
      in
      let telse =
        match else_body with
        | Some { value = body; span = bspan } ->
            Some (fst (check_scoped_block env bspan body (Expect w)))
        | None ->
            if not (is_unit w || has_error w) then
              Diagnostic.emit
                (Diagnostic.error span "type mismatch"
                |> Diagnostic.label "expected %s, found %s" (show_ty w)
                     (show_ty TUnit));
            None
      in
      let ty = if_result_ty tbranches telse w in
      mk span ty (TIf (tbranches, telse))

(* A binding lands in the scope of the arm so the env comes back out *)
and check_pattern env sty pat =
  match pat.pdesc with
  | PatWild -> (env, Some TPatWild)
  | PatBind _ ->
      let symbol = sym env pat.pspan in
      (extend_var env pat.pspan sty, Some (TPatBind (symbol, sty)))
  | PatValue e -> (
      let te = check env e sty in
      (* TODO(43f6): comparing these needs more than the integer test an arm emits *)
      let not_comparable () =
        Diagnostic.emit
          (Diagnostic.error pat.pspan "pattern is not comparable"
          |> Diagnostic.label "cannot test %s" (show_ty te.ty));
        (env, None)
      in
      (* TODO(766b): A range should work as a pattern *)
      let not_a_lit () =
        Diagnostic.emit
          (Diagnostic.error pat.pspan "pattern is not a literal"
          |> Diagnostic.help "use a literal or a variant");
        (env, None)
      in
      match te.tdesc with
      | _ when not (Typred.compatible sty te.ty) -> (env, None)
      | TVariant (_, value) -> (env, Some (TPatConst value))
      | TIntLit n -> (env, Some (TPatConst n))
      | TBoolLit b -> (env, Some (TPatConst (if b then 1L else 0L)))
      | TCharLit c -> (env, Some (TPatConst (Int64.of_int c)))
      | TErrorExpr -> (env, None)
      | TFloatLit _ | TStrLit _ | TCStrLit _ -> not_comparable ()
      | _ -> not_a_literal ())

(* An arm body can name its binder so the probe needs it in scope *)
and check_match env span scrutinee arms use =
  let ts = synth env scrutinee in
  let arm_env a =
    match a.pat.pdesc with
    | PatBind _ -> extend_var (push_scope env) a.pat.pspan ts.ty
    | _ -> env
  in
  let bodies = List.map (fun a -> (arm_env a, a.arm_body.value)) arms in
  match use with
  | Infer when is_probing env -> mk span (reconcile_arms bodies) TErrorExpr
  | Infer -> check_match_arms env span ts arms (Some (reconcile_arms bodies))
  | Expect w -> check_match_arms env span ts arms (Some w)
  | Discard -> check_match_arms env span ts arms None

(* An alias can't spell its own variants so we name the enum itself *)
and coverage_of env ty seen =
  let absent value name rest =
    if Hashtbl.mem seen value then rest else (value, name) :: rest
  in
  let in_declared_order cases =
    match List.map snd (List.sort compare cases) with
    | [] -> Covered
    | missing -> Missing missing
  in
  match resolve_ty ty with
  | TError -> Unknown
  | TBool -> in_declared_order (absent 0L "false" (absent 1L "true" []))
  | TEnum name -> (
      match Symbol.Table.find_opt env.ctx.type_defs (Keyname.key name) with
      | Some (Enum_type { contents = Completed info }) ->
          (* An alias can't spell its own variants so name the enum itself *)
          let shown = show_ty (TEnum name) in
          let gather variant value rest =
            absent value (shown ^ "." ^ Interner.text variant) rest
          in
          in_declared_order (List.fold_right gather variants [])
      | Some { kind = Struct; _ } -> Unbounded
      | None -> Unknown)
  | _ -> Unbounded

and report_coverage ts = function
  | Covered | Unknown -> ()
  | Missing names ->
      let fix =
        match names with
        | [ one ] -> Printf.sprintf "add `%s => { }` or `_ => { }`" one
        | _ -> "add the arms or `_ => { }`"
      in
      Diagnostic.emit
        (Diagnostic.error ts.tspan "match is not exhaustive"
        |> Diagnostic.label "%s not covered" (String.concat ", " names)
        |> Diagnostic.help fix)
  | Unbounded ->
      Diagnostic.emit
        (Diagnostic.error ts.tspan "match is not exhaustive"
        |> Diagnostic.label "on %s" (show_ty ts.ty)
        |> Diagnostic.help "add `_ => { }`")

(* An arm lost to an earlier error looks like a hole so we skip coverage *)
and check_match_arms env span ts arms want =
  let arm_use = match want with Some w -> Expect w | None -> Discard in
  let seen = Hashtbl.create 8 in
  let caught_all = ref false in
  let record pat tpat =
    let mark n =
      if Hashtbl.mem seen n then
        Diagnostic.emit (Diagnostic.error pat.pspan "duplicate pattern")
      else Hashtbl.add seen n ()
    in
    if !caught_all then
      Diagnostic.emit (Diagnostic.error pat.pspan "arm never runs")
    else
      match tpat with
      | TPatWild | TPatBind _ -> caught_all := true
      | TPatConst n ->
          if Hashtbl.mem seen n then
            Diagnostic.emit (Diagnostic.error pat.pspan "duplicate pattern")
          else Hashtbl.add seen n ()
  in
  let check_arm a =
    let arm_env, tpat = check_pattern (push_scope env) ts.ty a.pat in
    Option.iter (record a.pat) tpat;
    let tbody, _ =
      check_scoped_block arm_env a.arm_body.span a.arm_body.value arm_use
    in
    Option.map (fun tpat -> { tpat; tbody }) tpat
  in
  let tarms = List.filter_map check_arm arms in
  let arms_all_arrived =
    List.compare_lengths tarms arms = 0 && not env.ctx.initial_errors
  in
  if (not !caught_all) && arms_all_arrived then
    report_coverage ts (coverage_of env ts.ty seen);

  let diverges =
    !caught_all && List.for_all (fun a -> is_never (tblock_ty a.tbody)) tarms
  in
  let ty =
    match (diverges, want) with
    | true, _ -> TNever
    | false, Some w -> w
    | false, None -> TUnit
  in
  mk span ty (TMatch (ts, tarms))

(* A type value that isn't written out needs compile time eval to read it *)
and value_as_type env t =
  if (check env t TType).ty = TType then
    Diagnostic.emit (Diagnostic.error t.espan "invalid type");
  TError

and check_by_synth env e want =
  let typed = synth env e in
  coerce_expr e want typed

and check_size_literal env (e : expr) want target typ =
  match ty_of_ast env typ with
  | TError -> error_expr e.espan
  | ty ->
      let size = Int64.of_int (Layout.ty_size env.ctx.layouts ty) in
      (match target with
      | TInt kind when not (int_kind_fits kind false size) ->
          Diagnostic.emit
            (Diagnostic.error e.espan "size does not fit"
            |> Diagnostic.label "%Ld does not fit in %s" size (show_ty target))
      | _ -> ());
      mk e.espan want (TSizeOf ty)

and check_neg_int_lit env e want target value =
  match adopt_int_lit e.espan want target true value with
  | Some typed -> typed
  | None -> check env { e with edesc = Int (Int64.neg value, None) } want

and check_array_lit env e want elems =
  match (resolve_ty want, elems) with
  | TArray (elem, len), _ ->
      if List.compare_length_with elems len <> 0 then
        Diagnostic.emit
          (Diagnostic.error e.espan "wrong number of arguments"
          |> Diagnostic.label "expected %d elements, found %d" len
               (List.length elems));
      let typed = List.map (fun source -> check env source elem) elems in
      mk e.espan (TArray (elem, len)) (TArrayLit typed)
  | TSlice elem, _ ->
      let typed = List.map (fun source -> check env source elem) elems in
      let array = TArray (elem, List.length typed) in
      coerce_expr e want (mk e.espan array (TArrayLit typed))
  | TError, _ ->
      List.iter (fun source -> ignore (check env source TError)) elems;
      error_expr e.espan
  | _, [] ->
      fail e.espan
        (Diagnostic.error e.espan "type mismatch"
        |> Diagnostic.label "expected %s, found [0]_" (show_ty want))
  | _ -> check_by_synth env e want

and check env e want =
  let target = resolve_ty want in
  match e.edesc with
  | Int (value, None) -> check_int_lit e want target value
  | Float (value, None) -> check_float_lit e want target value
  | String value when target = TStr -> mk e.espan want (TStrLit value)
  | String _ -> check_by_synth env e want
  | SizeOf typ when is_integer want -> check_size_literal env e want target typ
  | UnOp (Neg, { edesc = Int (value, None); _ }) ->
      check_neg_int_literal env e want target value
  | UnOp (Neg, { edesc = Float (f, suf); _ }) ->
      check env { e with edesc = Float (-.f, suf) } want
  | UnOp (Neg, { edesc = Int (_, Some _); _ }) -> check_by_synth env e want
  | UnOp (((Neg | BitNot) as op), operand) when unop_accepts op want ->
      mk want (TUnOp (op, check env operand want))
  | ArrayLit elements -> check_array_literal env e want elements
  | BinOp (op, l, r) -> (
      match op.value with
      | (Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor)
        when binop_accepts op.value want ->
          mk e.espan want
            (TBinOp (op.value, check env l want, check env r want))
      | (Lshift | Rshift) when is_integer want ->
          let base = check env l want in
          let count = synth env r in
          verify_shift_count r.espan count;
          mk e.espan want (TBinOp (op.value, base, count))
      | _ -> check_by_synth env e want)
  | Block body ->
      let tb, ty = check_scoped_block env e.espan body (Expect want) in
      mk e.espan ty (TBlock tb)
  | If (branches, else_body) ->
      check_if env e.espan branches else_body (Some want)
  | Match (scrutinee, arms) ->
      check_match env e.espan scrutinee arms (Expect want)
  | Loop (label, PlainLoop, body) ->
      check_loop_expr env e.espan label PlainLoop body (Some want)
  | Lambda l -> check_lambda env e l (Some want)
  | Cast (t, inner) when has_hole t ->
      synth_conversion e.espan (synth env inner) (fill_ty env t want)
  | _ -> check_by_synth env e want

and coerce_expr e want te =
  let got = te.ty in
  if compatible want got then adopt want te
  else if widens_to got want then mk e.espan want (TCast te)
  else begin
    let mismatch =
      Diagnostic.error e.espan "type mismatch"
      |>
      match (resolve_ty want, resolve_ty got) with
      | TPtr, (TPointer _ | TCStr) | (TPointer _ | TCStr), TPtr ->
          Diagnostic.label "%s needs a cast to %s" (show_ty got) (show_ty want)
      | _ ->
          Diagnostic.label "expected %s, found %s" (show_ty want) (show_ty got)
    in
    let mismatch =
      match (e.edesc, resolve_ty want) with
      | Assign ({ value = None; _ }, _, _), TBool ->
          Diagnostic.help "use `==` to compare" mismatch
      | _ -> mismatch
    in
    Diagnostic.emit mismatch;
    te
  end

and check_comparison_operands env accepts l r =
  let is_contextual_literal e =
    is_num_lit e || match e.edesc with String _ -> true | _ -> false
  in
  coerce_common_pair env is_contextual_literal accepts l r

and check_range_bounds env lo hi =
  let tlo, thi, t = coerce_common_pair env is_num_lit is_integer lo hi in
  if is_integer t then (tlo, thi, t)
  else begin
    let bad, tbad = if is_integer tlo.ty then (hi, thi) else (lo, tlo) in
    Diagnostic.emit
      (Diagnostic.error bad.espan "invalid range bound"
      |> Diagnostic.label "expected integer, found %s" (show_ty tbad.ty));
    (tlo, thi, TError)
  end

(* A bad count drops the args so walk them or their errors never show *)
and walk_unpaired_args env args = List.iter (fun a -> ignore (synth env a)) args

(* C reads a float vararg as a double so we widen it first *)
and check_args env span fsig args =
  let f = fsig.func in
  let n_params = List.length f.sig_params in
  let n_args = List.length args in
  let expected_args =
    Printf.sprintf "%d argument%s" n_params (if n_params = 1 then "" else "s")
  in
  if fsig.param_hole then begin
    walk_unpaired_args env args;
    []
  end
  else if fsig.variadic then
    if n_args < n_params then begin
      Diagnostic.emit
        (Diagnostic.error span "wrong number of arguments"
        |> Diagnostic.label "expected at least %s, found %d" expected_args
             n_args);
      walk_unpaired_args env args;
      []
    end
    else
      let fixed = List.take n_params args in
      let rest = List.drop n_params args in
      let promote_vararg e =
        let te = synth env e in
        match resolve_ty te.ty with
        | TFloat F32 -> mk e.espan (TFloat F64) (TCast te)
        | _ -> te
      in
      List.map2 (check env) fixed f.sig_params @ List.map promote_vararg rest
  else if n_params <> n_args then begin
    Diagnostic.emit
      (Diagnostic.error span "wrong number of arguments"
      |> Diagnostic.label "expected %s, found %d" expected_args n_args);
    walk_unpaired_args env args;
    []
  end
  else List.map2 (check env) args f.sig_params

and synth_binop env span { value = op; span = op_span } l r =
  match op with
  | Add | Sub | Mul | Div | Mod | BitAnd | BitOr | BitXor ->
      let tl, tr, t = check_matching_operands env (binop_accepts op) l r in
      mk (verify_operands op_span op t [ tl.ty; tr.ty ]) (TBinOp (op, tl, tr))
  | Lshift | Rshift ->
      let tl = synth env l in
      let tr = synth env r in
      let t = verify_operands op_span op tl.ty [ tl.ty ] in
      verify_shift_count r.espan tr;
      mk span t (TBinOp (op, tl, tr))
  | Eq | Neq | Lt | Gt | Lte | Gte ->
      let tl, tr, t = check_comparison_operands env (binop_accepts op) l r in
      let ty =
        if has_error (verify_operands op_span op t [ tl.ty; tr.ty ]) then TError
        else TBool
      in
      mk span ty (TBinOp (op, tl, tr))
  | And | Or ->
      let tl = check env l TBool in
      let tr = check env r TBool in
      mk span TBool (TBinOp (op, tl, tr))

(* The caller reading this back must not be told it went fine *)
and synth_assign env span { value = base; span = op_span } l r =
  let tl, tr = check_assign_operands env base op_span l r in
  let ty = if is_poisoned tl || is_poisoned tr then TError else TUnit in
  mk span ty (TAssign (base, tl, tr))

and check_assign_operands env base op_span l r =
  let tl = synth env l in
  let no_place = tl.ty <> TError && not (is_lvalue tl) in
  if no_place then
    Diagnostic.emit
      (if tl.ty = TType then Diagnostic.error l.espan "cannot assign to type"
       else
         Diagnostic.error l.espan "cannot assign to expression"
         |> Diagnostic.label "on %s" (show_ty tl.ty));
  (match tl.tdesc with
  | TIdent s when Symbol.is_func s.Symbol.kind ->
      Diagnostic.emit (Diagnostic.error l.espan "cannot assign to function")
  | TIdent _ | TFieldAccess _ | TIndex _ -> (
      match root_binding tl with
      | Some s when Symbol.is_immutable s.Symbol.kind ->
          Diagnostic.emit
            (Diagnostic.error l.espan "cannot assign to immutable")
      | _ -> ())
  | _ -> ());
  let t =
    if no_place then TError
    else
      match base with
      | Some op -> verify_operands op_span op tl.ty [ tl.ty ]
      | None -> tl.ty
  in
  let tr =
    match base with
    | Some (Lshift | Rshift) ->
        let tr = synth env r in
        verify_shift_count r.espan tr;
        tr
    | _ -> check env r t
  in
  (tl, tr)

and synth_unop env span op e =
  match op with
  | Neg | BitNot ->
      let te = synth env e in
      let t = te.ty in
      if not (unop_accepts op t) then
        Diagnostic.emit
          (Diagnostic.error e.espan "invalid operand"
          |> Diagnostic.label "cannot apply `%s` to %s" (show_unop_sym op)
               (show_ty t));
      mk span t (TUnOp (op, te))
  | Not ->
      let te = check env e TBool in
      mk span TBool (TUnOp (op, te))
  | Deref -> (
      let te = synth env e in
      match resolve_ty te.ty with
      | TPointer inner -> mk span inner (TUnOp (op, te))
      | t when has_error t -> error_expr span
      | TPtr ->
          fail span
            (Diagnostic.error e.espan "cannot dereference ptr"
            |> Diagnostic.help "cast to a typed pointer")
      | t ->
          fail span
            (Diagnostic.error e.espan "cannot dereference"
            |> Diagnostic.label "on %s" (show_ty t)))
  | AddressOf ->
      let te = synth env e in
      let te = if te.ty = TType then type_as_value e else te in
      if te.ty <> TError && not (is_lvalue te) then
        Diagnostic.emit (Diagnostic.error e.espan "cannot take address");
      mk span (lift_ty (fun ty -> TPointer ty) te.ty) (TUnOp (op, te))

and synth_value_path env p =
  let root_span = (Nonempty.hd p.owner).span in
  match Resolve.sym_at_opt env.ctx.symbols root_span with
  | Some { Symbol.kind = Symbol.Type | Symbol.LocalType; _ } | None -> None
  | Some symbol ->
      let root =
        match symbol.Symbol.kind with
        | Symbol.Error -> error_expr root_span
        | _ -> mk root_span (find_var env root_span) (TIdent symbol)
      in
      let fields = List.tl (Nonempty.to_list p.owner) @ [ p.member ] in
      let step typed { value = field_name; span = field_span } =
        let span = Span.make (Span.lo root_span) (Span.hi field_span) in
        synth_field env span typed field_name field_span
      in
      Some (List.fold_left step root fields)

and synth_call env span callee args =
  match
    (direct_callee env callee, type_item env.ctx (key_at env.ctx callee.espan))
  with
  | Some func_sym, _ ->
      let fsig = find_func env callee.espan in
      let targs = check_args env span fsig args in
      let callee_texpr = mk callee.espan (TFunc fsig.func) (TIdent func_sym) in
      mk span fsig.func.sig_ret
        (TCall (callee_texpr, targs, fixed_count fsig.func))
  | None, Some _ ->
      let sym = Resolve.sym_at env.ctx.symbols callee.espan in
      Diagnostic.emit
        (Diagnostic.error callee.espan "expected a function"
        |> Diagnostic.label "this names %s"
             (Symbol.describe_kind sym.Symbol.kind));
      List.iter (fun a -> ignore (synth env a)) args;
      error_expr span
  | None, None -> apply_callee env span callee (synth env callee) args

(* The callee is a value holding a fn ptr so call through it *)
and apply_callee env span callee callee_texpr args =
  match resolve_ty callee_texpr.ty with
  | TError -> dummy_texpr
  | TFunc (param_tys, ret_ty, abi) ->
      let fsig =
        { param_tys; ret_ty; variadic = false; abi; param_hole = false }
      in
      let targs = check_args env span fsig args in
      mk ret_ty (TCall (callee_texpr, targs, None))
  | _ ->
      let d =
        Diagnostic.error callee.espan "not callable"
        |> Diagnostic.found (show_ty callee_texpr.ty)
      in
      let d =
        match Resolve.shadowed_at env.ctx.symbols callee.espan with
        | Some sym ->
            d
            |> Diagnostic.secondary sym.Symbol.name_span "shadowed by the value"
            |> Diagnostic.help "rename the value"
        | None -> d
      in
      fail span d

(* An inclusive end desugars to one past *)
and synth_index env span base idx =
  let tbase = synth env base in
  match resolve_ty tbase.ty with
  | TArray (elem, _) | TSlice elem -> (
      let zero = mk idx.espan (TInt Usize) (TIntLit 0L) in
      let whole_len = mk idx.espan (TInt Usize) (TLen tbase) in
      let one_past te =
        mk te.tspan te.ty (TBinOp (Add, te, mk te.tspan te.ty (TIntLit 1L)))
      in
      let slice tlo thi =
        mk span (TSlice elem) (TSliceExpr (tbase, tlo, thi))
      in
      let bound e = check env e (TInt Usize) in
      match idx.edesc with
      | Range (lo, hi) ->
          let tlo, thi, _ = check_range_bounds env lo hi in
          slice tlo thi
      | RangeInclusive (lo, hi) ->
          let tlo, thi, _ = check_range_bounds env lo hi in
          slice tlo (one_past thi)
      | RangeFrom lo -> slice (bound lo) whole_len
      | RangeTo hi -> slice zero (bound hi)
      | RangeToInclusive hi -> slice zero (one_past (bound hi))
      | RangeFull -> slice zero whole_len
      | _ ->
          let tidx = synth env idx in
          if not (is_integer tidx.ty || is_never tidx.ty) then
            Diagnostic.emit
              (Diagnostic.error idx.espan "invalid index"
              |> Diagnostic.label "expected integer, found %s" (show_ty tidx.ty)
              );
          mk span elem (TIndex (tbase, tidx)))
  | TError -> error_expr span
  | TPtr ->
      fail span
        (Diagnostic.error span "cannot index ptr"
        |> Diagnostic.help "cast to a typed pointer")
  | t ->
      fail span
        (Diagnostic.error span "cannot index"
        |> Diagnostic.label "on %s" (show_ty t))

(* The init is what an unannotated global gets its type from *)
and global_ty env fact =
  match fact.decl.bind_form with
  | Declared t ->
      let ty = ty_of_ast env t in
      if ty = TType then (type_as_value t).ty else zero_init_ty env t.espan ty
  | Typed (t, e) when not (has_hole t) ->
      let ty = ty_of_ast env t in
      if ty = TType then (type_as_value e).ty else ty
  | Typed (_, e) | Inferred e ->
      let te =
        try global_typed_init env e.espan fact
        with Diagnostic.Errors ds ->
          List.iter Diagnostic.emit ds;
          error_expr e.espan
      in
      te.ty

(* The running initializer state catches recursive demand *)
and global_typed_init env span fact =
  force_deferred span fact.typed (error_expr span) (fun () ->
      type_global_init (make_env env.ctx) span fact.decl)

and type_global_init env span b =
  match b.bind_form with
  | Typed (t, e) ->
      let want = written_want env t e in
      if want = TType then error_expr e.espan else check env e want
  | Inferred e ->
      let te = synth env e in
      if te.ty = TType then type_as_value e else te
  | Declared _ -> Diagnostic.ice ~span "global has no initializer"

and eval_array_size env e =
  let te = synth env e in
  match te.tdesc with
  | _ when not (is_integer te.ty) -> bad "array size must be an integer"
  | TIntLit n ->
      let shown =
        if is_unsigned te.ty then Printf.sprintf "%Lu" n else Int64.to_string n
      in
      if
        Int64.compare n 0x7FFF_FFFFL > 0
        || (Int64.compare n 0L < 0 && is_unsigned te.ty)
      then bad ("array size is too large: " ^ shown)
      else if Int64.compare n 0L < 0 then
        bad ("array size is negative: " ^ shown)
      else Int64.to_int n
  | _ when has_error te.ty -> 0
  | _ -> bad "array size must be a literal"

let ret_ty_of env fd =
  match fd.ret with Some t -> ty_of_ast env t | None -> TUnit

(* The signature pass enables forward calls *)
let collect_func env fd =
  let abi = resolve_abi fd.extern_abi in
  let param_tys = List.map (fun p -> ty_of_ast env p.param_typ) fd.params in
  let ret_ty = ret_ty_of env fd in
  Symbol.Table.replace env.ctx.func_sigs
    (key_at env.ctx fd.func_span)
    { param_tys; ret_ty; variadic = fd.variadic; abi; param_hole = false }

(* A repeat name is already reported with both spans by the resolver *)
let type_name_taken ctx span =
  let key = key_at ctx span in
  key = Symbol.unresolved_key || Symbol.Table.mem ctx.type_defs key

(* The reserved name enables forward field types *)
let reserve_struct_name ctx sd =
  if not (type_name_taken ctx sd.struct_span) then begin
    let seen = Hashtbl.create 8 in
    let check_duplicate f =
      if Hashtbl.mem seen f.field_name.value then
        Diagnostic.emit (Diagnostic.error f.field_name.span "duplicate field")
      else Hashtbl.add seen f.field_name.value ()
    in
    List.iter check_duplicate sd.fields;
    Symbol.Table.replace ctx.type_defs
      (key_at ctx sd.struct_span)
      (Struct_type (ref Unstarted));
    Layout.set_struct_fields ctx.layouts (key_at ctx sd.struct_span) []
  end

let fill_struct_fields env sd =
  match
    Symbol.Table.find_opt env.ctx.type_defs (key_at env.ctx sd.struct_span)
  with
  | Some (Struct_type body) ->
      let named_ty f = (f.field_name.value, ty_of_ast env f.field_typ) in
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
        (key_at env.ctx sd.struct_span)
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
  let size =
    match te.tdesc with
    | _ when not (is_integer te.ty) ->
        Error ("expected integer, found " ^ show_ty te.ty)
    | TIntLit n
      when Int64.compare n 0x7FFF_FFFFL > 0
           || (Int64.compare n 0L < 0 && is_unsigned te.ty) ->
        Error (shown n ^ " is too large")
    | TIntLit n when Int64.compare n 0L < 0 -> Error (shown n ^ " is negative")
    | TIntLit n -> Ok (Int64.to_int n)
    | _ when has_error te.ty -> Ok 0
    | _ -> Error "expected a literal"
  in
  let edges = Symbol.Table.create 16 in
  let record_edges = function
    | Struct sd -> (
        let key = key_at ctx sd.struct_span in
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
    | Struct sd when Symbol.Table.mem cyclic (key_at ctx sd.struct_span) ->
        Diagnostic.emit
          (Diagnostic.error sd.struct_name.span
             "recursive struct has infinite size")
    | Struct _ | Func _ | Extern _ | Global _ | TypeAlias _ | Enum _ -> ()
  in
  (* The report follows the file so the same struct is blamed every time *)
  List.iter report_cycle ctx.declarations

(* A variant names no type so one pass settles the whole enum *)
let reserve_enum_name ctx ed =
  if not (type_name_taken ctx ed.enum_span) then begin
    let variants_by_name = Hashtbl.create (List.length ed.variants) in
    let add next (v : ident) =
      if Hashtbl.mem variants_by_name v.value then begin
        Diagnostic.emit (Diagnostic.error v.span "duplicate variant");
        next
      end
      else begin
        Hashtbl.add variants_by_name v.value next;
        Int64.succ next
      end
    in
    ignore (List.fold_left add 0L ed.variants);
    Symbol.Table.replace ctx.type_defs (key_at ctx ed.enum_span)
      (Enum_type (ref (Completed { variants_by_name })))
  end

let reserve_alias_name ctx td =
  if not (type_name_taken ctx td.alias_span) then
    Symbol.Table.replace ctx.type_defs (key_at ctx td.alias_span)
      (Alias_type (ref Unstarted))

let rec named_type_spans (t : typ) =
  match t.tdesc with
  | Named _ -> [ t.tspan ]
  | Pointer inner | Slice inner | Array (_, inner) -> named_type_spans inner
  | FuncPtr (_, params, ret) ->
      List.concat_map named_type_spans params @ named_type_spans ret
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
        let key = key_at ctx td.alias_span in
        if key <> Symbol.unresolved_key && not (Hashtbl.mem defs key) then
          Hashtbl.add defs key decl
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ()
  in
  let rec resolve_alias key =
    match Hashtbl.find_opt defs key with
    | None -> ()
    | Some (TypeAlias td) -> (
        match Symbol.Table.find_opt ctx.type_defs key with
        | Some (Alias_type body) -> (
            match !body with
            | Completed _ -> ()
            | Running ->
                Diagnostic.emit
                  (Diagnostic.error td.alias_name.span "recursive type")
            | Unstarted ->
                body := Running;
                List.iter
                  (fun span -> resolve_alias (key_at ctx span))
                  (named_type_spans td.alias_typ);
                let env = make_env ctx in
                body := Completed (ty_of_ast env td.alias_typ))
        | Some (Struct_type _ | Builtin_type _ | Enum_type _) | None -> ())
    | Some (Func _ | Extern _ | Global _ | Struct _ | Enum _) -> ()
  in
  List.iter remember ctx.declarations;
  (* The order here follows the file so the same type gets blamed every time *)
  let resolve = function
    | TypeAlias td -> resolve_alias (key_at ctx td.alias_span)
    | Func _ | Extern _ | Global _ | Struct _ | Enum _ -> ()
  in
  let param_ty i p =
    match (p.param_typ.edesc, given, declared) with
    | Hole, Some f, _ ->
        Option.value (List.nth_opt f.sig_params i) ~default:TError
    | Hole, None, Some TError -> TError
    | Hole, None, _ ->
        Diagnostic.emit (Diagnostic.error p.param_name.span "cannot infer type");
        TError
    | _ -> ty_of_ast env p.param_typ
  in
  Symbol.Table.replace env.ctx.globals key t

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
  let collect = function
    | Func fd | Extern fd ->
        let env = make_env ctx in
        collect_func env fd
    | Global gd ->
        let env = make_env ctx in
        collect_global env gd
    | Struct _ | TypeAlias _ | Enum _ -> ()
  in
  List.iter collect ctx.declarations

let fill_struct_layouts ctx =
  let fill = function
    | Struct sd ->
        let env = make_env ctx in
        fill_struct_fields env sd
    | Func _ | Extern _ | Global _ | TypeAlias _ | Enum _ -> ()
  in
  let fsig = ref (Completed { func; param_hole = false }) in
  Symbol.Table.replace env.ctx.items key
    (FuncItem { def = fd; annot = None; fsig });
  if not (is_probing env) then begin
    Symbol.Table.replace env.ctx.lambdas key (check_func (make_env env.ctx) fd)
  end;
  mk e.espan (TFunc func) (TIdent (sym env e.espan))

let check_func ?(is_extern = false) env fd =
  let key = key_at env.ctx fd.func_span in
  let is_entry_point =
    symbol_at env.ctx fd.func_span false (fun s -> s.Symbol.entry_point)
  in
  let func =
    match func_sig env fd.func_span key with
    | Some s when List.compare_lengths s.func.sig_params l.lambda_params = 0 ->
        s.func
    | _ -> lambda_sig env l (resolve_abi l.lambda_abi) None
  in
  let { sig_params = param_tys; sig_ret = ret_ty; _ } = func in
  let params_typed =
    List.map2
      (fun p t -> (p.param_name, t, p.param_span))
      l.lambda_params param_tys
  in
  let params = List.map (fun (_, t, span) -> (sym env span, t)) params_typed in

  let invalid_entry_return =
    match resolve_ty ret_ty with
    | TError | TUnit | TInt I32 -> false
    | _ -> is_entry_point
  in
  if invalid_entry_return then begin
    let span =
      match l.lambda_ret with Some t -> t.espan | None -> fd.func_span
    in
    Diagnostic.emit
      (Diagnostic.error span "type mismatch"
      |> Diagnostic.label "expected %s or %s, found %s" (show_ty TUnit)
           (show_ty (TInt I32)) (show_ty ret_ty))
  end;

  let func_env = push_scope { env with ret_ty } in
  let param_env =
    List.fold_right
      (fun (_, t, span) e -> extend_var e span t)
      params_typed func_env
  in

  let implicit_return = (not is_extern) && not (is_unit ret_ty) in
  let body_use = if not implicit_return then Discard else Expect ret_ty in
  let body_span =
    match l.lambda_ret with Some t -> t.espan | None -> fd.func_name.span
  in
  let tbody0 = check_block param_env body_span l.lambda_body body_use in
  let tbody =
    match (implicit_return, List.rev tbody0) with
    | true, last :: rest when ty_equal last.ty ret_ty && not (is_never ret_ty)
      ->
        let ret = mk last.tspan TNever (TReturn (Some last)) in
        List.rev (ret :: rest)
    | _ -> tbody0
  in

  {
    tfunc_key = key;
    tfunc_name =
      link_name_at env fd.func_span (Interner.text fd.func_name.value);
    tfunc_source_name = Interner.text fd.func_name.value;
    tfunc_entry_point = is_entry_point;
    tfunc_params = params;
    tfunc_ret = ret_ty;
    tfunc_body = tbody;
    tfunc_export = fd.extern_abi <> NoAbi;
    tfunc_variadic = fd.variadic;
    tfunc_abi =
      (match collected with
      | Some s -> s.abi
      | None -> resolve_abi fd.extern_abi);
  }

let check_global env (gd : global_def) =
  let key = key_at env.ctx gd.span in
  (* The collected type is reused so a bad array size errors once *)
  let t =
    match !(fact.declared_ty) with Completed t -> t | _ -> global_ty env fact
  in
  let tinit =
    let* e = bind_value b in
    Some (global_typed_init env e.espan fact)
  in
  {
    tglobal_key = key;
    tglobal_name =
      link_name_at env b.bind_span (Interner.text b.bind_name.value);
    tglobal_ty = t;
    tglobal_init = tinit;
  }

let is_local_type ctx b =
  Option.exists
    (fun symbol -> symbol.Symbol.kind = Symbol.LocalType)
    (Resolve.sym_at_opt ctx.symbols b.bind_span)

(* The lambdas land in key order so the output is the same every run *)
let check_decls ctx =
  let check_decl = function
    | FuncItem { def; _ } ->
        let env = make_env ctx in
        let tfd = check_func env def in
        TFuncDecl tfd
    | Extern fd ->
        let env = make_env ctx in
        let tfd = check_func ~is_extern:true env fd in
        TExternDecl tfd
    | Struct sd ->
        (* A rejected duplicate has no completed table entry *)
        let field_tys =
          match struct_info ctx (key_at ctx sd.struct_span) with
          | Some info -> List.map snd info.field_tys
          | None ->
              let env = make_env ctx in
              List.map (fun f -> ty_of_ast env f.field_typ) sd.fields
        in
        typed_struct_decl ctx sd field_tys
    | Global gd ->
        let env = make_env ctx in
        TGlobalDecl (check_global env fact)
    | AliasItem { decl = b; body; aliased } ->
        let ty =
          match !aliased with
          | Completed ty -> ty
          | _ ->
              let env = make_env ctx in
              ty_of_ast env body
        in
        TTypeAliasDecl
          (keyname_at ctx td.alias_span (Interner.text td.alias_name.value), t)
    | Enum ed ->
        TEnumDecl
          (keyname_at ctx ed.enum_span (Interner.text ed.enum_name.value))
  in
  List.map check_declaration ctx.declarations

(* A global init can read a later global so defs go in first *)
let register_globals ctx =
  let register = function
    | Global gd ->
        Symbol.Table.replace ctx.global_facts (key_at ctx gd.span)
          {
            declaration = gd;
            declared_ty = ref Unstarted;
            typed = ref Unstarted;
          }
    | Func _ | Extern _ | Struct _ | TypeAlias _ | Enum _ -> ()
  in
  List.iter register ctx.declarations

(* The partial tree stays available so later checks can still run *)
let analyze uses decls =
  let decls = List.map item_of_decl (decls @ Resolve.local_decls uses) in
  let ctx = make_ctx uses decls in
  register_globals ctx;
  reserve_type_names ctx;
  resolve_type_bodies ctx;
  fill_struct_layouts ctx;
  collect_decls ctx;
  verify_type_cycles ctx;
  check_decls ctx
