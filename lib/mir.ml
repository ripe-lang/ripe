(* SPDX-License-Identifier: Apache-2.0 *)

open Types
open Tast

type local_id = int
type block_id = int
type storage_kind = Param | User | Temp | Result
type place_base = Local of local_id | Global of string

type constant =
  | Int of int64
  | Float of float
  | Bool of bool
  | Null
  | CStr of string
  | Char of int
  | Zero
  | Undef
  | Function of string
  | Str of string

type place = {
  place_base : place_base;
  place_projs : projection list;
  place_span : Ast.span;
}

and projection = Deref | Field of int * int | Index of operand | Tag
and operand = { op_desc : operand_desc; op_ty : ty; op_span : Ast.span }
and operand_desc = Copy of place | Const of constant

type unop = Neg | Not | BitNot

type binop =
  | Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  | Lte
  | Gte
  | BitAnd
  | BitOr
  | BitXor
  | Lshift
  | Rshift

type value = { val_desc : value_desc; val_ty : ty }

and value_desc =
  | Use of operand
  | UnOp of unop * operand
  | BinOp of binop * operand * operand
  | Cast of operand
  | AddressOf of place
  | Len of place
  | DataPtr of place
  | SizeOf of ty

(* TODO(b597): A whole aggregate value would avoid assigning every field on its own *)

type call_kind = Internal | External
type callee = Direct of string | Indirect of operand

type call = {
  call_dest : place option;
  call_callee : callee;
  call_kind : call_kind;
  call_args : operand list;
  call_ret : ty;
  call_variadic_start : int option;
}

type stmt = { stmt_desc : stmt_desc; stmt_span : Ast.span }

and stmt_desc =
  | Assign of place * value
  | Call of call
  | Slice of place * place * operand * operand

type terminator = { term_desc : terminator_desc; term_span : Ast.span }

and terminator_desc =
  | Jump of block_id
  | Branch of operand * block_id * block_id
  | ReturnValue of operand option
  | Unreachable

type local = {
  local_name : string option;
  local_ty : ty;
  local_storage : storage_kind;
  local_span : Ast.span;
}

type block = { block_stmts : stmt list; block_term : terminator option }

type func = {
  func_name : string;
  func_export : bool;
  func_abi : func_abi;
  func_params : local_id list;
  func_result : local_id option;
  func_locals : local array;
  func_blocks : block array;
  func_ret : ty;
  func_entry_point : bool;
  func_span : Ast.span;
}

type adt_decl = {
  adt_name : Keyname.t;
  adt_kind : adt_kind;
  adt_variants : ty list list;
  adt_local : bool;
}

type global_value =
  | GlobalConst of constant * ty
  | GlobalAddress of string
  | GlobalArray of global_value list
  | GlobalFields of int * (int * global_value) list

type global = {
  global_name : string;
  global_ty : ty;
  global_init : global_value option;
}

type program = {
  prog_adts : adt_decl list;
  prog_globals : global list;
  prog_funcs : func list;
}

type loop_ctx = {
  continue_block : block_id;
  break_block : block_id;
  result : (place * ty) option;
}

type builder = {
  global_names : (Symbol.key, string) Hashtbl.t;
  layouts : Layout.t;
  variant_funcs : (string, int64 * func_ty) Hashtbl.t;
  locals : local Dynarray.t;
  symbols : (Symbol.id, local_id) Hashtbl.t;
  blocks : block Dynarray.t;
  mutable current : block_id;
  mutable loops : loop_ctx list;
  mutable result : local_id option;
}

type ctx = {
  layouts : Layout.t;
  globals : (string, ty) Hashtbl.t;
  signatures : (string, ty list * ty) Hashtbl.t;
  func : func;
}

let make_builder global_names layouts variant_funcs =
  {
    global_names;
    layouts;
    variant_funcs;
    locals = Dynarray.create ();
    symbols = Hashtbl.create 16;
    blocks = Dynarray.make 1 { block_stmts = []; block_term = None };
    current = 0;
    loops = [];
    result = None;
  }

let finish_blocks state =
  Dynarray.to_array state.blocks
  |> Array.map (fun b -> { b with block_stmts = List.rev b.block_stmts })

let add_local state name storage ty span =
  let id = Dynarray.length state.locals in
  Dynarray.add_last state.locals
    {
      local_name = name;
      local_ty = ty;
      local_storage = storage;
      local_span = span;
    };
  id

let declare state symbol storage ty span =
  let id = add_local state (Some symbol.Symbol.name) storage ty span in
  Hashtbl.add state.symbols symbol.id id;
  id

let new_block state =
  let id = Dynarray.length state.blocks in
  Dynarray.add_last state.blocks { block_stmts = []; block_term = None };
  id

let current_block state = Dynarray.get state.blocks state.current
let is_live state = Option.is_none (current_block state).block_term
let switch state block = state.current <- block

let emit state desc span =
  let b = current_block state in
  if Option.is_none b.block_term then
    Dynarray.set state.blocks state.current
      {
        b with
        block_stmts = { stmt_desc = desc; stmt_span = span } :: b.block_stmts;
      }

let terminate state desc span =
  let b = current_block state in
  if Option.is_none b.block_term then
    Dynarray.set state.blocks state.current
      { b with block_term = Some { term_desc = desc; term_span = span } }

let local_place span id =
  { place_base = Local id; place_projs = []; place_span = span }

let add_projection place projection =
  { place with place_projs = projection :: place.place_projs }

let copy span ty place = { op_desc = Copy place; op_ty = ty; op_span = span }
let const span ty desc = { op_desc = Const desc; op_ty = ty; op_span = span }
let new_tmp state span ty = local_place span (add_local state None Temp ty span)

let store state destination ty span desc =
  emit state (Assign (destination, { val_desc = desc; val_ty = ty })) span

let assign state destination assigned =
  store state destination assigned.op_ty assigned.op_span (Use assigned)

let tmp_value state ty span desc =
  let destination = new_tmp state span ty in
  store state destination ty span desc;
  copy span ty destination

let materialize state operand =
  match operand.op_desc with
  | Copy place -> place
  | Const _ ->
      let destination = new_tmp state operand.op_span operand.op_ty in
      assign state destination operand;
      destination

let symbol_place state span symbol =
  match
    ( Hashtbl.find_opt state.symbols symbol.Symbol.id,
      Hashtbl.find_opt state.global_names (Symbol.key symbol) )
  with
  | Some id, _ -> local_place span id
  | None, Some name ->
      { place_base = Global name; place_projs = []; place_span = span }
  | None, None ->
      Diagnostic.ice ~span
        (Printf.sprintf "no MIR place for symbol %s" symbol.name)

let variant_func variant_funcs name tag func =
  let func_name =
    Printf.sprintf "_Rvariant%d_%Ld" (Keyname.key name :> int) tag
  in
  Hashtbl.replace variant_funcs func_name (tag, func);
  func_name

let start_variant state destination span ty tag =
  assign state destination (const span ty Undef);
  assign state
    (add_projection destination Tag)
    (const span Layout.tag_ty (Int tag))

(* TODO(7a4f): This is temporary until global inits fold again *)
let constant_of expr =
  match expr.tdesc with
  (* The MIR keeps the value but not the variant name *)
  | TIntLit value | TBuild (_, value, []) -> Int value
  | TFloatLit value -> Float value
  | TBoolLit value -> Bool value
  | TNullLit -> Null
  | TCStrLit value -> CStr value
  | TStrLit value -> Str value
  | TCharLit value -> Char value
  | TZero -> Zero
  | TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
      Function symbol.Symbol.link_name
  | _ ->
      raise
        (Diagnostic.Errors
           [ Diagnostic.error expr.tspan "global is not a literal" ])

let unop_of = function
  | Ast.Neg -> Neg
  | Ast.Not -> Not
  | Ast.BitNot -> BitNot
  | Ast.Deref -> Diagnostic.ice "deref is a projection and not a MIR value"
  | Ast.AddressOf -> Diagnostic.ice "address of is its own MIR value"

let binop_of = function
  | Ast.Add -> Add
  | Ast.Sub -> Sub
  | Ast.Mul -> Mul
  | Ast.Div -> Div
  | Ast.Mod -> Mod
  | Ast.Eq -> Eq
  | Ast.Neq -> Neq
  | Ast.Lt -> Lt
  | Ast.Gt -> Gt
  | Ast.Lte -> Lte
  | Ast.Gte -> Gte
  | Ast.BitAnd -> BitAnd
  | Ast.BitOr -> BitOr
  | Ast.BitXor -> BitXor
  | Ast.Lshift -> Lshift
  | Ast.Rshift -> Rshift
  | Ast.And | Ast.Or -> Diagnostic.ice "short circuit reached MIR as a value"

let is_valued ty = not (is_unit ty || is_never ty)

(* The minimum integer and negative one need no hardware divide *)
(* %3 = copy %1 == -1
   branch copy %3 block1 block2
   block1:
     %2 = -copy %0
     jump block3
   block2:
     %2 = copy %0 / copy %1
     jump block3 *)
let lower_div state destination span ty op left right =
  let negative_one = const span right.op_ty (Int (-1L)) in
  let divisor_is_negative_one =
    tmp_value state TBool span (BinOp (Eq, right, negative_one))
  in
  let wrap_block = new_block state in
  let divide_block = new_block state in
  let join_block = new_block state in
  terminate state
    (Branch (divisor_is_negative_one, wrap_block, divide_block))
    span;
  switch state wrap_block;
  if op = Mod then assign state destination (const span ty (Int 0L))
  else store state destination ty span (UnOp (Neg, left));
  terminate state (Jump join_block) span;
  switch state divide_block;
  store state destination ty span (BinOp (op, left, right));
  terminate state (Jump join_block) span;
  switch state join_block

(* A count past the width drains every bit *)
(* TODO(ca5f): Check on negative counts again since Go panics on them *)
(* %3 = cast copy %1 to u64
   %4 = copy %3 < 32
   branch copy %4 block1 block2
   block1:
     %2 = copy %0 << copy %1
     jump block3
   block2:
     %2 = 0
     jump block3 *)
let lower_shift state destination span ty op left right =
  let bits =
    match resolve_ty ty with
    | TInt k -> 8 * int_kind_size k
    | _ -> Diagnostic.ice "shift on a non integer"
  in
  let count = tmp_value state (TInt U64) span (Cast right) in
  let width = const span (TInt U64) (Int (Int64.of_int bits)) in
  let count_in_range = tmp_value state TBool span (BinOp (Lt, count, width)) in
  let shift_block = new_block state in
  let drained_block = new_block state in
  let join_block = new_block state in
  terminate state (Branch (count_in_range, shift_block, drained_block)) span;
  switch state shift_block;
  store state destination ty span (BinOp (op, left, right));
  terminate state (Jump join_block) span;
  switch state drained_block;
  (* A signed right shift settles at the top bit *)
  if op = Rshift && not (is_unsigned ty) then
    let top = const span right.op_ty (Int (Int64.of_int (bits - 1))) in
    store state destination ty span (BinOp (Rshift, left, top))
  else assign state destination (const span ty (Int 0L));
  terminate state (Jump join_block) span;
  switch state join_block

(* %2 = copy %0 + copy %1 *)
let lower_binary state destination span ty op left right =
  let lowered = binop_of op in
  match op with
  | _ when not (is_live state) -> ()
  | (Ast.Div | Ast.Mod) when div_int_needs_check left.op_ty ->
      lower_div state destination span ty lowered left right
  | Ast.Lshift | Ast.Rshift ->
      lower_shift state destination span ty lowered left right
  | _ -> store state destination ty span (BinOp (lowered, left, right))

(* %1 = call @g()
   %0 = 3 *)
let rec lower_block state destination = function
  | [] -> ()
  | [ last ] -> if is_live state then lower_into state destination last
  | stmt :: rest ->
      lower_stmt state stmt;
      lower_block state destination rest

(* branch copy %0 block1 block2
   block1:
     %2 = copy %1
     jump block3
   block2:
     %2 = false
     jump block3 *)
and lower_short_circuit state destination expr left right short_value =
  let right_block = new_block state in
  let short_block = new_block state in
  let join_block = new_block state in
  lower_cond state left
    (if short_value then short_block else right_block)
    (if short_value then right_block else short_block);
  switch state short_block;
  let short = const expr.tspan TBool (Bool short_value) in
  Option.iter (fun d -> assign state d short) destination;
  terminate state (Jump join_block) expr.tspan;
  switch state right_block;
  lower_into state destination right;
  terminate state (Jump join_block) expr.tspan;
  switch state join_block

(* branch copy %0 block2 block3
   block2:
     %1 = 1
     jump block1
   block3:
     %1 = 2
     jump block1 *)
and lower_if state destination expr branches else_body =
  let join = new_block state in
  let rec lower_branches = function
    | [] ->
        lower_arm state destination expr join
          (Option.value else_body ~default:[])
    | (condition, body) :: rest ->
        let yes = new_block state in
        let no = new_block state in
        lower_cond state condition yes no;
        switch state yes;
        lower_arm state destination expr join body;
        switch state no;
        lower_branches rest
  in
  lower_branches branches;
  switch state join

(* The arms stay in order because a jump table only pays off on a dense range *)
(* %2 = copy %0 == 1
   branch copy %2 block3 block2
   block2:
     %3 = copy %0
     %1 = copy %3
     jump block1
   block3:
     %1 = 10
     jump block1 *)
and lower_match state destination expr scrutinee arms =
  (* The scrutinee is a place so a field pattern can project into it *)
  let subject = materialize state (lower_expr state scrutinee) in
  let join = new_block state in
  let rec lower_arms = function
    | [] -> terminate state Unreachable expr.tspan
    | { tpat = (TPatWild | TPatBind _) as tpat; tbody } :: _ ->
        lower_pattern state expr.tspan subject scrutinee.ty tpat join;
        lower_arm state destination expr join tbody
    | { tpat; tbody } :: rest ->
        let no = new_block state in
        lower_pattern state expr.tspan subject scrutinee.ty tpat no;
        lower_arm state destination expr join tbody;
        switch state no;
        lower_arms rest
  in
  lower_arms arms;
  switch state join

(* %2 = copy %0.tag == 1
   branch copy %2 block3 block2
   block3:
     %3 = copy %0.variant1.field0 *)
and lower_pattern state span subject ty tpat no =
  let test found value =
    let yes = new_block state in
    let against = const span found.op_ty value in
    let equal = tmp_value state TBool span (BinOp (Eq, found, against)) in
    terminate state (Branch (equal, yes, no)) span;
    switch state yes
  in
  match (tpat, resolve_ty ty) with
  | TPatWild, _ -> ()
  | TPatBind (symbol, bound_ty), _ ->
      let id = declare state symbol User bound_ty span in
      assign state (local_place span id) (copy span bound_ty subject)
  | TPatConst value, TBool -> test (copy span ty subject) (Bool (value <> 0L))
  | TPatConst value, TChar ->
      test (copy span ty subject) (Char (Int64.to_int value))
  | TPatVariant (tag, subs), TAdt (name, _)
    when Layout.has_payload state.layouts ty ->
      test (copy span Layout.tag_ty (add_projection subject Tag)) (Int tag);
      let variant = Int64.to_int tag in
      List.iteri
        (fun i sub ->
          let part = add_projection subject (Field (variant, i)) in
          let part_ty = Layout.field_ty state.layouts name variant i in
          lower_pattern state span part part_ty sub no)
        subs
  | (TPatConst value | TPatVariant (value, _)), _ ->
      test (copy span ty subject) (Int value)

(* %1 = 10
   jump block1 *)
and lower_arm state destination expr join body =
  lower_block state destination body;
  terminate state (Jump join) expr.tspan

(* branch copy %0 block1 block2, jump block1 *)
and lower_cond state expr yes no =
  match expr.tdesc with
  | TUnOp (Ast.Not, inner) -> lower_cond state inner no yes
  | TBinOp (Ast.And, left, right) ->
      let middle = new_block state in
      lower_cond state left middle no;
      switch state middle;
      lower_cond state right yes no
  | TBinOp (Ast.Or, left, right) ->
      let middle = new_block state in
      lower_cond state left yes middle;
      switch state middle;
      lower_cond state right yes no
  | _ ->
      let condition = lower_expr state expr in
      terminate state (Branch (condition, yes, no)) expr.tspan

(* The stack gives nested loop control the nearest matching target *)
(* %1 = copy %1 + 1
   jump block1 *)
and lower_loop_body state span continue_block break_block result body =
  state.loops <- { continue_block; break_block; result } :: state.loops;
  List.iter (lower_stmt state) body;
  state.loops <- List.tl state.loops;
  if is_live state then terminate state (Jump continue_block) span

(* An inclusive loop checks the limit before stepping so it can't overflow *)
(* jump block1
   block1:
     %3 = copy %1 < copy %2
     branch copy %3 block2 block4
   block3:
     %1 = copy %1 + 1
     jump block1 *)
and lower_counted_loop state span op ty counter limit elem body =
  let condition_block = new_block state in
  let body_block = new_block state in
  let step_block = new_block state in
  let exit_block = new_block state in
  let current = copy span ty counter in
  let limit = copy span ty limit in
  terminate state (Jump condition_block) span;
  switch state condition_block;
  let in_range = tmp_value state TBool span (BinOp (op, current, limit)) in
  terminate state (Branch (in_range, body_block, exit_block)) span;
  switch state body_block;
  (match elem with
  | Some (destination, value) -> assign state destination value
  | None -> ());
  lower_loop_body state span step_block exit_block None body;
  switch state step_block;
  if op = Lte then begin
    let increment_block = new_block state in
    let last = tmp_value state TBool span (BinOp (Eq, current, limit)) in
    terminate state (Branch (last, exit_block, increment_block)) span;
    switch state increment_block
  end;
  let one = const span ty (Int 1L) in
  store state counter ty span (BinOp (Add, current, one));
  terminate state (Jump condition_block) span;
  switch state exit_block

and lower_for state span symbol elem_ty iter body =
  match iter.tdesc with
  | TRange (lo, hi) -> lower_range_for state span symbol elem_ty lo hi Lt body
  | TRangeInclusive (lo, hi) ->
      lower_range_for state span symbol elem_ty lo hi Lte body
  | _ -> lower_each_for state span symbol elem_ty iter body

(* %1 = 0
   %2 = 3 *)
and lower_range_for state span symbol elem_ty lo hi op body =
  let loop_id = declare state symbol User elem_ty symbol.Symbol.span in
  let lo = lower_expr state lo in
  let hi = lower_expr state hi in
  let counter = local_place span loop_id in
  assign state counter lo;
  let limit =
    local_place span (add_local state (Some "for.hi") Temp elem_ty hi.op_span)
  in
  assign state limit hi;
  lower_counted_loop state span op elem_ty counter limit None body

(* %2 = data_ptr %0
   %3 = len %0
   %4 = 0
   ...
   %5 = copy %2[copy %4] *)
and lower_each_for state span symbol elem_ty iter body =
  let usize = TInt Usize in
  let source = lower_expr state iter |> materialize state in
  let ptr_ty = TPointer elem_ty in
  let ptr_place = new_tmp state span ptr_ty in
  let len_place = new_tmp state span usize in
  let index = new_tmp state span usize in
  let loop_id = declare state symbol User elem_ty symbol.Symbol.span in
  store state ptr_place ptr_ty span (DataPtr source);
  store state len_place usize span (Len source);
  assign state index (const span usize (Int 0L));
  let at_index = add_projection ptr_place (Index (copy span usize index)) in
  let elem = (local_place span loop_id, copy span elem_ty at_index) in
  lower_counted_loop state span Lt usize index len_place (Some elem) body

(* jump block1
   block1:
     %2 = copy %0 == 1
     branch copy %2 block4 block5
   block4:
     %1 = 1
     jump block2 *)
and lower_loop state destination span ty body =
  let body_block = new_block state in
  let exit_block = new_block state in
  terminate state (Jump body_block) span;
  switch state body_block;
  let result = Option.map (fun place -> (place, ty)) destination in
  lower_loop_body state span body_block exit_block result body;
  switch state exit_block

(* copy %0, 1 *)
and lower_arg state expr =
  let value = lower_expr state expr in
  if is_unit value.op_ty then None
  else if Layout.is_aggregate state.layouts value.op_ty then
    Some (copy value.op_span value.op_ty (materialize state value))
  else Some value

(* 1, copy %0, copy %0.deref.field0, copy %1 *)
and lower_expr state expr =
  match expr.tdesc with
  | TIdent _ when is_unit expr.ty -> const expr.tspan expr.ty Undef
  | TIntLit _ | TFloatLit _ | TBoolLit _ | TNullLit | TCStrLit _ | TStrLit _
  | TCharLit _ | TZero ->
      const expr.tspan expr.ty (constant_of expr)
  | TBuild _ when not (Layout.is_aggregate state.layouts expr.ty) ->
      const expr.tspan expr.ty (constant_of expr)
  | TVariant (name, tag) -> (
      match resolve_ty expr.ty with
      | TFunc func ->
          const expr.tspan expr.ty
            (Function (variant_func state.variant_funcs name tag func))
      | _ -> Diagnostic.ice ~span:expr.tspan "variant value is not a function")
  | TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
      const expr.tspan expr.ty (constant_of expr)
  | TIdent symbol ->
      copy expr.tspan expr.ty (symbol_place state expr.tspan symbol)
  | TUnOp (Ast.Deref, _) | TFieldAccess _ | TIndex _ ->
      copy expr.tspan expr.ty (lower_place state expr)
  | _ when is_valued expr.ty ->
      let slot = new_tmp state expr.tspan expr.ty in
      lower_into state (Some slot) expr;
      copy expr.tspan expr.ty slot
  | _ ->
      lower_into state None expr;
      const expr.tspan expr.ty Undef

(* The destination is always fresh so writing into it early can't clobber a
   read *)
(* %1 = -copy %0, %1 = cast copy %0 to i64, %2 = address_of %1, %1 = len %0 *)
and lower_into state destination expr =
  let slot () =
    match destination with
    | Some slot -> slot
    | None -> new_tmp state expr.tspan expr.ty
  in
  let store_value desc =
    Option.iter (fun d -> store state d expr.ty expr.tspan desc) destination
  in
  (match expr.tdesc with
  | TErrorExpr -> Diagnostic.ice ~span:expr.tspan "error expression reached MIR"
  | TRange _ | TRangeInclusive _ ->
      Diagnostic.ice ~span:expr.tspan "range outside a for loop"
  | TBuild (_, tag, fields) when Layout.is_aggregate state.layouts expr.ty ->
      lower_build state (slot ()) expr tag fields
  | TCall (callee, args, variadic_start) ->
      let destination = if is_valued expr.ty then Some (slot ()) else None in
      lower_call state destination expr callee args variadic_start
  | TBinOp (Ast.And, left, right) ->
      lower_short_circuit state destination expr left right false
  | TBinOp (Ast.Or, left, right) ->
      lower_short_circuit state destination expr left right true
  | TBinOp (op, left, right) ->
      let left = lower_expr state left in
      let right = lower_expr state right in
      Option.iter
        (fun d -> lower_binary state d expr.tspan expr.ty op left right)
        destination
  | TAssign (None, left, right) when is_unit left.ty ->
      lower_into state None right
  | TAssign (None, left, right) ->
      let assigned = lower_expr state right in
      assign state (lower_place state left) assigned
  | TAssign (Some op, left, right) ->
      lower_compound_assign state expr op left right
  | TUnOp (Ast.AddressOf, { tdesc = TUnOp (Ast.Deref, inner); _ }) ->
      lower_into state destination inner
  | TUnOp (Ast.AddressOf, inner) ->
      store_value (AddressOf (lower_place state inner))
  | TUnOp (Ast.Deref, _)
  | TFieldAccess _ | TIndex _ | TIdent _ | TIntLit _ | TBuild _ | TVariant _
  | TFloatLit _ | TBoolLit _ | TNullLit | TCStrLit _ | TStrLit _ | TCharLit _
  | TZero ->
      let value = lower_expr state expr in
      Option.iter (fun d -> assign state d value) destination
  | TUnOp (op, inner) -> store_value (UnOp (unop_of op, lower_expr state inner))
  | TCast inner -> store_value (Cast (lower_expr state inner))
  | TSizeOf ty -> store_value (SizeOf ty)
  | TLen inner ->
      store_value (Len (lower_expr state inner |> materialize state))
  | TDataPtr inner ->
      store_value (DataPtr (lower_expr state inner |> materialize state))
  | TArrayLit elems -> lower_array_lit state (slot ()) expr elems
  | TSliceExpr (base, lo, hi) -> lower_slice state (slot ()) expr base lo hi
  | TLoop body -> lower_loop state destination expr.tspan expr.ty body
  | TBlock body -> lower_block state destination body
  | TIf (branches, else_body) ->
      lower_if state destination expr branches else_body
  | TMatch (scrutinee, arms) ->
      lower_match state destination expr scrutinee arms
  | TBinding _ | TReturn _ | TBreak _ | TContinue _ | TFor _ ->
      lower_stmt state expr
  | TLocalDecl | TUnitLit | TTypeExpr -> ());
  if is_never expr.ty then terminate state Unreachable expr.tspan

(* %0 = call @g(), call @puts(copy %1) *)
and lower_call state destination expr callee args variadic_start =
  let args = List.filter_map (lower_arg state) args in
  let kind =
    match resolve_ty callee.ty with
    | TFunc { sig_abi = C; _ } -> External
    | _ -> Internal
  in
  let callee =
    match callee.tdesc with
    | TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
        Direct symbol.Symbol.link_name
    | _ -> Indirect (lower_expr state callee)
  in
  let call =
    {
      call_dest = destination;
      call_callee = callee;
      call_kind = kind;
      call_args = args;
      call_ret = expr.ty;
      call_variadic_start = variadic_start;
    }
  in
  emit state (Call call) expr.tspan

(* %0, @count, %0.deref.field0, %0[copy %1] *)
and lower_place state expr =
  match expr.tdesc with
  | TIdent symbol when not (Symbol.is_func symbol.Symbol.kind) ->
      symbol_place state expr.tspan symbol
  | TUnOp (Ast.Deref, inner) -> lower_deref state inner
  | TFieldAccess (base, field) ->
      let source =
        match resolve_ty base.ty with
        | TPointer _ -> lower_deref state base
        | _ -> lower_expr state base |> materialize state
      in
      add_projection source (Field (0, field))
  | TIndex (base, index) ->
      let base_value = lower_expr state base in
      let source = materialize state base_value in
      let index = lower_expr state index in
      add_projection source (Index index)
  | _ -> lower_expr state expr |> materialize state

(* %0.deref *)
and lower_deref state ptr =
  let ptr = lower_expr state ptr in
  add_projection (materialize state ptr) Deref

(* %0 = undef
   %0[0] = 7
   %0[1] = 8 *)
and lower_array_lit state destination expr elems =
  assign state destination (const expr.tspan expr.ty Undef);
  List.iteri
    (fun index elem ->
      let index_operand =
        const elem.tspan (TInt Usize) (Int (Int64.of_int index))
      in
      let target = add_projection destination (Index index_operand) in
      lower_into state (Some target) elem)
    elems

(* %0 = undef
   %0.tag = 1
   %0.variant1.field0 = 3 *)
and lower_build state destination expr tag fields =
  if Layout.has_payload state.layouts expr.ty then
    start_variant state destination expr.tspan expr.ty tag
  else assign state destination (const expr.tspan expr.ty Undef);
  List.iter
    (fun (field, value) ->
      let part = add_projection destination (Field (Int64.to_int tag, field)) in
      lower_into state (Some part) value)
    fields

(* %0 = slice %1 1 3 *)
and lower_slice state destination expr base lo hi =
  let base = lower_expr state base |> materialize state in
  let lo = lower_expr state lo in
  let hi = lower_expr state hi in
  emit state (Slice (destination, base, lo, hi)) expr.tspan

(* %0[0] = copy %0[0] + 5 *)
and lower_compound_assign state expr op left right =
  let target = lower_place state left in
  let old = copy left.tspan left.ty target in
  match op with
  | Ast.And | Ast.Or ->
      let right_block = new_block state in
      let join = new_block state in
      let yes, no =
        if op = Ast.And then (right_block, join) else (join, right_block)
      in
      terminate state (Branch (old, yes, no)) expr.tspan;
      switch state right_block;
      assign state target (lower_expr state right);
      terminate state (Jump join) expr.tspan;
      switch state join
  | _ ->
      let right = lower_expr state right in
      lower_binary state target expr.tspan left.ty op old right

(* A later break can widen the loop type the earlier ones settled on *)
(* %1 = 1, %1 = cast copy %0 to i64 *)
and lower_break state (result, result_ty) value =
  if is_never value.ty || ty_equal value.ty result_ty then
    lower_into state (Some result) value
  else store state result result_ty value.tspan (Cast (lower_expr state value))

(* %1 = 0, return copy %1, jump block2 *)
and lower_stmt state expr =
  if is_live state then
    match expr.tdesc with
    | TBinding (symbol, ty, init) when not (is_never ty) ->
        let id = declare state symbol User ty symbol.Symbol.span in
        let local = local_place expr.tspan id in
        lower_into state (if is_valued ty then Some local else None) init
    | TBinding (_, _, init) -> lower_into state None init
    | TReturn returned ->
        let returned =
          match (returned, state.result) with
          | Some value, Some result ->
              lower_into state (Some (local_place expr.tspan result)) value;
              None
          | Some value, None when is_valued value.ty ->
              Some (lower_expr state value)
          | Some value, None ->
              lower_into state None value;
              None
          | None, _ -> None
        in
        terminate state (ReturnValue returned) expr.tspan
    | TBreak (depth, value) ->
        let target = List.nth state.loops depth in
        begin match (value, target.result) with
        | Some value, Some result -> lower_break state result value
        | Some value, None -> lower_into state None value
        | None, _ -> ()
        end;
        terminate state (Jump target.break_block) expr.tspan
    | TContinue depth ->
        let target = List.nth state.loops depth in
        terminate state (Jump target.continue_block) expr.tspan
    | TFor (symbol, elem_ty, iter, body) ->
        lower_for state expr.tspan symbol elem_ty iter body
    | _ -> lower_into state None expr

let rec global_value_of layouts variant_funcs expr =
  let value_of = global_value_of layouts variant_funcs in
  match expr.tdesc with
  | TUnOp (Ast.AddressOf, { tdesc = TIdent symbol; _ }) ->
      GlobalAddress symbol.Symbol.link_name
  | TArrayLit values -> GlobalArray (List.map value_of values)
  | TBuild (_, tag, fields) when Layout.is_aggregate layouts expr.ty ->
      let compare_field_ids (left, _) (right, _) = Int.compare left right in
      GlobalFields
        ( Int64.to_int tag,
          List.map
            (fun (field, value) -> (field, value_of value))
            (List.sort compare_field_ids fields) )
  | TVariant (name, tag) -> (
      match resolve_ty expr.ty with
      | TFunc func ->
          GlobalConst
            (Function (variant_func variant_funcs name tag func), expr.ty)
      | _ -> Diagnostic.ice ~span:expr.tspan "variant value is not a function")
  | _ -> GlobalConst (constant_of expr, expr.ty)

let build_global layouts variant_funcs global =
  {
    global_name = global.tglobal_name;
    global_ty = global.tglobal_ty;
    global_init =
      Option.map (global_value_of layouts variant_funcs) global.tglobal_init;
  }

(* A variant fn is only built when a variant is passed around as a function *)
let build_variant_func layouts (func_name, (tag, func)) =
  let state = make_builder (Hashtbl.create 0) layouts (Hashtbl.create 0) in
  let span = Ast.dummy_span in
  let result = add_local state (Some "result") Result func.sig_ret span in
  let params =
    List.map
      (fun ty -> (add_local state None Param ty span, ty))
      func.sig_params
  in
  let destination = local_place span result in
  start_variant state destination span func.sig_ret tag;
  List.iteri
    (fun i (id, ty) ->
      let part = add_projection destination (Field (Int64.to_int tag, i)) in
      assign state part (copy span ty (local_place span id)))
    params;
  terminate state (ReturnValue None) span;
  {
    func_name;
    func_export = false;
    func_abi = Ripe;
    func_params = List.map fst params;
    func_result = Some result;
    func_locals = Dynarray.to_array state.locals;
    func_blocks = finish_blocks state;
    func_ret = func.sig_ret;
    func_entry_point = false;
    func_span = span;
  }

let build_func global_names layouts variant_funcs func =
  let state = make_builder global_names layouts variant_funcs in
  let span =
    match (func.tfunc_body, func.tfunc_params) with
    | first :: _, _ -> first.tspan
    | [], (symbol, _) :: _ -> symbol.Symbol.span
    | [], [] -> Ast.dummy_span
  in
  (* A returned aggregate needs somewhere to live that outlives the frame *)
  (* TODO(73fc): A universal result slot would simplify inlining *)
  if Layout.is_aggregate layouts func.tfunc_ret then
    state.result <-
      Some (add_local state (Some "result") Result func.tfunc_ret span);
  let params =
    List.filter_map
      (fun (symbol, ty) ->
        if is_valued ty then
          Some (declare state symbol Param ty symbol.Symbol.span)
        else None)
      func.tfunc_params
  in
  List.iter (lower_stmt state) func.tfunc_body;
  if is_live state then
    if is_unit func.tfunc_ret then terminate state (ReturnValue None) span
    else terminate state Unreachable span;
  {
    func_name = func.tfunc_name;
    func_export = func.tfunc_export;
    func_abi = func.tfunc_abi;
    func_params = params;
    func_result = state.result;
    func_locals = Dynarray.to_array state.locals;
    func_blocks = finish_blocks state;
    func_ret = func.tfunc_ret;
    func_entry_point = func.tfunc_entry_point;
    func_span = span;
  }

let build decls =
  let adts, globals, funcs =
    List.fold_right
      (fun decl (adts, globals, funcs) ->
        match decl with
        | TAdtDecl (name, kind, variants) ->
            ( {
                adt_name = name;
                adt_kind = kind;
                adt_variants = variants;
                adt_local = false;
              }
              :: adts,
              globals,
              funcs )
        | TLocalAdtDecl (name, kind, variants) ->
            ( {
                adt_name = name;
                adt_kind = kind;
                adt_variants = variants;
                adt_local = true;
              }
              :: adts,
              globals,
              funcs )
        | TGlobalDecl global when is_valued global.tglobal_ty ->
            (adts, global :: globals, funcs)
        | TFuncDecl func -> (adts, globals, func :: funcs)
        | _ -> (adts, globals, funcs))
      decls ([], [], [])
  in
  let layouts = Layout.create () in
  List.iter
    (fun adt ->
      Layout.set_variants layouts (Keyname.key adt.adt_name) adt.adt_kind
        adt.adt_variants)
    adts;
  let global_names = Hashtbl.create 16 in
  List.iter
    (fun g -> Hashtbl.add global_names g.tglobal_key g.tglobal_name)
    globals;
  let variant_funcs = Hashtbl.create 8 in
  let globals = List.map (build_global layouts variant_funcs) globals in
  let functions =
    List.map (build_func global_names layouts variant_funcs) funcs
  in
  let variant_bodies =
    Hashtbl.to_seq variant_funcs
    |> List.of_seq
    |> List.sort (fun (a, _) (b, _) -> String.compare a b)
    |> List.map (build_variant_func layouts)
  in
  {
    prog_adts = adts;
    prog_globals = globals;
    prog_funcs = functions @ variant_bodies;
  }

(* A file without main still builds into a program that does nothing *)
let with_entry program =
  if List.exists (fun func -> func.func_entry_point) program.prog_funcs then
    program
  else
    let main =
      {
        tfunc_key = Symbol.unresolved_key;
        tfunc_name = Resolve.decl_link_name "main";
        tfunc_entry_point = true;
        tfunc_params = [];
        tfunc_ret = TUnit;
        tfunc_body = [];
        tfunc_export = false;
        tfunc_abi = Ripe;
      }
    in
    let main =
      build_func (Hashtbl.create 0) (Layout.create ()) (Hashtbl.create 0) main
    in
    { program with prog_funcs = program.prog_funcs @ [ main ] }

let show_storage = function
  | Param -> "param"
  | User -> "user"
  | Temp -> "temp"
  | Result -> "result"

let show_constant = function
  | Int value -> Int64.to_string value
  | Float value -> Printf.sprintf "%.17g" value
  | Bool value -> string_of_bool value
  | Null -> "null"
  | CStr value -> Printf.sprintf "%S" value
  | Char value -> Printf.sprintf "U+%04X" value
  | Zero -> "zero"
  | Undef -> "undef"
  | Function name -> "@" ^ name
  | Str value -> Printf.sprintf "str %S" value

let show_unop = function Neg -> "-" | Not -> "!" | BitNot -> "~"

let show_binop = function
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Eq -> "=="
  | Neq -> "!="
  | Lt -> "<"
  | Gt -> ">"
  | Lte -> "<="
  | Gte -> ">="
  | BitAnd -> "&"
  | BitOr -> "|"
  | BitXor -> "^"
  | Lshift -> "<<"
  | Rshift -> ">>"

let rec show_place value =
  let projection = function
    | Deref -> ".deref"
    | Field (0, field) -> Printf.sprintf ".field%d" field
    | Field (variant, field) ->
        Printf.sprintf ".variant%d.field%d" variant field
    | Index index -> Printf.sprintf "[%s]" (show_operand index)
    | Tag -> ".tag"
  in
  let base =
    match value.place_base with
    | Local id -> Printf.sprintf "%%%d" id
    | Global name -> "@" ^ name
  in
  Printf.sprintf "%s%s" base
    (String.concat "" (List.map projection (List.rev value.place_projs)))

and show_operand value =
  match value.op_desc with
  | Copy source -> "copy " ^ show_place source
  | Const value -> show_constant value

let show_value value =
  match value.val_desc with
  | Use operand_value -> show_operand operand_value
  | UnOp (op, operand_value) ->
      Printf.sprintf "%s%s" (show_unop op) (show_operand operand_value)
  | BinOp (op, left, right) ->
      Printf.sprintf "%s %s %s" (show_operand left) (show_binop op)
        (show_operand right)
  | Cast operand_value ->
      Printf.sprintf "cast %s to %s"
        (show_operand operand_value)
        (show_ty value.val_ty)
  | AddressOf source -> "address_of " ^ show_place source
  | Len source -> "len " ^ show_place source
  | DataPtr source -> "data_ptr " ^ show_place source
  | SizeOf ty -> "sizeof " ^ show_ty ty

let show_callee callee =
  match callee with
  | Direct name -> "@" ^ name
  | Indirect value -> show_operand value

let show_stmt stmt =
  match stmt.stmt_desc with
  | Assign (destination, assigned) ->
      Printf.sprintf "%s = %s" (show_place destination) (show_value assigned)
  | Call call ->
      let destination =
        match call.call_dest with
        | None -> ""
        | Some value -> show_place value ^ " = "
      in
      Printf.sprintf "%scall %s(%s)" destination
        (show_callee call.call_callee)
        (String.concat ", " (List.map show_operand call.call_args))
  | Slice (destination, source, lo, hi) ->
      Printf.sprintf "%s = slice %s %s %s" (show_place destination)
        (show_place source) (show_operand lo) (show_operand hi)

let show_terminator value =
  match value with
  | None -> "<missing terminator>"
  | Some { term_desc = Jump target; _ } -> Printf.sprintf "jump block%d" target
  | Some { term_desc = Branch (condition, yes, no); _ } ->
      Printf.sprintf "branch %s block%d block%d" (show_operand condition) yes no
  | Some { term_desc = ReturnValue None; _ } -> "return"
  | Some { term_desc = ReturnValue (Some value); _ } ->
      "return " ^ show_operand value
  | Some { term_desc = Unreachable; _ } -> "unreachable"

let show_function func =
  let buffer = Buffer.create 256 in
  let params =
    func.func_params
    |> List.map (fun id ->
        Printf.sprintf "%%%d: %s" id (show_ty func.func_locals.(id).local_ty))
    |> String.concat ", "
  in
  let ret_type =
    match func.func_ret with TUnit -> "" | ty -> " " ^ show_ty ty
  in
  Printf.bprintf buffer "fn %s(%s)%s {\n" func.func_name params ret_type;
  Array.iteri
    (fun id local ->
      let name =
        match local.local_name with None -> "" | Some name -> " " ^ name
      in
      Printf.bprintf buffer "  local %%%d%s: %s %s\n" id name
        (show_ty local.local_ty)
        (show_storage local.local_storage))
    func.func_locals;
  if Array.length func.func_locals > 0 then Buffer.add_char buffer '\n';
  Array.iteri
    (fun id block ->
      Printf.bprintf buffer "  block%d:\n" id;
      List.iter
        (fun item -> Printf.bprintf buffer "    %s\n" (show_stmt item))
        block.block_stmts;
      Printf.bprintf buffer "    %s\n" (show_terminator block.block_term);
      if id + 1 < Array.length func.func_blocks then Buffer.add_char buffer '\n')
    func.func_blocks;
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let dump (program : program) =
  String.concat ""
    (List.map
       (fun g ->
         Printf.sprintf "global %s: %s\n" g.global_name (show_ty g.global_ty))
       program.prog_globals
    @ (if List.is_empty program.prog_globals then [] else [ "\n" ])
    @ List.mapi
        (fun index function_ ->
          (if index = 0 then "" else "\n") ^ show_function function_)
        program.prog_funcs)

let fail ctx span fmt =
  Printf.ksprintf
    (fun message -> Diagnostic.ice ~span (ctx.func.func_name ^ ": " ^ message))
    fmt

let expect ctx span what want got =
  if not (ty_equal want got) then
    fail ctx span "%s is %s but should be %s" what (show_ty got) (show_ty want)

let expect_int ctx span what ty =
  match resolve_ty ty with
  | TInt _ -> ()
  | _ -> fail ctx span "%s is %s but should be an integer" what (show_ty ty)

let elem_of ctx span ty =
  match resolve_ty ty with
  | TArray (inner, _) | TSlice inner -> inner
  | TStr -> TInt U8
  | _ -> fail ctx span "%s has no elements" (show_ty ty)

(* local %0 value: i32 user *)
let local_of ctx span id =
  if id < 0 || id >= Array.length ctx.func.func_locals then
    fail ctx span "local %d does not exist" id;
  ctx.func.func_locals.(id)

let constant_fits layouts ty constant =
  match (constant, resolve_ty ty) with
  | Int _, TAdt _ -> not (Layout.is_aggregate layouts ty)
  | Int _, TInt _
  | Float _, TFloat _
  | Bool _, TBool
  | Null, (TNull | TPointer _ | TPtr)
  | CStr _, (TCStr | TPointer (TInt I8))
  | Char _, TChar
  | Function _, (TFunc _ | TPointer (TFunc _))
  | Str _, TStr
  | (Zero | Undef), _ ->
      true
  | _ -> false

(* copy %0, 42 *)
let rec verify_operand ctx operand =
  (match operand.op_desc with
  | Const constant ->
      if not (constant_fits ctx.layouts operand.op_ty constant) then
        fail ctx operand.op_span "constant %s can't be %s"
          (show_constant constant) (show_ty operand.op_ty)
  | Copy source ->
      expect ctx operand.op_span "operand" (verify_place ctx source)
        operand.op_ty);
  operand.op_ty

and verify_projection ctx span ty = function
  | Deref -> (
      match resolve_ty ty with
      | TPointer inner -> inner
      | _ -> fail ctx span "deref projection requires a pointer")
  | Field (variant, field) -> (
      match Layout.named_aggregate ctx.layouts ty with
      | Some name -> Layout.field_ty ctx.layouts name variant field
      | None -> fail ctx span "field projection requires fields")
  | Tag ->
      if not (Layout.has_payload ctx.layouts ty) then
        fail ctx span "tag projection requires a payload enum";
      Layout.tag_ty
  | Index index -> (
      expect_int ctx index.op_span "index" (verify_operand ctx index);
      match resolve_ty ty with
      | TArray (inner, _) | TSlice inner | TPointer inner -> inner
      | _ -> fail ctx span "index projection requires indexed storage")

(* %0, @global, %0.deref.field0[copy %1] *)
and verify_place ctx place =
  let span = place.place_span in
  let base =
    match place.place_base with
    | Local id -> (local_of ctx span id).local_ty
    | Global name -> (
        match Hashtbl.find_opt ctx.globals name with
        | Some ty -> ty
        | None -> fail ctx span "global %s does not exist" name)
  in
  List.fold_right
    (fun p ty -> verify_projection ctx span ty p)
    place.place_projs base

(* copy %0, copy %0 + 1, address_of %0, len %0 *)
let verify_value ctx span value =
  let same o = expect ctx span "operand" value.val_ty (verify_operand ctx o) in
  match value.val_desc with
  | Use o | UnOp ((Neg | BitNot), o) -> same o
  | UnOp (Not, o) ->
      expect ctx span "not" TBool value.val_ty;
      same o
  | BinOp ((Eq | Neq | Lt | Gt | Lte | Gte), left, right) ->
      expect ctx span "comparison" TBool value.val_ty;
      expect ctx span "right operand" (verify_operand ctx left)
        (verify_operand ctx right)
  | BinOp ((Lshift | Rshift), left, right) ->
      same left;
      expect_int ctx span "shift count" (verify_operand ctx right)
  | BinOp (_, left, right) ->
      same left;
      same right
  | Cast o -> ignore (verify_operand ctx o)
  | AddressOf source ->
      expect ctx span "address"
        (TPointer (verify_place ctx source))
        value.val_ty
  | Len source ->
      ignore (elem_of ctx span (verify_place ctx source));
      expect ctx span "length" (TInt Usize) value.val_ty
  | DataPtr source ->
      let inner = elem_of ctx span (verify_place ctx source) in
      expect ctx span "data pointer" (TPointer inner) value.val_ty
  | SizeOf _ -> expect_int ctx span "size" value.val_ty

(* An extern never reaches MIR so its call is taken at its word *)
let signature_of ctx span call =
  match call.call_callee with
  | Direct name -> (
      match Hashtbl.find_opt ctx.signatures name with
      | Some (params, ret_ty) -> (Some params, ret_ty)
      | None -> (None, call.call_ret))
  | Indirect callee -> (
      match resolve_ty (verify_operand ctx callee) with
      | TFunc { sig_params; sig_ret; _ } ->
          (Some (List.filter is_valued sig_params), sig_ret)
      | ty -> fail ctx span "callee is %s but should be a function" (show_ty ty)
      )

(* %0 = call @add(copy %1), call @puts(copy %1) *)
let verify_call ctx span call =
  let params, ret_ty = signature_of ctx span call in
  expect ctx span "call" ret_ty call.call_ret;
  let args = List.map (verify_operand ctx) call.call_args in
  let fixed =
    match call.call_variadic_start with
    | Some n -> List.filteri (fun i _ -> i < n) args
    | None -> args
  in
  Option.iter
    (fun params ->
      if List.compare_lengths params fixed <> 0 then
        fail ctx span "call passes %d arguments but takes %d"
          (List.length fixed) (List.length params);
      List.iter2 (expect ctx span "argument") params fixed)
    params;
  match call.call_dest with
  | Some destination ->
      if not (is_valued ret_ty) then
        fail ctx span "unit call has result storage";
      expect ctx span "call storage" ret_ty (verify_place ctx destination)
  | None -> if is_valued ret_ty then fail ctx span "call has no result storage"

(* %0 = copy %1, %0 = slice %1 1 3, %0 = call @add(copy %1) *)
let verify_stmt ctx stmt =
  let span = stmt.stmt_span in
  match stmt.stmt_desc with
  | Assign (destination, assigned) ->
      verify_value ctx span assigned;
      expect ctx span "assignment"
        (verify_place ctx destination)
        assigned.val_ty
  | Slice (destination, source, lo, hi) ->
      let source_ty = verify_place ctx source in
      let sliced =
        match resolve_ty source_ty with
        | TStr -> TStr
        | _ -> TSlice (elem_of ctx span source_ty)
      in
      expect ctx span "slice" sliced (verify_place ctx destination);
      expect_int ctx span "slice start" (verify_operand ctx lo);
      expect_int ctx span "slice end" (verify_operand ctx hi)
  | Call call -> verify_call ctx span call

(* block0 *)
let verify_target ctx span id =
  if id < 0 || id >= Array.length ctx.func.func_blocks then
    fail ctx span "block %d does not exist" id

(* jump block1, branch %0 block1 block2, return %0, unreachable *)
let verify_terminator ctx terminator =
  let span = terminator.term_span in
  match terminator.term_desc with
  | Jump target -> verify_target ctx span target
  | Branch (condition, yes, no) ->
      expect ctx span "condition" TBool (verify_operand ctx condition);
      verify_target ctx span yes;
      verify_target ctx span no
  | ReturnValue returned -> (
      match (returned, ctx.func.func_result) with
      | Some _, Some _ ->
          fail ctx span "return has a value but the result is storage"
      | None, Some _ -> ()
      | Some returned, None ->
          expect ctx span "return" ctx.func.func_ret
            (verify_operand ctx returned)
      | None, None ->
          if is_valued ctx.func.func_ret then
            fail ctx span "return has no value for %s"
              (show_ty ctx.func.func_ret))
  | Unreachable -> ()

(* fn add(%0: i32, %1: i32) i32 { ... } *)
let verify_func ctx =
  let func = ctx.func in
  Array.iter
    (fun local ->
      if has_error local.local_ty then
        fail ctx local.local_span "local has no type")
    func.func_locals;
  List.iter
    (fun id ->
      if (local_of ctx func.func_span id).local_storage <> Param then
        fail ctx func.func_span "local %d is not a param" id)
    func.func_params;
  Option.iter
    (fun id ->
      let result = local_of ctx func.func_span id in
      if result.local_storage <> Result then
        fail ctx func.func_span "local %d is not the result" id;
      expect ctx func.func_span "result" func.func_ret result.local_ty)
    func.func_result;
  Array.iteri
    (fun id block ->
      List.iter (verify_stmt ctx) block.block_stmts;
      match block.block_term with
      | Some t -> verify_terminator ctx t
      | None -> fail ctx func.func_span "block %d has no terminator" id)
    func.func_blocks

let verify program =
  let layouts = Layout.create () in
  List.iter
    (fun adt ->
      Layout.set_variants layouts (Keyname.key adt.adt_name) adt.adt_kind
        adt.adt_variants)
    program.prog_adts;
  let globals =
    List.to_seq program.prog_globals
    |> Seq.map (fun g -> (g.global_name, g.global_ty))
    |> Hashtbl.of_seq
  in
  let signatures =
    List.to_seq program.prog_funcs
    |> Seq.map (fun f ->
        let params =
          List.map (fun id -> f.func_locals.(id).local_ty) f.func_params
        in
        (f.func_name, (params, f.func_ret)))
    |> Hashtbl.of_seq
  in
  List.iter
    (fun func -> verify_func { layouts; globals; signatures; func })
    program.prog_funcs
