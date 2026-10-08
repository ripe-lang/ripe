(* SPDX-License-Identifier: Apache-2.0 *)

open Types
open! Tast

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
  base : place_base;
  projections : projection list;
  place_span : Ast.span;
}

and projection = Deref | Field of int | Index of operand
and operand = { desc : operand_desc; ty : Types.ty; span : Ast.span }
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

type value = { desc : value_desc; ty : Types.ty }

and value_desc =
  | Use of operand
  | Unary of unop * operand
  | Binary of binop * operand * operand
  | Cast of operand
  | AddressOf of place
  | Len of place
  | DataPtr of place
  | SizeOf of Types.ty

(* TODO(b597): This needs one value for a whole aggregate *)

type call_kind = Internal | External
type callee = Direct of string | Indirect of operand

type call = {
  destination : place option;
  callee : callee;
  kind : call_kind;
  args : operand list;
  return_ty : Types.ty;
  variadic_start : int option;
}

type check =
  | Bounds of operand * operand
  | SliceBounds of operand * operand * operand
  | Null of operand
  | DivZero of operand
  | NegativeShift of operand

type statement = { desc : statement_desc; span : Ast.span }

and statement_desc =
  | Assign of place * value
  | Call of call
  | Slice of place * place * operand * operand

type terminator = { desc : terminator_desc; span : Ast.span }

and terminator_desc =
  | Jump of block_id
  | Branch of operand * block_id * block_id
  | Check of check * block_id
  | ReturnValue of operand option
  | Unreachable

type local = {
  name : string option;
  ty : Types.ty;
  storage : storage_kind;
  span : Ast.span;
}

type block = { statements : statement list; terminator : terminator option }

type func = {
  name : string;
  source_name : string;
  public : bool;
  abi : Types.func_abi;
  params : local_id list;
  result : local_id option;
  locals : local array;
  blocks : block array;
  return_ty : Types.ty;
  entry_point : bool;
  span : Ast.span;
}

type struct_decl = { name : Qname.t; fields : Types.ty list; local : bool }

type global_value =
  | GlobalConst of constant * Types.ty
  | GlobalAddress of string
  | GlobalArray of global_value list
  | GlobalStruct of (int * global_value) list

type global = {
  name : string;
  ty : Types.ty;
  init : global_value option;
  public : bool;
}

type program = {
  structs : struct_decl list;
  globals : global list;
  functions : func list;
}

type loop_context = {
  label : Ast.name option;
  continue_block : block_id;
  break_block : block_id;
  result : (place * Types.ty) option;
}

type builder = {
  layouts : Layout.t;
  global_names : (Symbol.key, string) Hashtbl.t;
  locals : local Dynarray.t;
  symbols : (Symbol.id, local_id) Hashtbl.t;
  blocks : block Dynarray.t;
  mutable current : block_id;
  mutable loops : loop_context list;
  mutable result : local_id option;
}

type context = {
  structs : ty array Symbol.Table.t;
  globals : (string, ty) Hashtbl.t;
  signatures : (string, ty list * ty) Hashtbl.t;
  func : func;
}

let make_builder layouts global_names =
  {
    layouts;
    global_names;
    locals = Dynarray.create ();
    symbols = Hashtbl.create 16;
    blocks = Dynarray.make 1 { statements = []; terminator = None };
    current = 0;
    loops = [];
    result = None;
  }

let finish_blocks (state : builder) =
  Dynarray.to_array state.blocks
  |> Array.map (fun (b : block) ->
      { b with statements = List.rev b.statements })

let add_local (state : builder) name storage ty span =
  let id = Dynarray.length state.locals in
  Dynarray.add_last state.locals { name; ty; storage; span };
  id

let declare state (symbol : Symbol.t) storage ty span =
  let id = add_local state (Some symbol.name) storage ty span in
  Hashtbl.add state.symbols symbol.id id;
  id

let new_block (state : builder) =
  let id = Dynarray.length state.blocks in
  Dynarray.add_last state.blocks { statements = []; terminator = None };
  id

let current_block (state : builder) = Dynarray.get state.blocks state.current
let is_live state = Option.is_none (current_block state).terminator
let switch state block = state.current <- block

let emit (state : builder) desc span =
  let b = current_block state in
  if Option.is_none b.terminator then
    Dynarray.set state.blocks state.current
      { b with statements = { desc; span } :: b.statements }

let terminate (state : builder) desc span =
  let b = current_block state in
  if Option.is_none b.terminator then
    Dynarray.set state.blocks state.current
      { b with terminator = Some { desc; span } }

let loop_target state label span =
  let target =
    match (label, state.loops) with
    | None, loop :: _ -> Some loop
    | None, [] -> None
    | Some label, loops ->
        List.find_opt (fun loop -> loop.label = Some label.Ast.value) loops
  in
  match target with
  | Some target -> target
  | None -> Diagnostic.ice ~span "loop target does not exist"

let local_place span id =
  { base = Local id; projections = []; place_span = span }

let add_projection place projection =
  { place with projections = projection :: place.projections }

let copy span ty place : operand = { desc = Copy place; ty; span }
let const span ty desc : operand = { desc = Const desc; ty; span }

let new_temp (state : builder) span ty =
  local_place span (add_local state None Temp ty span)

let store state destination ty span desc =
  emit state (Assign (destination, { desc; ty })) span

let assign state destination (assigned : operand) =
  store state destination assigned.ty assigned.span (Use assigned)

let temp_value state ty span desc =
  let destination = new_temp state span ty in
  store state destination ty span desc;
  copy span ty destination

let materialize state (operand : operand) =
  match operand.desc with
  | Copy place -> place
  | Const _ ->
      let destination = new_temp state operand.span operand.ty in
      assign state destination operand;
      destination

let symbol_place (state : builder) span (symbol : Symbol.t) =
  match Hashtbl.find_opt state.symbols symbol.id with
  | Some id -> local_place span id
  | None -> (
      match Hashtbl.find_opt state.global_names (Symbol.key symbol) with
      | Some name -> { base = Global name; projections = []; place_span = span }
      | None ->
          Diagnostic.ice ~span
            (Printf.sprintf "no MIR place for symbol %s" symbol.name))

(* TODO(7a4f): This is temporary until global inits fold again *)
let constant_of (expr : Tast.texpr) =
  match expr.desc with
  (* The MIR keeps the value but not the variant name *)
  | Tast.TInt value | Tast.TVariant (_, value) -> Int value
  | Tast.TFloat value -> Float value
  | Tast.TBool value -> Bool value
  | Tast.TNull -> Null
  | Tast.TCStr value -> CStr value
  | Tast.TStr value -> Str value
  | Tast.TChar value -> Char value
  | Tast.TZero -> Zero
  | Tast.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
      Function symbol.Symbol.link_name
  | _ ->
      raise
        (Diagnostic.Errors
           [ Diagnostic.error expr.span "global initializer must be a literal" ])

(* The surviving source operators keep their meaning *)
let unop_of op =
  match op with
  | Ast.Neg -> Neg
  | Ast.Not -> Not
  | Ast.BitNot -> BitNot
  | Ast.Deref -> Diagnostic.ice "deref is a projection and not a MIR value"
  | Ast.AddressOf -> Diagnostic.ice "address of is its own MIR value"

let binop_of op =
  match op with
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

let is_valued ty = ty <> Types.TUnit && ty <> Types.TNever

(* check_bounds copy %1 copy %2 block1 *)
let lower_check state check span =
  if is_live state then begin
    let ok = new_block state in
    terminate state (Check (check, ok)) span;
    switch state ok
  end

(* A zero width pointee touches no memory *)
(* check_null copy %0 block1 *)
let lower_null_check (state : builder) pointee pointer span =
  if Layout.ty_size state.layouts pointee > 0 then
    lower_check state (Null pointer) span

(* Plain and compound arithmetic guard the same two operators the same way *)
(* check_div_zero copy %1 block1, check_negative_shift copy %1 block1 *)
let lower_arith_check state op left_ty right span =
  match op with
  | (Ast.Div | Ast.Mod) when not (is_float left_ty) ->
      lower_check state (DivZero right) span
  | (Ast.Lshift | Ast.Rshift) when not (is_unsigned right.ty) ->
      lower_check state (NegativeShift right) span
  | _ -> ()

(* The minimum integer and negative one need no hardware divide *)
(* %3 = copy %1 == -1
   branch copy %3 block1 block2
   block1:
     %2 = -copy %0
     jump block3
   block2:
     %2 = copy %0 / copy %1
     jump block3 *)
let lower_div state destination span ty op (left : operand) (right : operand) =
  let negative_one = const span right.ty (Int (-1L)) in
  let divisor_is_negative_one =
    temp_value state Types.TBool span (Binary (Eq, right, negative_one))
  in
  let wrap_block = new_block state in
  let divide_block = new_block state in
  let join_block = new_block state in
  terminate state
    (Branch (divisor_is_negative_one, wrap_block, divide_block))
    span;
  switch state wrap_block;
  if op = Mod then assign state destination (const span ty (Int 0L))
  else store state destination ty span (Unary (Neg, left));
  terminate state (Jump join_block) span;
  switch state divide_block;
  store state destination ty span (Binary (op, left, right));
  terminate state (Jump join_block) span;
  switch state join_block

(* A count past the width drains every bit *)
(* %3 = copy %1 < 32
   branch copy %3 block1 block2
   block1:
     %2 = copy %0 << copy %1
     jump block3
   block2:
     %2 = 0
     jump block3 *)
let lower_shift state destination span ty op (left : operand) (right : operand)
    =
  let bits = 8 * int_kind_size (int_kind_of ty) in
  let width = const span right.ty (Int (Int64.of_int bits)) in
  let count_in_range =
    temp_value state Types.TBool span (Binary (Lt, right, width))
  in
  let shift_block = new_block state in
  let drained_block = new_block state in
  let join_block = new_block state in
  terminate state (Branch (count_in_range, shift_block, drained_block)) span;
  switch state shift_block;
  store state destination ty span (Binary (op, left, right));
  terminate state (Jump join_block) span;
  switch state drained_block;
  (* A signed right shift settles at the top bit *)
  if op = Rshift && not (is_unsigned ty) then
    let top = const span right.ty (Int (Int64.of_int (bits - 1))) in
    store state destination ty span (Binary (Rshift, left, top))
  else assign state destination (const span ty (Int 0L));
  terminate state (Jump join_block) span;
  switch state join_block

(* %2 = copy %0 + copy %1 *)
let lower_binary state destination span ty (op : Ast.binop) (left : operand)
    (right : operand) =
  let lowered = binop_of op in
  match op with
  | _ when not (is_live state) -> ()
  | (Ast.Div | Ast.Mod) when div_int_needs_check left.ty ->
      lower_div state destination span ty lowered left right
  | Ast.Lshift | Ast.Rshift ->
      lower_shift state destination span ty lowered left right
  | _ -> store state destination ty span (Binary (lowered, left, right))

(* %1 = call @g()
   %0 = 3 *)
let rec lower_block state destination body =
  match List.rev body with
  | [] -> ()
  | last :: reversed ->
      List.iter (lower_statement state) (List.rev reversed);
      if is_live state then lower_into state destination last

(* branch copy %0 block1 block2
   block1:
     %2 = copy %1
     jump block3
   block2:
     %2 = false
     jump block3 *)
and lower_short_circuit state destination (expr : Tast.texpr) left right
    short_value =
  let right_block = new_block state in
  let short_block = new_block state in
  let join_block = new_block state in
  lower_cond state left
    (if short_value then short_block else right_block)
    (if short_value then right_block else short_block);
  switch state short_block;
  let short = const expr.span Types.TBool (Bool short_value) in
  Option.iter (fun d -> assign state d short) destination;
  terminate state (Jump join_block) expr.span;
  switch state right_block;
  lower_into state destination right;
  terminate state (Jump join_block) expr.span;
  switch state join_block

(* branch copy %0 block2 block3
   block2:
     %1 = 1
     jump block1
   block3:
     %1 = 2
     jump block1 *)
and lower_if state destination (expr : Tast.texpr) branches else_body =
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

(* One test per arm because a jump table only pays off on a dense range *)
(* %2 = copy %0 == 1
   branch copy %2 block3 block2
   block2:
     %3 = copy %0
     %1 = copy %3
     jump block1
   block3:
     %1 = 10
     jump block1 *)
and lower_match state destination (expr : Tast.texpr) (scrutinee : Tast.texpr)
    arms =
  (* The scrutinee is a place so a field pattern can project into it *)
  let subject = materialize state (lower_expr state scrutinee) in
  let join = new_block state in
  let ty = scrutinee.ty in
  let wanted value =
    match resolve_ty ty with
    | Types.TBool -> Bool (value <> 0L)
    | Types.TChar -> Char (Int64.to_int value)
    | _ -> Int value
  in
  let rec lower_arms = function
    | [] -> terminate state Unreachable expr.span
    | { tpat = Tast.TPatWild; tbody } :: _ ->
        lower_arm state destination expr join tbody
    | { tpat = Tast.TPatBind (symbol, bound_ty); tbody } :: _ ->
        let id = declare state symbol User bound_ty expr.span in
        assign state (local_place expr.span id)
          (copy expr.span bound_ty subject);
        lower_arm state destination expr join tbody
    | { tpat = Tast.TPatConst value; tbody } :: rest ->
        let no = new_block state in
        let yes = new_block state in
        let against = const expr.span ty (wanted value) in
        let found = copy expr.span ty subject in
        let equal =
          temp_value state Types.TBool expr.span (Binary (Eq, found, against))
        in
        terminate state (Branch (equal, yes, no)) expr.span;
        switch state yes;
        lower_arm state destination expr join tbody;
        switch state no;
        lower_arms rest
  in
  lower_arms arms;
  switch state join

(* %1 = 10
   jump block1 *)
and lower_arm state destination (expr : Tast.texpr) join body =
  lower_block state destination body;
  terminate state (Jump join) expr.span

(* branch copy %0 block1 block2, jump block1 *)
and lower_cond state (expr : Tast.texpr) yes no =
  match expr.desc with
  | Tast.TBool value ->
      terminate state (Jump (if value then yes else no)) expr.span
  | Tast.TUnOp (Ast.Not, inner) -> lower_cond state inner no yes
  | Tast.TBinOp (Ast.And, left, right) ->
      let middle = new_block state in
      lower_cond state left middle no;
      switch state middle;
      lower_cond state right yes no
  | Tast.TBinOp (Ast.Or, left, right) ->
      let middle = new_block state in
      lower_cond state left yes middle;
      switch state middle;
      lower_cond state right yes no
  | _ ->
      let condition = lower_expr state expr in
      if is_live state then
        terminate state (Branch (condition, yes, no)) expr.span

(* jump block1
   block1:
     %2 = copy %1 < copy %0
     branch copy %2 block2 block3
   block2:
     %1 = copy %1 + 1
     jump block1 *)
and lower_while state span label condition body =
  let condition_block = new_block state in
  let body_block = new_block state in
  let exit_block = new_block state in
  terminate state (Jump condition_block) span;
  switch state condition_block;
  lower_cond state condition body_block exit_block;
  switch state body_block;
  lower_loop_body state span label condition_block exit_block None body;
  switch state exit_block

(* The stack gives nested loop control the nearest matching target *)
(* %1 = copy %1 + 1
   jump block1 *)
and lower_loop_body state span label continue_block break_block result body =
  let label = Option.map (fun label -> label.Ast.value) label in
  state.loops <- { label; continue_block; break_block; result } :: state.loops;
  List.iter (lower_statement state) body;
  state.loops <- List.tl state.loops;
  if is_live state then terminate state (Jump continue_block) span

(* An inclusive loop checks the limit before stepping so it can't overflow *)
(* jump block1
   block1:
     %3 = copy %1 < copy %2
     branch copy %3 block2 block4
   block3:
     %4 = copy %1 + 1
     %1 = copy %4
     jump block1 *)
and lower_counted_loop state span label op ty counter limit element body =
  let condition_block = new_block state in
  let body_block = new_block state in
  let step_block = new_block state in
  let exit_block = new_block state in
  let current = copy span ty counter in
  let limit = copy span ty limit in
  terminate state (Jump condition_block) span;
  switch state condition_block;
  let in_range =
    temp_value state Types.TBool span (Binary (op, current, limit))
  in
  terminate state (Branch (in_range, body_block, exit_block)) span;
  switch state body_block;
  (match element with
  | Some (destination, value) -> assign state destination value
  | None -> ());
  lower_loop_body state span label step_block exit_block None body;
  switch state step_block;
  if op = Lte then begin
    let increment_block = new_block state in
    let last =
      temp_value state Types.TBool span (Binary (Eq, current, limit))
    in
    terminate state (Branch (last, exit_block, increment_block)) span;
    switch state increment_block
  end;
  let one = const span ty (Int 1L) in
  assign state counter (temp_value state ty span (Binary (Add, current, one)));
  terminate state (Jump condition_block) span;
  switch state exit_block

and lower_for state span label symbol elem_ty (iter : Tast.texpr) body =
  match iter.desc with
  | Tast.TRange (lo, hi) ->
      lower_range_for state span label symbol elem_ty lo hi Lt body
  | Tast.TRangeInclusive (lo, hi) ->
      lower_range_for state span label symbol elem_ty lo hi Lte body
  | _ -> lower_each_for state span label symbol elem_ty iter body

(* %1 = 0
   %2 = 3 *)
and lower_range_for state span label symbol elem_ty lo hi op body =
  let loop_id = declare state symbol User elem_ty symbol.Symbol.span in
  let lo = lower_expr state lo in
  let hi = lower_expr state hi in
  let counter = local_place span loop_id in
  assign state counter lo;
  let limit =
    local_place span (add_local state (Some "for.hi") Temp elem_ty hi.span)
  in
  assign state limit hi;
  lower_counted_loop state span label op elem_ty counter limit None body

(* %6 = data_ptr %0
   %7 = len %0
   %2 = copy %6
   %3 = copy %7
   %4 = 0
   ...
   %5 = copy %2[copy %4] *)
and lower_each_for state span label symbol elem_ty iter body =
  let usize = Types.TInt Usize in
  let source = lower_expr state iter |> materialize state in
  let source =
    match resolve_ty iter.ty with
    | Types.TSlice _ ->
        let snapshot = new_temp state iter.span iter.ty in
        assign state snapshot (copy iter.span iter.ty source);
        snapshot
    | _ -> source
  in
  let pointer_ty = Types.TPointer elem_ty in
  let pointer_place = new_temp state span pointer_ty in
  let length_place = new_temp state span usize in
  let index = new_temp state span usize in
  let loop_id = declare state symbol User elem_ty symbol.Symbol.span in
  let pointer = temp_value state pointer_ty span (DataPtr source) in
  let length = temp_value state usize span (Len source) in
  assign state pointer_place pointer;
  assign state length_place length;
  assign state index (const span usize (Int 0L));
  let at_index = add_projection pointer_place (Index (copy span usize index)) in
  let element = (local_place span loop_id, copy span elem_ty at_index) in
  lower_counted_loop state span label Lt usize index length_place (Some element)
    body

(* jump block1
   block1:
     %2 = copy %0 == 1
     branch copy %2 block4 block5
   block4:
     %1 = 1
     jump block2 *)
and lower_loop state destination span label ty body =
  let body_block = new_block state in
  let exit_block = new_block state in
  terminate state (Jump body_block) span;
  switch state body_block;
  let result = Option.map (fun place -> (place, ty)) destination in
  lower_loop_body state span label body_block exit_block result body;
  switch state exit_block

(* copy %0, 1 *)
and lower_arg state expr =
  let value : operand = lower_expr state expr in
  if value.ty = Types.TUnit then None
  else if is_aggregate value.ty then
    Some (copy value.span value.ty (materialize state value))
  else Some value

(* Literals and place reads come back as is and the rest lands in a temp *)
(* 1, copy %0, copy %0.deref.field0, copy %1 *)
and lower_expr state (expr : Tast.texpr) =
  match expr.desc with
  | Tast.TIdent _ when expr.ty = Types.TUnit -> const expr.span expr.ty Undef
  | Tast.TInt _ | Tast.TVariant _ | Tast.TFloat _ | Tast.TBool _ | Tast.TNull
  | Tast.TCStr _ | Tast.TStr _ | Tast.TChar _ | Tast.TZero ->
      const expr.span expr.ty (constant_of expr)
  | Tast.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
      const expr.span expr.ty (constant_of expr)
  | Tast.TIdent symbol ->
      copy expr.span expr.ty (symbol_place state expr.span symbol)
  | Tast.TUnOp (Ast.Deref, _) | Tast.TFieldAccess _ | Tast.TIndex _ ->
      copy expr.span expr.ty (lower_place state expr)
  | _ when is_valued expr.ty ->
      let slot = new_temp state expr.span expr.ty in
      lower_into state (Some slot) expr;
      copy expr.span expr.ty slot
  | _ ->
      lower_into state None expr;
      const expr.span expr.ty Undef

(* The destination is always fresh so writing into it early can't clobber a
   read *)
(* %1 = -copy %0, %1 = cast copy %0 to i64, %2 = address_of %1, %1 = len %0 *)
and lower_into state destination (expr : Tast.texpr) =
  let slot () =
    match destination with
    | Some slot -> slot
    | None -> new_temp state expr.span expr.ty
  in
  let store_value desc =
    Option.iter (fun d -> store state d expr.ty expr.span desc) destination
  in
  (match expr.desc with
  | Tast.TErrorExpr ->
      Diagnostic.ice ~span:expr.span "error expression reached MIR"
  | Tast.TRange _ | Tast.TRangeInclusive _ ->
      Diagnostic.ice ~span:expr.span "range outside a for loop"
  | Tast.TCall (callee, args, variadic_start) ->
      let destination = if is_valued expr.ty then Some (slot ()) else None in
      lower_call state destination expr callee args variadic_start
  | Tast.TBinOp (Ast.And, left, right) ->
      lower_short_circuit state destination expr left right false
  | Tast.TBinOp (Ast.Or, left, right) ->
      lower_short_circuit state destination expr left right true
  | Tast.TBinOp (op, left, right) ->
      let left = lower_expr state left in
      let right = lower_expr state right in
      lower_arith_check state op left.ty right expr.span;
      Option.iter
        (fun d -> lower_binary state d expr.span expr.ty op left right)
        destination
  | Tast.TAssign (None, left, right) when left.ty = Types.TUnit ->
      lower_into state None right
  | Tast.TAssign (None, left, right) ->
      let assigned = lower_expr state right in
      assign state (lower_place state left) assigned
  | Tast.TAssign (Some op, left, right) ->
      lower_compound_assign state expr op left right
  | Tast.TUnOp (Ast.AddressOf, { desc = Tast.TUnOp (Ast.Deref, inner); _ }) ->
      lower_into state destination inner
  | Tast.TUnOp (Ast.AddressOf, ({ desc = Tast.TIdent symbol; _ } as inner))
    when Symbol.is_func symbol.Symbol.kind ->
      let function_value = const expr.span expr.ty (constant_of inner) in
      Option.iter (fun d -> assign state d function_value) destination
  | Tast.TUnOp (Ast.AddressOf, inner) ->
      store_value (AddressOf (lower_place state inner))
  | Tast.TUnOp (Ast.Deref, _)
  | Tast.TFieldAccess _ | Tast.TIndex _ | Tast.TIdent _ | Tast.TInt _
  | Tast.TVariant _ | Tast.TFloat _ | Tast.TBool _ | Tast.TNull | Tast.TCStr _
  | Tast.TStr _ | Tast.TChar _ | Tast.TZero ->
      let value = lower_expr state expr in
      Option.iter (fun d -> assign state d value) destination
  | Tast.TUnOp (op, inner) ->
      store_value (Unary (unop_of op, lower_expr state inner))
  | Tast.TCast inner -> store_value (Cast (lower_expr state inner))
  | Tast.TSizeOf ty -> store_value (SizeOf ty)
  | Tast.TLen inner ->
      store_value (Len (lower_expr state inner |> materialize state))
  | Tast.TDataPtr inner ->
      store_value (DataPtr (lower_expr state inner |> materialize state))
  | Tast.TArrayLit elements -> lower_array_lit state (slot ()) expr elements
  | Tast.TStructLit (_, fields) -> lower_struct_lit state (slot ()) expr fields
  | Tast.TSliceExpr (base, lo, hi) ->
      lower_slice state (slot ()) expr base lo hi
  | Tast.TLoop (label, body) ->
      lower_loop state destination expr.span label expr.ty body
  | Tast.TBlock body -> lower_block state destination body
  | Tast.TIf (branches, else_body) ->
      lower_if state destination expr branches else_body
  | Tast.TMatch (scrutinee, arms) ->
      lower_match state destination expr scrutinee arms
  | Tast.TBinding _ | Tast.TReturn _ | Tast.TBreak _ | Tast.TContinue _
  | Tast.TWhile _ | Tast.TFor _ ->
      lower_statement state expr
  | Tast.TLocalDecl | Tast.TUnit -> ());
  if expr.ty = Types.TNever then terminate state Unreachable expr.span

(* %0 = call @g(), call @puts(copy %1) *)
and lower_call state destination expr callee args variadic_start =
  let args = List.filter_map (lower_arg state) args in
  let kind =
    match resolve_ty callee.ty with
    | Types.TFunc (_, _, C) -> External
    | _ -> Internal
  in
  let callee =
    match callee.desc with
    | Tast.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
        Direct symbol.Symbol.link_name
    | _ -> Indirect (lower_expr state callee)
  in
  let call =
    { destination; callee; kind; args; return_ty = expr.ty; variadic_start }
  in
  emit state (Call call) expr.span

(* %0, @count, %0.deref.field0, %0[copy %1] *)
and lower_place state expr =
  match expr.desc with
  | Tast.TIdent symbol -> symbol_place state expr.span symbol
  | Tast.TUnOp (Ast.Deref, inner) -> lower_deref state expr.ty inner expr.span
  | Tast.TFieldAccess (base, field) ->
      let source =
        match resolve_ty base.ty with
        | Types.TPointer pointee -> lower_deref state pointee base base.span
        | _ -> lower_expr state base |> materialize state
      in
      add_projection source (Field field)
  | Tast.TIndex (base, index) ->
      let base_value = lower_expr state base in
      let source = materialize state base_value in
      let index = lower_expr state index in
      (match resolve_ty base.ty with
      | Types.TArray _ | Types.TSlice _ ->
          let length =
            temp_value state (Types.TInt Usize) base.span (Len source)
          in
          lower_check state (Bounds (index, length)) expr.span
      | Types.TPointer _ -> ()
      | _ ->
          let message = "index on non indexed MIR place" in
          Diagnostic.ice ~span:base.span message);
      add_projection source (Index index)
  | _ -> lower_expr state expr |> materialize state

(* check_null copy %0 block1
   block1:
     ... %0.deref *)
and lower_deref state pointee pointer span =
  let pointer = lower_expr state pointer in
  lower_null_check state pointee pointer span;
  add_projection (materialize state pointer) Deref

(* %0 = undef
   %0[0] = 7
   %0[1] = 8 *)
and lower_array_lit state destination (expr : Tast.texpr) elements =
  assign state destination (const expr.span expr.ty Undef);
  List.iteri
    (fun index (element : Tast.texpr) ->
      let index_operand =
        const element.span (Types.TInt Usize) (Int (Int64.of_int index))
      in
      let target = add_projection destination (Index index_operand) in
      lower_into state (Some target) element)
    elements

(* %0 = zero
   %0.field1 = call @side(1)
   %0.field0 = call @side(2) *)
and lower_struct_lit state destination (expr : Tast.texpr) fields =
  assign state destination (const expr.span expr.ty Zero);
  List.iter
    (fun (field, value) ->
      lower_into state (Some (add_projection destination (Field field))) value)
    fields

(* %2 = len %1
   check_slice_bounds 1 3 copy %2 block1
   block1:
     %0 = slice %1 1 3 *)
and lower_slice state destination expr base lo hi =
  let base = lower_expr state base |> materialize state in
  let lo = lower_expr state lo in
  let hi = lower_expr state hi in
  let length = temp_value state (Types.TInt Usize) expr.span (Len base) in
  lower_check state (SliceBounds (lo, hi, length)) expr.span;
  emit state (Slice (destination, base, lo, hi)) expr.span

(* %0[0] = copy %0[0] + 5 *)
and lower_compound_assign state (expr : Tast.texpr) op (left : Tast.texpr) right
    =
  let target = lower_place state left in
  let old = copy left.span left.ty target in
  let right = lower_expr state right in
  lower_arith_check state op old.ty right expr.span;
  lower_binary state target expr.span left.ty op old right

(* A later break can widen the loop type the earlier ones settled on *)
(* %1 = 1, %1 = cast copy %0 to i64 *)
and lower_break state (result, result_ty) (value : Tast.texpr) =
  if value.ty = Types.TNever || ty_equal value.ty result_ty then
    lower_into state (Some result) value
  else store state result result_ty value.span (Cast (lower_expr state value))

(* %1 = 0, return copy %1, jump block2 *)
and lower_statement state expr =
  if is_live state then
    match expr.desc with
    | Tast.TBinding (symbol, ty, init) when ty <> Types.TNever ->
        let id = declare state symbol User ty symbol.Symbol.span in
        let local = local_place expr.span id in
        lower_into state (if is_valued ty then Some local else None) init
    | Tast.TBinding (_, _, init) -> lower_into state None init
    | Tast.TReturn returned ->
        let returned =
          match (returned, state.result) with
          | Some value, Some result ->
              lower_into state (Some (local_place expr.span result)) value;
              None
          | Some value, None when is_valued value.ty ->
              Some (lower_expr state value)
          | Some value, None ->
              lower_into state None value;
              None
          | None, _ -> None
        in
        terminate state (ReturnValue returned) expr.span
    | Tast.TBreak (label, value) ->
        let target = loop_target state label expr.span in
        Option.iter
          (fun value ->
            match target.result with
            | Some result -> lower_break state result value
            | None -> lower_into state None value)
          value;
        terminate state (Jump target.break_block) expr.span
    | Tast.TContinue label ->
        let target = loop_target state label expr.span in
        terminate state (Jump target.continue_block) expr.span
    | Tast.TWhile (label, condition, body) ->
        lower_while state expr.span label condition body
    | Tast.TFor (label, symbol, elem_ty, iter, body) ->
        lower_for state expr.span label symbol elem_ty iter body
    | _ -> lower_into state None expr

let rec global_value_of (expr : Tast.texpr) =
  match expr.desc with
  | Tast.TUnOp (Ast.AddressOf, { desc = Tast.TIdent symbol; _ }) ->
      GlobalAddress symbol.Symbol.link_name
  | Tast.TArrayLit values -> GlobalArray (List.map global_value_of values)
  | Tast.TStructLit (_, fields) ->
      let compare_field_ids (left, _) (right, _) = Int.compare left right in
      GlobalStruct
        (List.map
           (fun (field, value) -> (field, global_value_of value))
           (List.sort compare_field_ids fields))
  | _ -> GlobalConst (constant_of expr, expr.ty)

let build_global (global : Tast.tglobal_def) =
  {
    name = global.name;
    ty = global.ty;
    init = Option.map global_value_of global.init;
    public = List.mem Ast.Pub global.modifiers;
  }

let build_func layouts global_names (func : Tast.tfunc_def) =
  let state = make_builder layouts global_names in
  let span =
    match (func.body, func.params) with
    | first :: _, _ -> first.span
    | [], (symbol, _) :: _ -> symbol.Symbol.span
    | [], [] -> Ast.dummy_span
  in
  (* A returned aggregate needs somewhere to live that outlives the frame *)
  (* TODO(73fc): A universal result slot would simplify inlining *)
  if Types.is_aggregate func.ret_ty then
    state.result <-
      Some (add_local state (Some "result") Result func.ret_ty span);
  let params =
    List.filter_map
      (fun (symbol, ty) ->
        if is_valued ty then
          Some (declare state symbol Param ty symbol.Symbol.span)
        else None)
      func.params
  in
  List.iter (lower_statement state) func.body;
  if is_live state then
    if func.ret_ty = Types.TUnit then terminate state (ReturnValue None) span
    else terminate state Unreachable span;
  {
    name = func.name;
    source_name = func.source_name;
    public = List.mem Ast.Pub func.modifiers;
    abi = func.abi;
    params;
    result = state.result;
    locals = Dynarray.to_array state.locals;
    blocks = finish_blocks state;
    return_ty = func.ret_ty;
    entry_point = func.entry_point;
    span;
  }

let build declarations =
  let structs, globals, funcs =
    List.fold_right
      (fun decl (structs, globals, funcs) ->
        match decl with
        | Tast.TStruct (name, fields, _) ->
            ({ name; fields; local = false } :: structs, globals, funcs)
        | Tast.TLocalStruct (name, fields) ->
            ({ name; fields; local = true } :: structs, globals, funcs)
        | Tast.TGlobal global when is_valued global.ty ->
            (structs, global :: globals, funcs)
        | Tast.TFunc func -> (structs, globals, func :: funcs)
        | _ -> (structs, globals, funcs))
      declarations ([], [], [])
  in
  let layouts = Layout.create () in
  List.iter
    (fun (decl : struct_decl) ->
      Layout.set_struct_fields layouts (Qname.key decl.name) decl.fields)
    structs;
  let global_names = Hashtbl.create 16 in
  List.iter
    (fun (g : Tast.tglobal_def) -> Hashtbl.add global_names g.key g.name)
    globals;
  {
    structs;
    globals = List.map build_global globals;
    functions = List.map (build_func layouts global_names) funcs;
  }

let show_storage storage =
  match storage with
  | Param -> "param"
  | User -> "user"
  | Temp -> "temp"
  | Result -> "result"

let show_constant constant =
  match constant with
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

let show_unop op = match op with Neg -> "-" | Not -> "!" | BitNot -> "~"

let show_binop op =
  match op with
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
    | Field field -> Printf.sprintf ".field%d" field
    | Index index -> Printf.sprintf "[%s]" (show_operand index)
  in
  let base =
    match value.base with
    | Local id -> Printf.sprintf "%%%d" id
    | Global name -> "@" ^ name
  in
  Printf.sprintf "%s%s" base
    (String.concat "" (List.map projection (List.rev value.projections)))

and show_operand (value : operand) =
  match value.desc with
  | Copy source -> "copy " ^ show_place source
  | Const value -> show_constant value

let show_value (value : value) =
  match value.desc with
  | Use operand_value -> show_operand operand_value
  | Unary (op, operand_value) ->
      Printf.sprintf "%s%s" (show_unop op) (show_operand operand_value)
  | Binary (op, left, right) ->
      Printf.sprintf "%s %s %s" (show_operand left) (show_binop op)
        (show_operand right)
  | Cast operand_value ->
      Printf.sprintf "cast %s to %s"
        (show_operand operand_value)
        (Types.show_ty value.ty)
  | AddressOf source -> "address_of " ^ show_place source
  | Len source -> "len " ^ show_place source
  | DataPtr source -> "data_ptr " ^ show_place source
  | SizeOf ty -> "sizeof " ^ Types.show_ty ty

let show_callee (callee : callee) =
  match callee with
  | Direct name -> "@" ^ name
  | Indirect value -> show_operand value

let show_statement (statement : statement) =
  match statement.desc with
  | Assign (destination, assigned) ->
      Printf.sprintf "%s = %s" (show_place destination) (show_value assigned)
  | Call call ->
      let destination =
        match call.destination with
        | None -> ""
        | Some value -> show_place value ^ " = "
      in
      Printf.sprintf "%scall %s(%s)" destination (show_callee call.callee)
        (String.concat ", " (List.map show_operand call.args))
  | Slice (destination, source, lo, hi) ->
      Printf.sprintf "%s = slice %s %s %s" (show_place destination)
        (show_place source) (show_operand lo) (show_operand hi)

let show_check (check : check) =
  match check with
  | Bounds (index, length) ->
      Printf.sprintf "bounds %s %s" (show_operand index) (show_operand length)
  | SliceBounds (lo, hi, length) ->
      Printf.sprintf "slice_bounds %s %s %s" (show_operand lo) (show_operand hi)
        (show_operand length)
  | Null pointer -> Printf.sprintf "null %s" (show_operand pointer)
  | DivZero divisor -> Printf.sprintf "div_zero %s" (show_operand divisor)
  | NegativeShift count ->
      Printf.sprintf "negative_shift %s" (show_operand count)

let show_terminator (value : terminator option) =
  match value with
  | None -> "<missing terminator>"
  | Some { desc = Jump target; _ } -> Printf.sprintf "jump block%d" target
  | Some { desc = Branch (condition, yes, no); _ } ->
      Printf.sprintf "branch %s block%d block%d" (show_operand condition) yes no
  | Some { desc = Check (check, ok); _ } ->
      Printf.sprintf "check_%s block%d" (show_check check) ok
  | Some { desc = ReturnValue None; _ } -> "return"
  | Some { desc = ReturnValue (Some value); _ } ->
      "return " ^ show_operand value
  | Some { desc = Unreachable; _ } -> "unreachable"

let show_function (func : func) =
  let buffer = Buffer.create 256 in
  let params =
    func.params
    |> List.map (fun id ->
        Printf.sprintf "%%%d: %s" id (Types.show_ty func.locals.(id).ty))
    |> String.concat ", "
  in
  let return_type =
    match func.return_ty with Types.TUnit -> "" | ty -> " " ^ Types.show_ty ty
  in
  Printf.bprintf buffer "fn %s(%s)%s {\n" func.name params return_type;
  Array.iteri
    (fun id (local : local) ->
      let name = match local.name with None -> "" | Some name -> " " ^ name in
      Printf.bprintf buffer "  local %%%d%s: %s %s\n" id name
        (Types.show_ty local.ty)
        (show_storage local.storage))
    func.locals;
  if Array.length func.locals > 0 then Buffer.add_char buffer '\n';
  Array.iteri
    (fun id (block : block) ->
      Printf.bprintf buffer "  block%d:\n" id;
      List.iter
        (fun item -> Printf.bprintf buffer "    %s\n" (show_statement item))
        block.statements;
      Printf.bprintf buffer "    %s\n" (show_terminator block.terminator);
      if id + 1 < Array.length func.blocks then Buffer.add_char buffer '\n')
    func.blocks;
  Buffer.add_string buffer "}\n";
  Buffer.contents buffer

let show_global (global : global) =
  Printf.sprintf "global %s: %s\n" global.name (Types.show_ty global.ty)

let dump (program : program) =
  String.concat ""
    (List.map show_global program.globals
    @ (if List.is_empty program.globals then [] else [ "\n" ])
    @ List.mapi
        (fun index function_ ->
          (if index = 0 then "" else "\n") ^ show_function function_)
        program.functions)

let fail ctx span fmt =
  Printf.ksprintf
    (fun message -> Diagnostic.ice ~span (ctx.func.name ^ ": " ^ message))
    fmt

let expect ctx span what want got =
  if not (ty_equal want got) then
    fail ctx span "%s is %s but should be %s" what (show_ty got) (show_ty want)

let expect_int ctx span what ty =
  match resolve_ty ty with
  | Types.TInt _ -> ()
  | _ -> fail ctx span "%s is %s but should be an integer" what (show_ty ty)

let element_of ctx span ty =
  match resolve_ty ty with
  | Types.TArray (inner, _) | Types.TSlice inner -> inner
  | Types.TStr -> Types.TInt U8
  | _ -> fail ctx span "%s has no elements" (show_ty ty)

(* local %0 value: i32 user *)
let local_of ctx span id =
  if id < 0 || id >= Array.length ctx.func.locals then
    fail ctx span "local %d does not exist" id;
  ctx.func.locals.(id)

let constant_fits ty constant =
  match (constant, resolve_ty ty) with
  | Int _, (Types.TInt _ | Types.TEnum _)
  | Float _, Types.TFloat _
  | Bool _, Types.TBool
  | Null, (Types.TNull | Types.TPointer _ | Types.TPtr)
  | CStr _, (Types.TCStr | Types.TPointer (Types.TInt I8))
  | Char _, Types.TChar
  | Function _, (Types.TFunc _ | Types.TPointer (Types.TFunc _))
  | Str _, Types.TStr
  | (Zero | Undef), _ ->
      true
  | _ -> false

(* copy %0, 42 *)
let rec verify_operand ctx (operand : operand) =
  (match operand.desc with
  | Const constant ->
      if not (constant_fits operand.ty constant) then
        fail ctx operand.span "constant %s can't be %s" (show_constant constant)
          (show_ty operand.ty)
  | Copy source ->
      expect ctx operand.span "operand" (verify_place ctx source) operand.ty);
  operand.ty

and verify_projection ctx span ty = function
  | Deref -> (
      match resolve_ty ty with
      | Types.TPointer inner -> inner
      | _ -> fail ctx span "deref projection requires a pointer")
  | Field field -> (
      match resolve_ty ty with
      | Types.TStruct (name, _) -> (
          match Symbol.Table.find_opt ctx.structs (Qname.key name) with
          | Some fields when field >= 0 && field < Array.length fields ->
              fields.(field)
          | Some _ -> fail ctx span "field projection %d does not exist" field
          | None -> fail ctx span "struct %s has no layout" (Qname.show name))
      | _ -> fail ctx span "field projection requires a struct")
  | Index index -> (
      expect_int ctx index.span "index" (verify_operand ctx index);
      match resolve_ty ty with
      | Types.TArray (inner, _) | Types.TSlice inner | Types.TPointer inner ->
          inner
      | _ -> fail ctx span "index projection requires indexed storage")

(* %0, @global, %0.deref.field0[copy %1] *)
and verify_place ctx (place : place) =
  let span = place.place_span in
  let base =
    match place.base with
    | Local id -> (local_of ctx span id).ty
    | Global name -> (
        match Hashtbl.find_opt ctx.globals name with
        | Some ty -> ty
        | None -> fail ctx span "global %s does not exist" name)
  in
  List.fold_right
    (fun p ty -> verify_projection ctx span ty p)
    place.projections base

(* copy %0, copy %0 + 1, address_of %0, len %0 *)
let verify_value ctx span (value : value) =
  let same o = expect ctx span "operand" value.ty (verify_operand ctx o) in
  match value.desc with
  | Use o | Unary ((Neg | BitNot), o) -> same o
  | Unary (Not, o) ->
      expect ctx span "not" Types.TBool value.ty;
      same o
  | Binary ((Eq | Neq | Lt | Gt | Lte | Gte), left, right) ->
      expect ctx span "comparison" Types.TBool value.ty;
      expect ctx span "right operand" (verify_operand ctx left)
        (verify_operand ctx right)
  | Binary ((Lshift | Rshift), left, right) ->
      same left;
      expect_int ctx span "shift count" (verify_operand ctx right)
  | Binary (_, left, right) ->
      same left;
      same right
  | Cast o -> ignore (verify_operand ctx o)
  | AddressOf source ->
      expect ctx span "address"
        (Types.TPointer (verify_place ctx source))
        value.ty
  | Len source ->
      ignore (element_of ctx span (verify_place ctx source));
      expect ctx span "length" (Types.TInt Usize) value.ty
  | DataPtr source ->
      let inner = element_of ctx span (verify_place ctx source) in
      expect ctx span "data pointer" (Types.TPointer inner) value.ty
  | SizeOf _ -> expect_int ctx span "size" value.ty

(* bounds copy %0 copy %1, null copy %0 *)
let verify_check ctx span = function
  | Bounds (index, length) ->
      expect_int ctx span "index" (verify_operand ctx index);
      expect ctx span "length" (Types.TInt Usize) (verify_operand ctx length)
  | SliceBounds (lo, hi, length) ->
      expect_int ctx span "slice start" (verify_operand ctx lo);
      expect_int ctx span "slice end" (verify_operand ctx hi);
      expect ctx span "length" (Types.TInt Usize) (verify_operand ctx length)
  | Null pointer -> (
      match resolve_ty (verify_operand ctx pointer) with
      | Types.TPointer _ | Types.TPtr | Types.TCStr -> ()
      | ty -> fail ctx span "null check on %s" (show_ty ty))
  | DivZero divisor ->
      expect_int ctx span "divisor" (verify_operand ctx divisor)
  | NegativeShift count ->
      expect_int ctx span "shift count" (verify_operand ctx count)

(* An extern never reaches MIR so its call is taken at its word *)
let signature_of ctx span (call : call) =
  match call.callee with
  | Direct name -> (
      match Hashtbl.find_opt ctx.signatures name with
      | Some (params, return_ty) -> (Some params, return_ty)
      | None -> (None, call.return_ty))
  | Indirect callee -> (
      match resolve_ty (verify_operand ctx callee) with
      | Types.TFunc (params, return_ty, _) ->
          (Some (List.filter is_valued params), return_ty)
      | ty -> fail ctx span "callee is %s but should be a function" (show_ty ty)
      )

(* %0 = call @add(copy %1), call @puts(copy %1) *)
let verify_call ctx span (call : call) =
  let params, return_ty = signature_of ctx span call in
  expect ctx span "call" return_ty call.return_ty;
  let args = List.map (verify_operand ctx) call.args in
  let fixed =
    match call.variadic_start with
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
  match call.destination with
  | Some destination ->
      if not (is_valued return_ty) then
        fail ctx span "unit call has result storage";
      expect ctx span "call storage" return_ty (verify_place ctx destination)
  | None ->
      if is_valued return_ty then fail ctx span "call has no result storage"

(* %0 = copy %1, %0 = slice %1 1 3, %0 = call @add(copy %1) *)
let verify_statement ctx (statement : statement) =
  let span = statement.span in
  match statement.desc with
  | Assign (destination, assigned) ->
      verify_value ctx span assigned;
      expect ctx span "assignment" (verify_place ctx destination) assigned.ty
  | Slice (destination, source, lo, hi) ->
      let source_ty = verify_place ctx source in
      let sliced =
        match resolve_ty source_ty with
        | Types.TStr -> Types.TStr
        | _ -> Types.TSlice (element_of ctx span source_ty)
      in
      expect ctx span "slice" sliced (verify_place ctx destination);
      expect_int ctx span "slice start" (verify_operand ctx lo);
      expect_int ctx span "slice end" (verify_operand ctx hi)
  | Call call -> verify_call ctx span call

(* block0 *)
let verify_target ctx span id =
  if id < 0 || id >= Array.length ctx.func.blocks then
    fail ctx span "block %d does not exist" id

(* jump block1, branch %0 block1 block2, return %0, unreachable *)
let verify_terminator ctx (terminator : terminator) =
  let span = terminator.span in
  match terminator.desc with
  | Jump target -> verify_target ctx span target
  | Branch (condition, yes, no) ->
      expect ctx span "condition" Types.TBool (verify_operand ctx condition);
      verify_target ctx span yes;
      verify_target ctx span no
  | Check (checked, ok) ->
      verify_check ctx span checked;
      verify_target ctx span ok
  | ReturnValue returned -> (
      match (returned, ctx.func.result) with
      | Some _, Some _ ->
          fail ctx span "return has a value but the result is storage"
      | None, Some _ -> ()
      | Some returned, None ->
          expect ctx span "return" ctx.func.return_ty
            (verify_operand ctx returned)
      | None, None ->
          if is_valued ctx.func.return_ty then
            fail ctx span "return has no value for %s"
              (show_ty ctx.func.return_ty))
  | Unreachable -> ()

(* fn add(%0: i32, %1: i32) i32 { ... } *)
let verify_func ctx =
  let func = ctx.func in
  Array.iter
    (fun (local : local) ->
      if Types.has_error local.ty then fail ctx local.span "local has no type")
    func.locals;
  List.iter
    (fun id ->
      if (local_of ctx func.span id).storage <> Param then
        fail ctx func.span "local %d is not a param" id)
    func.params;
  Option.iter
    (fun id ->
      let result = local_of ctx func.span id in
      if result.storage <> Result then
        fail ctx func.span "local %d is not the result" id;
      expect ctx func.span "result" func.return_ty result.ty)
    func.result;
  Array.iteri
    (fun id (block : block) ->
      List.iter (verify_statement ctx) block.statements;
      match block.terminator with
      | Some t -> verify_terminator ctx t
      | None -> fail ctx func.span "block %d has no terminator" id)
    func.blocks

let verify (program : program) =
  let structs = Symbol.Table.create 8 in
  List.iter
    (fun (decl : struct_decl) ->
      Symbol.Table.replace structs (Qname.key decl.name)
        (Array.of_list decl.fields))
    program.structs;
  let globals = Hashtbl.create 8 in
  List.iter
    (fun (global : global) -> Hashtbl.replace globals global.name global.ty)
    program.globals;
  let signatures = Hashtbl.create 16 in
  List.iter
    (fun (func : func) ->
      let params = List.map (fun id -> func.locals.(id).ty) func.params in
      Hashtbl.replace signatures func.name (params, func.return_ty))
    program.functions;
  List.iter
    (fun func -> verify_func { structs; globals; signatures; func })
    program.functions
