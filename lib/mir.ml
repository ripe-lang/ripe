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
  | Assert of check * block_id * block_id
  | Panic of check
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

type open_block = {
  mutable statements : statement list;
  mutable terminator : terminator option;
}

type loop_context = {
  label : Ast.name option;
  continue_block : block_id;
  break_block : block_id;
  result : (place * Types.ty) option;
}

type builder = {
  locals_rev : local list ref;
  next_local : int ref;
  symbols : (Symbol.id, local_id) Hashtbl.t;
  globals : (Symbol.key, string) Hashtbl.t;
  blocks : (block_id, open_block) Hashtbl.t;
  next_block : int ref;
  current : block_id ref;
  loops : loop_context list ref;
  struct_layouts : Layout.structs;
  bare_return_zero : bool;
  mutable result : local_id option;
}

type error = { function_name : string; error_span : Ast.span; message : string }

type context = {
  structs : ty array Symbol.Table.t;
  globals : (string, ty) Hashtbl.t;
}

type func_context = { program : context; func : func; errors : error list ref }

let build_struct_layouts (struct_decls : struct_decl list) =
  let struct_layouts = Layout.make_structs () in
  List.iter
    (fun (decl : struct_decl) ->
      Layout.set_struct_fields struct_layouts (Qname.key decl.name) decl.fields)
    struct_decls;
  struct_layouts

let constant_of_value = function
  | Constant.VFloat (value, _) -> Float value
  | Constant.VBool value -> Bool value
  | Constant.VChar value -> Char value
  | Constant.VInt _ as value -> Int (Constant.int_of value)

(* This is only temporary *)
let _keep_constant_of_value = constant_of_value

let rec global_init (expr : Tast.texpr) =
  match expr.desc with
  | Tast.TInt value -> GlobalConst (Int value, expr.ty)
  (* The MIR keeps the value but not the variant name *)
  | Tast.TVariant (_, value) -> GlobalConst (Int value, expr.ty)
  | Tast.TFloat value -> GlobalConst (Float value, expr.ty)
  | Tast.TBool value -> GlobalConst (Bool value, expr.ty)
  | Tast.TNull -> GlobalConst (Null, expr.ty)
  | Tast.TCStr value -> GlobalConst (CStr value, expr.ty)
  | Tast.TStr value -> GlobalConst (Str value, expr.ty)
  | Tast.TChar value -> GlobalConst (Char value, expr.ty)
  | Tast.TZero -> GlobalConst (Zero, expr.ty)
  | Tast.TUndef -> GlobalConst (Undef, expr.ty)
  | Tast.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
      GlobalConst (Function symbol.Symbol.link_name, expr.ty)
  | Tast.TUnOp (Ast.AddressOf, { desc = Tast.TIdent symbol; _ }) ->
      GlobalAddress symbol.Symbol.link_name
  | Tast.TArrayLit values -> GlobalArray (List.map global_init values)
  | Tast.TStructLit (_, fields) ->
      let compare_field_ids (left, _) (right, _) = Int.compare left right in
      GlobalStruct
        (List.map
           (fun (field, value) -> (field, global_init value))
           (List.sort compare_field_ids fields))
  | _ -> Diagnostic.ice ~span:expr.span "unsupported MIR global initializer"

let make_builder ~struct_layouts ~globals ~bare_return_zero =
  let blocks = Hashtbl.create 16 in
  let entry = 0 in
  Hashtbl.add blocks entry ({ statements = []; terminator = None } : open_block);
  {
    locals_rev = ref [];
    next_local = ref 0;
    symbols = Hashtbl.create 16;
    globals;
    blocks;
    next_block = ref 1;
    current = ref entry;
    loops = ref [];
    struct_layouts;
    bare_return_zero;
    result = None;
  }

let finish_blocks state =
  Array.init !(state.next_block) (fun id ->
      let b = Hashtbl.find state.blocks id in
      ({ statements = List.rev b.statements; terminator = b.terminator }
        : block))

let add_local state ?name storage ty span =
  let id = !(state.next_local) in
  incr state.next_local;
  state.locals_rev := { name; ty; storage; span } :: !(state.locals_rev);
  id

let finish_locals state = Array.of_list (List.rev !(state.locals_rev))
let bind_symbol state symbol id = Hashtbl.add state.symbols symbol.Symbol.id id

let new_block state =
  let id = !(state.next_block) in
  incr state.next_block;
  Hashtbl.add state.blocks id { statements = []; terminator = None };
  id

let current_block (state : builder) = Hashtbl.find state.blocks !(state.current)
let is_live state = Option.is_none (current_block state).terminator
let switch state block = state.current := block

let emit state desc span =
  let block = current_block state in
  if Option.is_none block.terminator then
    block.statements <- { desc; span } :: block.statements

let terminate state desc span =
  let block = current_block state in
  if Option.is_none block.terminator then
    block.terminator <- Some { desc; span }

(* The stack gives nested loop control the nearest matching target *)
let with_loop state label ~continue_block ~break_block ~result body =
  let label = Option.map (fun label -> label.Ast.value) label in
  state.loops :=
    { label; continue_block; break_block; result } :: !(state.loops);
  body ();
  state.loops :=
    match !(state.loops) with
    | _ :: loops -> loops
    | [] -> Diagnostic.ice "loop stack is empty"

let loop_target state label span =
  let target =
    match (label, !(state.loops)) with
    | None, loop :: _ -> Some loop
    | None, [] -> None
    | Some label, loops ->
        List.find_opt (fun loop -> loop.label = Some label.Ast.value) loops
  in
  match target with
  | Some target -> target
  | None -> Diagnostic.ice ~span "loop target does not exist"

let continue_target state label span =
  (loop_target state label span).continue_block

let place place_span base = { base; projections = []; place_span }

let add_projection place projection =
  { place with projections = projection :: place.projections }

let local_place span id = place span (Local id)
let copy span ty place : operand = { desc = Copy place; ty; span }

let constant (expr : Tast.texpr) desc : operand =
  { desc = Const desc; ty = expr.ty; span = expr.span }

let const_operand span ty desc : operand = { desc = Const desc; ty; span }

let assign state destination assigned =
  if is_live state then
    emit state
      (Assign (destination, { desc = Use assigned; ty = assigned.ty }))
      assigned.span

let temp_value_into state destination ty span desc =
  emit state (Assign (destination, { desc; ty })) span;
  copy span ty destination

let temp_value (state : builder) ty span desc =
  let destination = local_place span (add_local state Temp ty span) in
  temp_value_into state destination ty span desc

let materialize (state : builder) (operand : operand) =
  match operand.desc with
  | Copy place -> place
  | Const _ ->
      let id = add_local state Temp operand.ty operand.span in
      let destination = local_place operand.span id in
      assign state destination operand;
      destination

let save_operand (state : builder) (operand : operand) =
  let id = add_local state Temp operand.ty operand.span in
  let destination = local_place operand.span id in
  assign state destination operand;
  copy operand.span operand.ty destination

let global_place (state : builder) span (symbol : Symbol.t) =
  match Hashtbl.find_opt state.globals (Symbol.key symbol) with
  | Some name -> place span (Global name)
  | None ->
      Diagnostic.ice ~span
        (Printf.sprintf "no MIR place for symbol %s" symbol.Symbol.name)

let symbol_place state span symbol =
  match Hashtbl.find_opt state.symbols symbol.Symbol.id with
  | Some id -> local_place span id
  | None -> global_place state span symbol

(* The surviving source operators keep their meaning *)
let lower_unop op =
  match op with
  | Ast.Neg -> Neg
  | Ast.Not -> Not
  | Ast.BitNot -> BitNot
  | Ast.Pos -> Diagnostic.ice "unary plus reached MIR"
  | Ast.Deref -> Diagnostic.ice "deref is a projection and not a MIR value"
  | Ast.AddressOf -> Diagnostic.ice "address of is its own MIR value"

let lower_binop op =
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

let emit_check state check span =
  if is_live state then begin
    let fail = new_block state in
    let ok = new_block state in
    terminate state (Assert (check, ok, fail)) span;
    switch state fail;
    terminate state (Panic check) span;
    switch state ok
  end

let check_bounds state index length span =
  emit_check state (Bounds (index, length)) span

let check_slice_bounds state lo hi length span =
  emit_check state (SliceBounds (lo, hi, length)) span

(* A zero width pointee touches no memory *)
let check_null state pointee pointer span =
  let structs = state.struct_layouts in
  if Layout.ty_size structs pointee > 0 then
    emit_check state (Null pointer) span

let div_zero state divisor span = emit_check state (DivZero divisor) span

let negative_shift state count span =
  emit_check state (NegativeShift count) span

(* Plain and compound arithmetic guard the same two operators the same way *)
let check_arithmetic state op ~operand_ty right span =
  match op with
  | (Ast.Div | Ast.Mod) when not (is_float operand_ty) ->
      div_zero state right span
  | (Ast.Lshift | Ast.Rshift) when not (is_unsigned right.ty) ->
      negative_shift state right span
  | _ -> ()

let rec block_value state expr body =
  match List.rev body with
  | [] -> constant expr Undef
  | last :: reversed ->
      List.iter (lower_statement state) (List.rev reversed);
      if is_live state then lower_expr state last else constant expr Undef

and block_value_into state destination (expr : Tast.texpr) body =
  match List.rev body with
  | [] -> assign state destination (constant expr Undef)
  | last :: reversed ->
      List.iter (lower_statement state) (List.rev reversed);
      if is_live state then lower_fresh_into state destination last

and lower_short_circuit state (expr : Tast.texpr) left right short_value =
  let right_block = new_block state in
  let short_block = new_block state in
  let join_block = new_block state in
  let result = add_local state Temp Types.TBool expr.span in
  lower_branch state left
    (if short_value then short_block else right_block)
    (if short_value then right_block else short_block);
  switch state short_block;
  assign state
    (local_place expr.span result)
    (const_operand expr.span Types.TBool (Bool short_value));
  terminate state (Jump join_block) expr.span;
  switch state right_block;
  let right = lower_expr state right in
  assign state (local_place expr.span result) right;
  if is_live state then terminate state (Jump join_block) expr.span;
  switch state join_block;
  copy expr.span Types.TBool (local_place expr.span result)

(* A branch expr only needs a slot when it hands a value back *)
and branch_result state (expr : Tast.texpr) =
  if expr.ty = Types.TUnit || expr.ty = Types.TNever then None
  else
    let id = add_local state Temp expr.ty expr.span in
    Some (local_place expr.span id)

and join_result state (expr : Tast.texpr) result join =
  switch state join;
  if expr.ty = Types.TNever then terminate state Unreachable expr.span;
  match result with
  | Some result -> copy expr.span expr.ty result
  | None -> constant expr Undef

and lower_if state expr branches else_body =
  let result = branch_result state expr in
  let join = lower_if_into state expr result branches else_body in
  join_result state expr result join

and lower_if_into state expr result branches else_body =
  let join = new_block state in
  let lower_else = function
    | Some body -> lower_arm state expr result join body
    | None -> terminate state (Jump join) expr.span
  in
  let rec lower_branches = function
    | [] -> lower_else else_body
    | (condition, body) :: rest ->
        let yes = new_block state in
        let no = new_block state in
        lower_branch state condition yes no;
        switch state yes;
        lower_arm state expr result join body;
        switch state no;
        lower_branches rest
  in
  lower_branches branches;
  join

(* A pattern walks the scrutinee and gathers what to compare and what to name *)
and pattern_plan (place : place) (ty : ty) (pat : Tast.tpattern) =
  match pat with
  | Tast.TPatWild -> ([], [])
  | Tast.TPatBind (symbol, bound_ty) -> ([], [ (symbol, bound_ty, place) ])
  | Tast.TPatConst value -> ([ (place, ty, value) ], [])

(* One test per arm because a jump table only pays off on a dense range *)
and lower_match state (expr : Tast.texpr) (scrutinee : Tast.texpr) arms =
  let result = branch_result state expr in
  let join = lower_match_into state expr result scrutinee arms in
  join_result state expr result join

and lower_match_into state (expr : Tast.texpr) result (scrutinee : Tast.texpr)
    arms =
  (* The scrutinee is a place so a field pattern can project into it *)
  let subject = materialize state (lower_expr state scrutinee) in
  let join = new_block state in
  let wanted ty value =
    match resolve_ty ty with
    | Types.TBool -> Bool (value <> 0L)
    | Types.TChar -> Char (Int64.to_int value)
    | _ -> Int value
  in
  let bind (symbol, bound_ty, place) =
    let id = add_local state ~name:symbol.Symbol.name User bound_ty expr.span in
    bind_symbol state symbol id;
    assign state (local_place expr.span id) (copy expr.span bound_ty place)
  in
  let rec lower_tests fail = function
    | [] -> ()
    | (place, ty, value) :: rest ->
        let next = new_block state in
        let against = const_operand expr.span ty (wanted ty value) in
        let found = copy expr.span ty place in
        let equal =
          temp_value state Types.TBool expr.span (Binary (Eq, found, against))
        in
        terminate state (Branch (equal, next, fail)) expr.span;
        switch state next;
        lower_tests fail rest
  in
  let rec lower_arms = function
    | [] -> terminate state Unreachable expr.span
    | { tpat; tbody } :: rest -> (
        let tests, binds = pattern_plan subject scrutinee.ty tpat in
        let no = if List.is_empty tests then None else Some (new_block state) in
        Option.iter (fun no -> lower_tests no tests) no;
        List.iter bind binds;
        lower_arm state expr result join tbody;
        match no with
        | Some no ->
            switch state no;
            lower_arms rest
        | None -> ())
  in
  lower_arms arms;
  join

and lower_arm state (expr : Tast.texpr) result join body =
  match result with
  | None ->
      List.iter (lower_statement state) body;
      if is_live state then terminate state (Jump join) expr.span
  | Some result ->
      block_value_into state result expr body;
      if is_live state then begin
        terminate state (Jump join) expr.span
      end

and lower_branch state (expr : Tast.texpr) yes no =
  match expr.desc with
  | Tast.TBool value ->
      terminate state (Jump (if value then yes else no)) expr.span
  | Tast.TUnOp (Ast.Not, inner) -> lower_branch state inner no yes
  | Tast.TBinOp (Ast.And, left, right) ->
      let middle = new_block state in
      lower_branch state left middle no;
      switch state middle;
      lower_branch state right yes no
  | Tast.TBinOp (Ast.Or, left, right) ->
      let middle = new_block state in
      lower_branch state left yes middle;
      switch state middle;
      lower_branch state right yes no
  | _ ->
      let condition = lower_expr state expr in
      if is_live state then
        terminate state (Branch (condition, yes, no)) expr.span

and lower_while state span label condition body =
  let condition_block = new_block state in
  let body_block = new_block state in
  let exit_block = new_block state in
  terminate state (Jump condition_block) span;
  switch state condition_block;
  lower_branch state condition body_block exit_block;
  switch state body_block;
  with_loop state label ~continue_block:condition_block ~break_block:exit_block
    ~result:None (fun () -> List.iter (lower_statement state) body);
  if is_live state then terminate state (Jump condition_block) span;
  switch state exit_block

and lower_counted_loop state span label ~condition ?(enter_body = fun () -> ())
    ~step body =
  let condition_block = new_block state in
  let body_block = new_block state in
  let step_block = new_block state in
  let exit_block = new_block state in
  terminate state (Jump condition_block) span;
  switch state condition_block;
  terminate state (Branch (condition (), body_block, exit_block)) span;
  switch state body_block;
  enter_body ();
  with_loop state label ~continue_block:step_block ~break_block:exit_block
    ~result:None (fun () -> List.iter (lower_statement state) body);
  if is_live state then terminate state (Jump step_block) span;
  switch state step_block;
  step exit_block;
  terminate state (Jump condition_block) span;
  switch state exit_block

and lower_for state span label symbol elem_ty (iter : Tast.texpr) body =
  match iter.desc with
  | Tast.TRange (lo, hi) ->
      lower_range_for state span label symbol elem_ty lo hi false body
  | Tast.TRangeInclusive (lo, hi) ->
      lower_range_for state span label symbol elem_ty lo hi true body
  | _ -> lower_each_for state span label symbol elem_ty iter body

and lower_range_for state span label symbol elem_ty lo hi inclusive body =
  let loop_id =
    add_local state ~name:symbol.Symbol.name User elem_ty symbol.Symbol.span
  in
  bind_symbol state symbol loop_id;
  let lo = lower_expr state lo in
  let hi = lower_expr state hi in
  assign state (local_place span loop_id) lo;
  let high_id = add_local state ~name:"for.hi" Temp elem_ty hi.span in
  assign state (local_place span high_id) hi;
  let counter () = copy span elem_ty (local_place span loop_id) in
  let limit () = copy span elem_ty (local_place span high_id) in
  lower_counted_loop state span label
    ~condition:(fun () ->
      let op = if inclusive then Lte else Lt in
      temp_value state Types.TBool span (Binary (op, counter (), limit ())))
    ~step:(fun exit_block ->
      let current = counter () in
      if inclusive then begin
        let increment_block = new_block state in
        let done_ =
          temp_value state Types.TBool span (Binary (Eq, current, limit ()))
        in
        terminate state (Branch (done_, exit_block, increment_block)) span;
        switch state increment_block
      end;
      let one = const_operand span elem_ty (Int 1L) in
      let next = temp_value state elem_ty span (Binary (Add, current, one)) in
      assign state (local_place span loop_id) next)
    body

and lower_each_for state span label symbol elem_ty iter body =
  let source = lower_expr state iter |> materialize state in
  let source =
    match resolve_ty iter.ty with
    | Types.TSlice _ ->
        let id = add_local state Temp iter.ty iter.span in
        let snapshot = local_place iter.span id in
        assign state snapshot (copy iter.span iter.ty source);
        snapshot
    | _ -> source
  in
  let pointer_ty = Types.TPointer elem_ty in
  let pointer_id = add_local state Temp pointer_ty span in
  let length_id = add_local state Temp (Types.TInt Usize) span in
  let index_id = add_local state Temp (Types.TInt Usize) span in
  let loop_id =
    add_local state ~name:symbol.Symbol.name User elem_ty symbol.Symbol.span
  in
  bind_symbol state symbol loop_id;
  let pointer = temp_value state pointer_ty span (DataPtr source) in
  let length = temp_value state (Types.TInt Usize) span (Len source) in
  assign state (local_place span pointer_id) pointer;
  assign state (local_place span length_id) length;
  assign state
    (local_place span index_id)
    (const_operand span (Types.TInt Usize) (Int 0L));
  let index () = copy span (Types.TInt Usize) (local_place span index_id) in
  lower_counted_loop state span label
    ~condition:(fun () ->
      let length = copy span (Types.TInt Usize) (local_place span length_id) in
      temp_value state Types.TBool span (Binary (Lt, index (), length)))
    ~enter_body:(fun () ->
      let element =
        {
          base = Local pointer_id;
          projections = [ Index (index ()) ];
          place_span = span;
        }
      in
      assign state (local_place span loop_id) (copy span elem_ty element))
    ~step:(fun _ ->
      let one = const_operand span (Types.TInt Usize) (Int 1L) in
      let next =
        temp_value state (Types.TInt Usize) span (Binary (Add, index (), one))
      in
      assign state (local_place span index_id) next)
    body

and lower_loop state span label ty body =
  let result =
    match ty with
    | Types.TUnit | Types.TNever -> None
    | _ ->
        let id = add_local state Temp ty span in
        Some (local_place span id)
  in
  lower_loop_into state span label ty result body;
  match result with
  | Some result -> copy span ty result
  | None -> const_operand span ty Undef

and lower_loop_into state span label ty result body =
  let body_block = new_block state in
  let exit_block = new_block state in
  terminate state (Jump body_block) span;
  switch state body_block;
  with_loop state label ~continue_block:body_block ~break_block:exit_block
    ~result:(Option.map (fun place -> (place, ty)) result)
    (fun () -> List.iter (lower_statement state) body);
  if is_live state then terminate state (Jump body_block) span;
  switch state exit_block

(* The minimum integer and negative one need no hardware divide *)
and lower_guarded_div_into state destination span ty op (left : operand)
    (right : operand) =
  let negative_one = const_operand span right.ty (Int (-1L)) in
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
  let wrapped =
    if op = Mod then const_operand span ty (Int 0L)
    else temp_value state ty span (Unary (Neg, left))
  in
  assign state destination wrapped;
  terminate state (Jump join_block) span;
  switch state divide_block;
  let divided = temp_value state ty span (Binary (op, left, right)) in
  assign state destination divided;
  terminate state (Jump join_block) span;
  switch state join_block;
  copy span ty destination

and lower_guarded_div state span ty op left right =
  let destination = local_place span (add_local state Temp ty span) in
  lower_guarded_div_into state destination span ty op left right

(* A count past the width drains every bit *)
and lower_guarded_shift_into state destination span ty op (left : operand)
    (right : operand) =
  let bits = 8 * int_kind_size (int_kind_of ty) in
  let width = const_operand span right.ty (Int (Int64.of_int bits)) in
  let count_in_range =
    temp_value state Types.TBool span (Binary (Lt, right, width))
  in
  let shift_block = new_block state in
  let drained_block = new_block state in
  let join_block = new_block state in
  terminate state (Branch (count_in_range, shift_block, drained_block)) span;
  switch state shift_block;
  let shifted = temp_value state ty span (Binary (op, left, right)) in
  assign state destination shifted;
  terminate state (Jump join_block) span;
  switch state drained_block;
  (* A signed right shift settles at the top bit *)
  let drained =
    if op = Rshift && not (is_unsigned ty) then
      let top = const_operand span right.ty (Int (Int64.of_int (bits - 1))) in
      temp_value state ty span (Binary (Rshift, left, top))
    else const_operand span ty (Int 0L)
  in
  assign state destination drained;
  terminate state (Jump join_block) span;
  switch state join_block;
  copy span ty destination

and lower_guarded_shift state span ty op left right =
  let destination = local_place span (add_local state Temp ty span) in
  lower_guarded_shift_into state destination span ty op left right

and lower_binary state span ty (op : Ast.binop) (left : operand)
    (right : operand) =
  let lowered = lower_binop op in
  match op with
  | (Ast.Div | Ast.Mod) when div_int_needs_check left.ty ->
      lower_guarded_div state span ty lowered left right
  | Ast.Lshift | Ast.Rshift ->
      lower_guarded_shift state span ty lowered left right
  | _ -> temp_value state ty span (Binary (lowered, left, right))

and lower_binary_into state destination span ty (op : Ast.binop)
    (left : operand) (right : operand) =
  let lowered = lower_binop op in
  match op with
  | (Ast.Div | Ast.Mod) when div_int_needs_check left.ty ->
      lower_guarded_div_into state destination span ty lowered left right
  | Ast.Lshift | Ast.Rshift ->
      lower_guarded_shift_into state destination span ty lowered left right
  | _ ->
      temp_value_into state destination ty span (Binary (lowered, left, right))

and map_operands state = function
  | [] -> []
  | expr :: rest ->
      let value : operand = lower_expr state expr in
      if value.ty = Types.TUnit then map_operands state rest
      else
        let value =
          if is_aggregate value.ty then
            copy value.span value.ty (materialize state value)
          else value
        in
        value :: map_operands state rest

and lower_expr state expr =
  match expr.desc with
  | Tast.TErrorExpr ->
      Diagnostic.ice ~span:expr.span "error expression reached MIR"
  | Tast.TInt value -> constant expr (Int value)
  (* The backend sees the variant value as an integer *)
  | Tast.TVariant (_, value) -> constant expr (Int value)
  | Tast.TFloat value -> constant expr (Float value)
  | Tast.TBool value -> constant expr (Bool value)
  | Tast.TNull -> constant expr Null
  | Tast.TCStr value -> constant expr (CStr value)
  | Tast.TStr value -> constant expr (Str value)
  | Tast.TChar value -> constant expr (Char value)
  | Tast.TZero -> constant expr Zero
  | Tast.TUndef -> constant expr Undef
  | Tast.TIdent _ when expr.ty = Types.TUnit -> constant expr Undef
  | Tast.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
      constant expr (Function symbol.Symbol.link_name)
  | Tast.TIdent symbol ->
      copy expr.span expr.ty (symbol_place state expr.span symbol)
  | Tast.TCall (callee, args, variadic_start) ->
      lower_call state expr callee args variadic_start
  | Tast.TBinOp (Ast.And, left, right) ->
      lower_short_circuit state expr left right false
  | Tast.TBinOp (Ast.Or, left, right) ->
      lower_short_circuit state expr left right true
  | Tast.TAssign (None, ({ desc = Tast.TIdent _; _ } as left), right)
    when left.ty <> Types.TUnit ->
      let destination = lower_place state left in
      lower_expr_into state destination right;
      copy expr.span left.ty destination
  | Tast.TAssign (None, left, right) ->
      let assigned = lower_expr state right in
      if left.ty <> Types.TUnit then begin
        let destination = lower_place state left in
        assign state destination assigned
      end;
      assigned
  | Tast.TAssign (Some op, left, right) ->
      lower_compound_assign state expr op left right
  | Tast.TBinOp (op, left, right) ->
      let left, right = lower_binop_operands state expr op left right in
      lower_binary state expr.span expr.ty op left right
  | Tast.TUnOp (Ast.Pos, inner) -> lower_expr state inner
  | Tast.TUnOp (Ast.AddressOf, { desc = Tast.TUnOp (Ast.Deref, inner); _ }) ->
      lower_expr state inner
  | Tast.TUnOp (Ast.AddressOf, ({ desc = Tast.TIdent symbol; _ } as inner))
    when Symbol.is_func symbol.Symbol.kind ->
      let function_value = lower_expr state inner in
      { function_value with ty = expr.ty; span = expr.span }
  | Tast.TUnOp (Ast.AddressOf, inner) ->
      let inner = lower_place state inner in
      temp_value state expr.ty expr.span (AddressOf inner)
  | Tast.TUnOp (Ast.Deref, _) ->
      let source = lower_place state expr in
      copy expr.span expr.ty source
  | Tast.TUnOp (op, inner) ->
      let inner = lower_expr state inner in
      temp_value state expr.ty expr.span (Unary (lower_unop op, inner))
  | Tast.TFieldAccess _ | Tast.TIndex _ ->
      let source = lower_place state expr in
      copy expr.span expr.ty source
  | Tast.TCast inner ->
      let inner = lower_expr state inner in
      temp_value state expr.ty expr.span (Cast inner)
  | Tast.TSizeOf ty -> temp_value state expr.ty expr.span (SizeOf ty)
  | Tast.TRange _ | Tast.TRangeInclusive _ ->
      Diagnostic.ice ~span:expr.span "range outside a for loop"
  | Tast.TArrayLit elements -> lower_array_literal state expr elements
  | Tast.TLen inner ->
      let inner = lower_expr state inner |> materialize state in
      temp_value state expr.ty expr.span (Len inner)
  | Tast.TSliceExpr (base, lo, hi) -> lower_slice state expr base lo hi
  | Tast.TDataPtr inner ->
      let inner = lower_expr state inner |> materialize state in
      temp_value state expr.ty expr.span (DataPtr inner)
  | Tast.TStructLit (_, fields) -> lower_struct_literal state expr fields
  | Tast.TLocalDecl -> constant expr Undef
  | Tast.TLoop (label, body) -> lower_loop state expr.span label expr.ty body
  | Tast.TBlock body -> block_value state expr body
  | Tast.TIf (branches, else_body) -> lower_if state expr branches else_body
  | Tast.TMatch (scrutinee, arms) -> lower_match state expr scrutinee arms
  | Tast.TWhile (label, condition, body) ->
      lower_while state expr.span label condition body;
      constant expr Undef
  | Tast.TFor (label, symbol, elem_ty, iter, body) ->
      lower_for state expr.span label symbol elem_ty iter body;
      constant expr Undef
  | Tast.TBinding _ | Tast.TReturn _ | Tast.TBreak _ | Tast.TContinue _
  | Tast.TPairAssign _ ->
      lower_statement state expr;
      constant expr Undef
  | Tast.TUnit -> constant expr Undef

and lower_call state expr callee args variadic_start =
  let destination =
    if expr.ty = Types.TUnit || expr.ty = Types.TNever then None
    else
      let id = add_local state Temp expr.ty expr.span in
      Some (local_place expr.span id)
  in
  emit_call state destination expr callee args variadic_start;
  match destination with
  | Some destination -> copy expr.span expr.ty destination
  | None -> constant expr Undef

and emit_call state destination expr callee args variadic_start =
  let args = map_operands state args in
  let callee_value, kind =
    match callee.desc with
    | Tast.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
        let kind =
          match resolve_ty callee.ty with
          | Types.TFunc (_, _, C) -> External
          | _ -> Internal
        in
        (Direct symbol.Symbol.link_name, kind)
    | _ ->
        let kind =
          match resolve_ty callee.ty with
          | Types.TFunc (_, _, C) -> External
          | _ -> Internal
        in
        (Indirect (lower_expr state callee), kind)
  in
  emit state
    (Call
       {
         destination;
         callee = callee_value;
         kind;
         args;
         return_ty = expr.ty;
         variadic_start;
       })
    expr.span;
  if expr.ty = Types.TNever then terminate state Unreachable expr.span

and lower_binop_operands state expr op left right =
  let left = lower_expr state left in
  let right = lower_expr state right in
  check_arithmetic state op ~operand_ty:left.ty right expr.span;
  (left, right)

and lower_expr_into state destination expr =
  match expr.desc with
  | Tast.TCall (callee, args, variadic_start) ->
      let result =
        if expr.ty = Types.TUnit || expr.ty = Types.TNever then None
        else Some destination
      in
      emit_call state result expr callee args variadic_start
  | Tast.TBinOp (op, left, right) when op <> Ast.And && op <> Ast.Or ->
      let left, right = lower_binop_operands state expr op left right in
      ignore
        (lower_binary_into state destination expr.span expr.ty op left right)
  | _ ->
      let assigned = lower_expr state expr in
      if is_live state then assign state destination assigned

and lower_fresh_into state destination expr =
  match expr.desc with
  | Tast.TArrayLit elements ->
      fill_array_literal state destination expr elements
  | Tast.TStructLit (_, fields) ->
      fill_struct_literal state destination expr fields
  | Tast.TSliceExpr (base, lo, hi) ->
      fill_slice state destination expr base lo hi
  | Tast.TBlock body -> block_value_into state destination expr body
  | Tast.TIf (branches, else_body) ->
      let join =
        lower_if_into state expr (Some destination) branches else_body
      in
      switch state join;
      if expr.ty = Types.TNever then terminate state Unreachable expr.span
  | Tast.TMatch (scrutinee, arms) ->
      let join =
        lower_match_into state expr (Some destination) scrutinee arms
      in
      switch state join;
      if expr.ty = Types.TNever then terminate state Unreachable expr.span
  | Tast.TLoop (label, body) ->
      lower_loop_into state expr.span label expr.ty (Some destination) body
  | _ -> lower_expr_into state destination expr

and lower_place state expr =
  match expr.desc with
  | Tast.TIdent symbol -> symbol_place state expr.span symbol
  | Tast.TUnOp (Ast.Deref, inner) ->
      let pointer = lower_expr state inner in
      check_null state expr.ty pointer expr.span;
      let source = materialize state pointer in
      add_projection source Deref
  | Tast.TFieldAccess (base, field) ->
      let source =
        match resolve_ty base.ty with
        | Types.TPointer pointee ->
            let pointer = lower_expr state base in
            check_null state pointee pointer base.span;
            let source = materialize state pointer in
            add_projection source Deref
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
          check_bounds state index length expr.span
      | Types.TPointer _ -> ()
      | _ ->
          let message = "index on non indexed MIR place" in
          Diagnostic.ice ~span:base.span message);
      add_projection source (Index index)
  | _ -> lower_expr state expr |> materialize state

and fill_array_literal state destination (expr : Tast.texpr) elements =
  let element_ty =
    match resolve_ty expr.ty with
    | Types.TArray (element, _) -> element
    | _ -> Diagnostic.ice ~span:expr.span "array literal has non array type"
  in
  emit state
    (Assign (destination, { desc = Use (constant expr Undef); ty = expr.ty }))
    expr.span;
  List.iteri
    (fun index (element : Tast.texpr) ->
      let index_operand =
        const_operand element.span (Types.TInt Usize) (Int (Int64.of_int index))
      in
      let target = add_projection destination (Index index_operand) in
      if is_aggregate element_ty then lower_fresh_into state target element
      else
        let assigned = lower_expr state element in
        assign state target { assigned with ty = element_ty })
    elements

and lower_array_literal state expr elements =
  let id = add_local state Temp expr.ty expr.span in
  let destination = local_place expr.span id in
  fill_array_literal state destination expr elements;
  copy expr.span expr.ty destination

and fill_struct_literal state destination expr fields =
  emit state
    (Assign (destination, { desc = Use (constant expr Zero); ty = expr.ty }))
    expr.span;
  List.iter
    (fun (field, (value : Tast.texpr)) ->
      let target = add_projection destination (Field field) in
      if is_aggregate value.ty then lower_fresh_into state target value
      else assign state target (lower_expr state value))
    fields

and lower_struct_literal state expr fields =
  let id = add_local state Temp expr.ty expr.span in
  let destination = local_place expr.span id in
  fill_struct_literal state destination expr fields;
  copy expr.span expr.ty destination

and fill_slice state destination expr base lo hi =
  let base = lower_expr state base |> materialize state in
  let lo = lower_expr state lo in
  let hi = lower_expr state hi in
  let length = temp_value state (Types.TInt Usize) expr.span (Len base) in
  check_slice_bounds state lo hi length expr.span;
  emit state (Slice (destination, base, lo, hi)) expr.span

and lower_slice state expr base lo hi =
  let id = add_local state Temp expr.ty expr.span in
  let destination = local_place expr.span id in
  fill_slice state destination expr base lo hi;
  copy expr.span expr.ty destination

and lower_compound_assign state (expr : Tast.texpr) op (left : Tast.texpr) right
    =
  let target = lower_place state left in
  let old = copy left.span left.ty target in
  let right = lower_expr state right in
  check_arithmetic state op ~operand_ty:old.ty right expr.span;
  let updated = lower_binary state expr.span left.ty op old right in
  assign state target updated;
  updated

and lower_pair_assign state first_target second_target first_value second_value
    =
  let first_value = lower_expr state first_value |> save_operand state in
  let second_value = lower_expr state second_value |> save_operand state in
  let first_target = lower_place state first_target in
  assign state first_target first_value;
  let second_target = lower_place state second_target in
  assign state second_target second_value

(* A later break can widen the loop type the earlier ones settled on *)
and widen_break state result_ty (value : Tast.texpr) (lowered : operand) =
  if lowered.ty = Types.TNever || ty_equal lowered.ty result_ty then lowered
  else temp_value state result_ty value.span (Cast lowered)

and lower_statement state expr =
  if is_live state then
    match expr.desc with
    | Tast.TBinding (_, _, ty, init)
      when ty = Types.TNever || init.ty = Types.TNever ->
        ignore (lower_expr state init)
    | Tast.TBinding (_, symbol, Types.TUnit, init) ->
        let id =
          add_local state ~name:symbol.Symbol.name User Types.TUnit
            symbol.Symbol.span
        in
        bind_symbol state symbol id;
        ignore (lower_expr state init)
    | Tast.TBinding (Ast.Const, _, _, _) -> ()
    | Tast.TBinding (_, symbol, ty, init) ->
        let id =
          add_local state ~name:symbol.Symbol.name User ty symbol.Symbol.span
        in
        bind_symbol state symbol id;
        lower_fresh_into state (local_place expr.span id) init
    | Tast.TReturn (Some value) when state.result <> None ->
        let result = Option.get state.result in
        lower_fresh_into state (local_place expr.span result) value;
        if is_live state then terminate state (ReturnValue None) expr.span
    | Tast.TReturn (Some value) when value.ty = Types.TUnit ->
        ignore (lower_expr state value);
        if is_live state then terminate state (ReturnValue None) expr.span
    | Tast.TReturn returned ->
        let returned =
          match returned with
          | Some value -> Some (lower_expr state value)
          | None when state.bare_return_zero ->
              Some (const_operand expr.span (Types.TInt I32) (Int 0L))
          | None -> None
        in
        if is_live state then terminate state (ReturnValue returned) expr.span
    | Tast.TBreak (label, value) ->
        let target = loop_target state label expr.span in
        let break_with value =
          match target.result with
          | Some (result, result_ty) when is_aggregate result_ty ->
              lower_fresh_into state result value
          | Some (result, result_ty) ->
              let lowered = lower_expr state value in
              assign state result (widen_break state result_ty value lowered)
          | None when value.ty = Types.TUnit -> ignore (lower_expr state value)
          | None -> Diagnostic.ice ~span:expr.span "loop has no result"
        in
        Option.iter break_with value;
        terminate state (Jump target.break_block) expr.span
    | Tast.TContinue label ->
        terminate state (Jump (continue_target state label expr.span)) expr.span
    | Tast.TWhile (label, condition, body) ->
        lower_while state expr.span label condition body
    | Tast.TFor (label, symbol, elem_ty, iter, body) ->
        lower_for state expr.span label symbol elem_ty iter body
    | Tast.TLoop (label, body) ->
        ignore (lower_loop state expr.span label expr.ty body)
    | Tast.TPairAssign (first_target, second_target, first_value, second_value)
      ->
        lower_pair_assign state first_target second_target first_value
          second_value
    | Tast.TBlock body -> List.iter (lower_statement state) body
    | _ ->
        ignore (lower_expr state expr);
        if expr.ty = Types.TNever && is_live state then
          terminate state Unreachable expr.span

let build_func struct_layouts globals (func : Tast.tfunc_def) =
  let state =
    make_builder ~struct_layouts ~globals
      ~bare_return_zero:(func.entry_point && func.ret_ty = Types.TInt Types.I32)
  in
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
      Some (add_local state ~name:"result" Result func.ret_ty span);
  let params =
    List.filter_map
      (fun (symbol, ty) ->
        if ty = Types.TUnit then None
        else
          let id =
            add_local state ~name:symbol.Symbol.name Param ty symbol.Symbol.span
          in
          bind_symbol state symbol id;
          Some id)
      func.params
  in
  List.iter (lower_statement state) func.body;
  if is_live state then
    if func.entry_point && func.ret_ty = Types.TInt Types.I32 then
      terminate state
        (ReturnValue (Some (const_operand span (Types.TInt Types.I32) (Int 0L))))
        span
    else if func.ret_ty = Types.TUnit then
      terminate state (ReturnValue None) span
    else terminate state Unreachable span;
  {
    name = func.name;
    source_name = func.source_name;
    public = List.mem Ast.Pub func.modifiers;
    params;
    result = state.result;
    locals = finish_locals state;
    blocks = finish_blocks state;
    return_ty = func.ret_ty;
    entry_point = func.entry_point;
    span;
  }

let build (declarations : Tast.tdecl list) =
  let globals_by_id = Hashtbl.create 16 in
  let structs_rev = ref [] in
  let globals_rev = ref [] in
  let functions_rev = ref [] in
  List.iter
    (function
      | Tast.TStruct (name, fields, _) ->
          structs_rev := { name; fields; local = false } :: !structs_rev
      | Tast.TLocalStruct (name, fields) ->
          structs_rev := { name; fields; local = true } :: !structs_rev
      | Tast.TGlobal global when global.ty = Types.TUnit -> ()
      | Tast.TGlobal global when global.kind <> Ast.Const ->
          Hashtbl.add globals_by_id global.key global.name;
          globals_rev := global :: !globals_rev
      | Tast.TFunc func -> functions_rev := func :: !functions_rev
      | Tast.TGlobal _ | Tast.TExtern _ | Tast.TTypeAlias _ | Tast.TEnum _ -> ())
    declarations;
  let structs = List.rev !structs_rev in
  let struct_layouts = build_struct_layouts structs in
  let globals =
    List.rev !globals_rev
    |> List.map (fun (global : Tast.tglobal_def) ->
        {
          name = global.name;
          ty = global.ty;
          init = Option.map global_init global.init;
          public = List.mem Ast.Pub global.modifiers;
        })
  in
  let functions =
    List.rev !functions_rev
    |> List.map (build_func struct_layouts globals_by_id)
  in
  { structs; globals; functions }

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
  | Some { desc = Assert (assertion, ok, fail); _ } ->
      Printf.sprintf "assert_%s block%d block%d" (show_check assertion) ok fail
  | Some { desc = Panic failed; _ } -> "panic " ^ show_check failed
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
  Printf.bprintf buffer "func %s(%s)%s {\n" func.name params return_type;
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

let show_error (error : error) =
  Printf.sprintf "%s: %s" error.function_name error.message

let add ctx span message =
  ctx.errors :=
    { function_name = ctx.func.name; error_span = span; message }
    :: !(ctx.errors)

(* local %0 value: i32 user *)
let local ctx span id =
  if id < 0 || id >= Array.length ctx.func.locals then begin
    add ctx span (Printf.sprintf "local %d does not exist" id);
    None
  end
  else Some ctx.func.locals.(id).ty

(* copy %0 / 42 *)
let rec operand ctx (operand : operand) =
  match operand.desc with
  | Const _ -> Some operand.ty
  | Copy place_value ->
      let place_ty = place ctx place_value in
      (match place_ty with
      | Some ty when not (ty_equal ty operand.ty) ->
          add ctx operand.span
            (Printf.sprintf "operand has type %s but place has type %s"
               (show_ty operand.ty) (show_ty ty))
      | Some _ | None -> ());
      place_ty

and struct_field ctx span field name =
  match Symbol.Table.find_opt ctx.program.structs (Qname.key name) with
  | Some fields when field >= 0 && field < Array.length fields ->
      Some fields.(field)
  | Some _ ->
      add ctx span (Printf.sprintf "field projection %d does not exist" field);
      None
  | None ->
      add ctx span (Printf.sprintf "struct %s has no layout" (Qname.show name));
      None

and deref_ty ctx span ty =
  match resolve_ty ty with
  | Types.TPointer inner -> Some inner
  | _ ->
      add ctx span "deref projection requires a pointer";
      None

and field_ty ctx span ty field =
  match resolve_ty ty with
  | Types.TStruct (name, _) -> struct_field ctx span field name
  | _ ->
      add ctx span "field projection requires a struct";
      None

and projection_ty ctx span ty = function
  | Deref -> deref_ty ctx span ty
  | Field field -> field_ty ctx span ty field
  | Index index -> (
      ignore (operand ctx index);
      if
        not (match resolve_ty index.ty with Types.TInt _ -> true | _ -> false)
      then add ctx index.span "index projection requires an integer";
      match resolve_ty ty with
      | Types.TArray (inner, _) | Types.TSlice inner | Types.TPointer inner ->
          Some inner
      | _ ->
          add ctx span "index projection requires indexed storage";
          None)

and project ctx span ty = function
  | [] -> Some ty
  | projection :: rest ->
      Option.bind (project ctx span ty rest) (fun ty ->
          projection_ty ctx span ty projection)

and global ctx span name =
  match Hashtbl.find_opt ctx.program.globals name with
  | Some ty -> Some ty
  | None ->
      add ctx span (Printf.sprintf "global %s does not exist" name);
      None

(* %0 / @global / %0.deref.field0[copy %1] *)
and place ctx (place : place) =
  let span = place.place_span in
  let base_ty =
    match place.base with
    | Local id -> local ctx span id
    | Global name -> global ctx span name
  in
  match base_ty with
  | Some ty -> project ctx span ty place.projections
  | None -> None

(* copy %0 / copy %0 + 1 / address_of %0 / len %0 *)
let value ctx (value : value) =
  let check_operand operand_value = ignore (operand ctx operand_value) in
  (match value.desc with
  | Use operand_value -> check_operand operand_value
  | Unary (_, operand_value) -> check_operand operand_value
  | Binary (_, left, right) ->
      check_operand left;
      check_operand right
  | Cast operand_value -> check_operand operand_value
  | AddressOf place_value | Len place_value | DataPtr place_value ->
      ignore (place ctx place_value)
  | SizeOf _ -> ());
  value.ty

(* bounds copy %0 copy %1 / null copy %0 *)
let check ctx = function
  | Bounds (index, length) ->
      ignore (operand ctx index);
      ignore (operand ctx length)
  | SliceBounds (lo, hi, length) ->
      ignore (operand ctx lo);
      ignore (operand ctx hi);
      ignore (operand ctx length)
  | Null pointer -> ignore (operand ctx pointer)
  | DivZero divisor -> ignore (operand ctx divisor)
  | NegativeShift count -> ignore (operand ctx count)

let verify_call ctx span (call : call) =
  (match call.callee with
  | Direct _ -> ()
  | Indirect callee -> ignore (operand ctx callee));
  List.iter (fun arg -> ignore (operand ctx arg)) call.args;
  let destination_ty = Option.bind call.destination (place ctx) in
  if is_aggregate call.return_ty then
    match destination_ty with
    | Some ty when ty_equal ty call.return_ty -> ()
    | Some ty ->
        add ctx span
          (Printf.sprintf
             "aggregate result storage has type %s but call returns %s"
             (show_ty ty) (show_ty call.return_ty))
    | None -> add ctx span "aggregate call has no result storage"
  else
    match (call.return_ty, destination_ty) with
    | (Types.TUnit | Types.TNever), None -> ()
    | (Types.TUnit | Types.TNever), Some _ ->
        add ctx span "unit call has result storage"
    | return_ty, Some ty when ty_equal ty return_ty -> ()
    | return_ty, Some ty ->
        add ctx span
          (Printf.sprintf "call result has type %s but call returns %s"
             (show_ty ty) (show_ty return_ty))
    | _, None -> add ctx span "call has no result storage"

(* %0 = copy %1 / %0 = call @add(copy %1) *)
let statement ctx (statement : statement) =
  let span = statement.span in
  match statement.desc with
  | Assign (destination, assigned) -> (
      let destination_ty = place ctx destination in
      let assigned_ty = value ctx assigned in
      match destination_ty with
      | Some ty when not (Typred.compatible ty assigned_ty) ->
          add ctx span
            (Printf.sprintf "assignment stores %s in %s" (show_ty assigned_ty)
               (show_ty ty))
      | Some _ | None -> ())
  | Slice (destination, source, lo, hi) ->
      ignore (place ctx destination);
      ignore (place ctx source);
      ignore (operand ctx lo);
      ignore (operand ctx hi)
  | Call call -> verify_call ctx span call

(* block0 *)
let block_exists ctx span id =
  if id < 0 || id >= Array.length ctx.func.blocks then
    add ctx span (Printf.sprintf "block %d does not exist" id)

let verify_return ctx span returned =
  match (ctx.func.return_ty, returned) with
  | Types.TUnit, None | Types.TNever, None -> ()
  | Types.TUnit, Some value ->
      ignore (operand ctx value);
      add ctx span "unit function returns a runtime value"
  | return_ty, Some value ->
      ignore (operand ctx value);
      if not (Typred.compatible return_ty value.ty) then
        add ctx span
          (Printf.sprintf "return has type %s but function returns %s"
             (show_ty value.ty) (show_ty return_ty))
  | return_ty, None ->
      add ctx span
        (Printf.sprintf "return has no value for %s" (show_ty return_ty))

(* jump block1 / branch %0 block1 block2 / return %0 / unreachable *)
let terminator ctx (terminator : terminator) =
  let span = terminator.span in
  match terminator.desc with
  | Jump target -> block_exists ctx span target
  | Branch (condition, yes, no) ->
      ignore (operand ctx condition);
      block_exists ctx span yes;
      block_exists ctx span no
  | Assert (assertion, ok, fail) ->
      check ctx assertion;
      block_exists ctx span ok;
      block_exists ctx span fail
  | Panic failed -> check ctx failed
  | ReturnValue (Some value) when ctx.func.result <> None ->
      ignore (operand ctx value);
      add ctx span "return has a value but the result is storage"
  | ReturnValue None when ctx.func.result <> None -> ()
  | ReturnValue returned -> verify_return ctx span returned
  | Unreachable -> ()

let verify_block ctx id (block : block) =
  List.iter (statement ctx) block.statements;
  match block.terminator with
  | Some term -> terminator ctx term
  | None ->
      add ctx ctx.func.span (Printf.sprintf "block %d has no terminator" id)

(* func add(%0: i32, %1: i32) i32 { ... } *)
let verify_func program func =
  let ctx = { program; func; errors = ref [] } in
  Array.iter
    (fun (local : local) ->
      if local.ty = Types.TError then add ctx local.span "local has no type")
    func.locals;
  List.iter (fun id -> ignore (local ctx func.span id)) func.params;
  Array.iteri (verify_block ctx) func.blocks;
  List.rev !(ctx.errors)

let make_context (program : program) =
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
  { structs; globals }

let verify program =
  let context = make_context program in
  let errors = List.concat_map (verify_func context) program.functions in
  match errors with [] -> Ok () | _ -> Error errors
