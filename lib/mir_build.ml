(* SPDX-License-Identifier: GPL-2.0-only *)

module Mir_const = struct
  open Types
  open Mir
  module S = Typed_ast

  type global_state = {
    init : S.texpr option;
    mutable value : Const_eval.const_num option;
    mutable busy : bool;
  }

  type context = {
    structs : ty list Symbol.Table.t;
    globals : global_state Symbol.Table.t;
    comptime_globals : unit Symbol.Table.t;
  }

  let make_context declarations =
    let structs = Symbol.Table.create 16 in
    let globals = Symbol.Table.create 16 in
    let comptime_globals = Symbol.Table.create 16 in
    List.iter
      (function
        | S.TStruct (name, fields, _) | S.TLocalStruct (name, fields) ->
            Symbol.Table.add structs (Qname.key name) fields
        | S.TGlobal global ->
            Symbol.Table.add globals global.S.key
              { init = global.S.init; value = None; busy = false };
            if global.S.kind = Ast.Comptime then
              Symbol.Table.add comptime_globals global.S.key ()
        | S.TFunc _ | S.TExtern _ | S.TTypeAlias _ | S.TNewtype _ | S.TEnum _ ->
            ())
      declarations;
    { structs; globals; comptime_globals }

  let is_comptime_global context key =
    Symbol.Table.mem context.comptime_globals key

  (* Global initializers can name each other so evaluation is lazy and cached *)
  let rec global_num (context : context) span key =
    let state : global_state =
      match Symbol.Table.find_opt context.globals key with
      | Some state -> state
      | None -> raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
    in
    match state.value with
    | Some value -> value
    | None ->
        if state.busy then
          raise
            (Diagnostic.Errors [ Diagnostic.error_at span "cyclic constant" ]);
        let init =
          match state.init with
          | Some init -> init
          | None ->
              raise (Diagnostic.Errors [ Const_eval.unsupported_const span ])
        in
        state.busy <- true;
        let value =
          match
            Const_eval.fold_const_num ~sizeof:(ty_size context.structs)
              ~resolve:(fun symbol _ span ->
                global_num context span (Symbol.key symbol))
              init
          with
          | value ->
              state.busy <- false;
              value
          | exception ex ->
              state.busy <- false;
              raise ex
        in
        state.value <- Some value;
        value

  let eval (context : context) expr =
    Const_eval.fold_const_num ~sizeof:(ty_size context.structs)
      ~resolve:(fun symbol _ span ->
        global_num context span (Symbol.key symbol))
      expr

  (* A comptime binding in scope shadows a global of the same name *)
  let eval_in_scope (context : context) locals expr =
    Const_eval.fold_const_num ~sizeof:(ty_size context.structs)
      ~resolve:(fun symbol _ span ->
        let key = Symbol.key symbol in
        match Hashtbl.find_opt locals key with
        | Some value -> value
        | None -> global_num context span key)
      expr

  let constant_of_num ty = function
    | Const_eval.Nf value -> Float value
    | Const_eval.Ni32 value when resolve_ty ty = TBool -> Bool (value <> 0l)
    | Const_eval.Ni32 value when resolve_ty ty = TChar ->
        Char (Int32.to_int value)
    | Const_eval.Ni32 value -> Int (Int64.of_int32 value)
    | Const_eval.Ni64 value -> Int value

  let rec global_init context (expr : S.texpr) =
    match expr.S.desc with
    | S.TInt value -> GlobalConst (Int value, expr.S.ty)
    | S.TVariant (_, value) -> GlobalConst (Int value, expr.S.ty)
    | S.TFloat value -> GlobalConst (Float value, expr.S.ty)
    | S.TBool value -> GlobalConst (Bool value, expr.S.ty)
    | S.TNull -> GlobalConst (Null, expr.S.ty)
    | S.TCStr value -> GlobalConst (CStr value, expr.S.ty)
    | S.TStr value -> GlobalConst (Str value, expr.S.ty)
    | S.TChar value -> GlobalConst (Char value, expr.S.ty)
    | S.TZero -> GlobalConst (Zero, expr.S.ty)
    | S.TUndef -> GlobalConst (Undef, expr.S.ty)
    | S.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
        GlobalConst (Function symbol.Symbol.link_name, expr.S.ty)
    | S.TUnOp (Ast.AddressOf, { S.desc = S.TIdent symbol; _ }) ->
        GlobalAddress symbol.Symbol.link_name
    | S.TArrayLit values -> GlobalArray (List.map (global_init context) values)
    | S.TStructLit (_, fields) ->
        GlobalStruct
          (List.map
             (fun (field, value) -> (field, global_init context value))
             fields)
    | _ when Const_eval.foldable expr ->
        GlobalConst (constant_of_num expr.S.ty (eval context expr), expr.S.ty)
    | _ -> Diagnostic.ice ~span:expr.S.span "unsupported MIR global initializer"
end

module Mir_builder = struct
  open Mir
  module S = Typed_ast

  type open_block = {
    mutable statements : statement list;
    mutable terminator : terminator option;
  }

  type loop = {
    label : Ast.name option;
    continue_block : block_id;
    break_block : block_id;
    result : (place * Types.ty) option;
  }

  type state = {
    locals_rev : local list ref;
    next_local : int ref;
    symbols : (Symbol.id, local_id) Hashtbl.t;
    globals : (Symbol.key, string) Hashtbl.t;
    blocks : (block_id, open_block) Hashtbl.t;
    next_block : int ref;
    current : block_id ref;
    loops : loop list ref;
    consts : (Symbol.key, Const_eval.const_num) Hashtbl.t;
    const_context : Mir_const.context;
    bare_return_zero : bool;
    mutable result : local_id option;
    recur : recur;
  }

  (* The layers share this record because OCaml limits mutual recursion to one module *)
  and recur = {
    expr : state -> S.texpr -> operand;
    statement : state -> S.texpr -> unit;
  }

  let make ~const_context ~globals ~recur ~bare_return_zero =
    let blocks = Hashtbl.create 16 in
    Hashtbl.add blocks 0 ({ statements = []; terminator = None } : open_block);
    {
      locals_rev = ref [];
      next_local = ref 0;
      symbols = Hashtbl.create 16;
      globals;
      blocks;
      next_block = ref 1;
      current = ref 0;
      loops = ref [];
      consts = Hashtbl.create 16;
      const_context;
      bare_return_zero;
      result = None;
      recur;
    }

  let finish (state : state) =
    Array.init !(state.next_block) (fun id ->
        let b = Hashtbl.find state.blocks id in
        ({ statements = List.rev b.statements; terminator = b.terminator }
          : block))

  let add_local (state : state) ?name storage ty span =
    let id = !(state.next_local) in
    incr state.next_local;
    state.locals_rev := { name; ty; storage; span } :: !(state.locals_rev);
    id

  let finish_locals (state : state) =
    Array.of_list (List.rev !(state.locals_rev))

  let bind_symbol (state : state) (symbol : Symbol.t) id =
    Hashtbl.add state.symbols symbol.Symbol.id id

  let new_block (state : state) =
    let id = !(state.next_block) in
    incr state.next_block;
    Hashtbl.add state.blocks id { statements = []; terminator = None };
    id

  let current_block (state : state) = Hashtbl.find state.blocks !(state.current)
  let is_live (state : state) = Option.is_none (current_block state).terminator
  let switch (state : state) block = state.current := block

  let emit (state : state) desc span =
    let block = current_block state in
    if Option.is_none block.terminator then
      block.statements <- { desc; span } :: block.statements

  let terminate (state : state) desc span =
    let block = current_block state in
    if Option.is_none block.terminator then
      block.terminator <- Some { desc; span }

  (* The stack gives nested loop control the nearest matching target *)
  let with_loop (state : state) label ~continue_block ~break_block ~result body
      =
    let label =
      Option.map (fun (label : Ast.loop_label) -> label.Ast.value) label
    in
    state.loops :=
      { label; continue_block; break_block; result } :: !(state.loops);
    body ();
    state.loops :=
      match !(state.loops) with
      | _ :: loops -> loops
      | [] -> Diagnostic.ice "loop stack is empty"

  let loop_target (state : state) label span =
    let target =
      match label with
      | None -> (
          match !(state.loops) with loop :: _ -> Some loop | [] -> None)
      | Some label ->
          List.find_opt
            (fun loop -> loop.label = Some label.Ast.value)
            !(state.loops)
    in
    match target with
    | Some target -> target
    | None -> Diagnostic.ice ~span "loop target does not exist"

  let continue_target state label span =
    (loop_target state label span).continue_block

  let place place_span base = { base; projections = []; place_span }
  let local_place span id = place span (Local id)
  let copy span ty place : operand = { desc = Copy place; ty; span }

  let constant (expr : S.texpr) desc : operand =
    { desc = Const desc; ty = expr.S.ty; span = expr.S.span }

  let const_operand span ty desc : operand = { desc = Const desc; ty; span }

  let eval_const (state : state) expr =
    Mir_const.eval_in_scope state.const_context state.consts expr

  let evaluated_constant (state : state) (expr : S.texpr) =
    constant expr (Mir_const.constant_of_num expr.S.ty (eval_const state expr))

  let assign (state : state) destination (assigned : operand) =
    if is_live state then
      emit state
        (Assign (destination, { desc = Use assigned; ty = assigned.ty }))
        assigned.span

  let temp_value (state : state) ty span desc =
    let id = add_local state Temp ty span in
    let destination = local_place span id in
    emit state (Assign (destination, { desc; ty })) span;
    copy span ty destination

  let materialize (state : state) (operand : operand) =
    match operand.desc with
    | Copy place -> place
    | Const _ ->
        let id = add_local state Temp operand.ty operand.span in
        let destination = local_place operand.span id in
        assign state destination operand;
        destination

  let save_operand (state : state) (operand : operand) =
    let id = add_local state Temp operand.ty operand.span in
    let destination = local_place operand.span id in
    assign state destination operand;
    copy operand.span operand.ty destination

  let symbol_place (state : state) span (symbol : Symbol.t) =
    match Hashtbl.find_opt state.symbols symbol.Symbol.id with
    | Some id -> local_place span id
    | None -> (
        match Hashtbl.find_opt state.globals (Symbol.key symbol) with
        | Some name -> place span (Global name)
        | None ->
            Diagnostic.ice ~span
              (Printf.sprintf "no MIR place for symbol %s" symbol.Symbol.name))
end

module Mir_op = struct
  (* Source operators that survive lowering keep their meaning and the rest are gone by now *)
  let unop (op : Ast.unop) : Mir.unop =
    match op with
    | Ast.Neg -> Mir.Neg
    | Ast.Not -> Mir.Not
    | Ast.BitNot -> Mir.BitNot
    | Ast.Pos -> Diagnostic.ice "unary plus reached MIR"
    | Ast.Deref -> Diagnostic.ice "deref is a projection and not a MIR value"
    | Ast.AddressOf -> Diagnostic.ice "address of is its own MIR value"

  let binop (op : Ast.binop) : Mir.binop =
    match op with
    | Ast.Add -> Mir.Add
    | Ast.Sub -> Mir.Sub
    | Ast.Mul -> Mir.Mul
    | Ast.Div -> Mir.Div
    | Ast.Mod -> Mir.Mod
    | Ast.Eq -> Mir.Eq
    | Ast.Neq -> Mir.Neq
    | Ast.Lt -> Mir.Lt
    | Ast.Gt -> Mir.Gt
    | Ast.Lte -> Mir.Lte
    | Ast.Gte -> Mir.Gte
    | Ast.BitAnd -> Mir.BitAnd
    | Ast.BitOr -> Mir.BitOr
    | Ast.BitXor -> Mir.BitXor
    | Ast.Lshift -> Mir.Lshift
    | Ast.Rshift -> Mir.Rshift
    | Ast.And | Ast.Or -> Diagnostic.ice "short circuit reached MIR as a value"
    | Ast.Assign | Ast.AddAssign | Ast.SubAssign | Ast.MulAssign | Ast.DivAssign
    | Ast.ModAssign | Ast.BitAndAssign | Ast.BitOrAssign | Ast.BitXorAssign
    | Ast.LshiftAssign | Ast.RshiftAssign ->
        Diagnostic.ice "assignment reached MIR as a value"
end

module Mir_check = struct
  open Types
  open Mir
  module B = Mir_builder

  let emit_check state check span =
    if B.is_live state then begin
      let fail = B.new_block state in
      let ok = B.new_block state in
      B.terminate state (Assert (check, ok, fail)) span;
      B.switch state fail;
      B.terminate state (Panic check) span;
      B.switch state ok
    end

  let bounds state index length span =
    emit_check state (Bounds (index, length)) span

  let slice_bounds state lo hi length span =
    emit_check state (SliceBounds (lo, hi, length)) span

  let null state pointer span = emit_check state (Null pointer) span
  let div_zero state divisor span = emit_check state (DivZero divisor) span

  let negative_shift state count span =
    emit_check state (NegativeShift count) span

  (* Plain and compound arithmetic guard the same two operators the same way *)
  let arith state op ~operand_ty (right : operand) span =
    match op with
    | (Ast.Div | Ast.Mod) when not (is_float operand_ty) ->
        div_zero state right span
    | (Ast.Lshift | Ast.Rshift) when not (is_unsigned right.ty) ->
        negative_shift state right span
    | _ -> ()
end

module Mir_control = struct
  open Types
  open Mir
  module S = Typed_ast
  module B = Mir_builder

  let lower_expr state expr = state.B.recur.B.expr state expr
  let lower_statement state expr = state.B.recur.B.statement state expr

  let rec lower_short_circuit state (expr : S.texpr) left right short_value =
    let right_block = B.new_block state in
    let short_block = B.new_block state in
    let join_block = B.new_block state in
    let result = B.add_local state Temp TBool expr.S.span in
    lower_branch state left
      (if short_value then short_block else right_block)
      (if short_value then right_block else short_block);
    B.switch state short_block;
    B.assign state
      (B.local_place expr.S.span result)
      (B.const_operand expr.S.span TBool (Bool short_value));
    B.terminate state (Jump join_block) expr.S.span;
    B.switch state right_block;
    let right = lower_expr state right in
    B.assign state (B.local_place expr.S.span result) right;
    if B.is_live state then B.terminate state (Jump join_block) expr.S.span;
    B.switch state join_block;
    B.copy expr.S.span TBool (B.local_place expr.S.span result)

  and lower_if state (expr : S.texpr) branches else_body =
    let result =
      if expr.S.ty = TUnit || expr.S.ty = TNever then None
      else Some (B.add_local state Temp expr.S.ty expr.S.span)
    in
    let join = B.new_block state in
    let rec lower_branches = function
      | [] -> (
          match else_body with
          | Some body -> lower_arm state expr result join body
          | None -> B.terminate state (Jump join) expr.S.span)
      | (condition, body) :: rest ->
          let yes = B.new_block state in
          let no = B.new_block state in
          lower_branch state condition yes no;
          B.switch state yes;
          lower_arm state expr result join body;
          B.switch state no;
          lower_branches rest
    in
    lower_branches branches;
    B.switch state join;
    if expr.S.ty = TNever then B.terminate state Unreachable expr.S.span;
    match result with
    | Some result ->
        B.copy expr.S.span expr.S.ty (B.local_place expr.S.span result)
    | None -> B.constant expr Undef

  (* A pattern walks the scrutinee and gathers what to compare and what to name *)
  and pattern_plan (place : place) (ty : ty) (pat : S.tpattern) =
    match pat with
    | S.TPatWild -> ([], [])
    | S.TPatBind (symbol, bound_ty) -> ([], [ (symbol, bound_ty, place) ])
    | S.TPatConst value -> ([ (place, ty, value) ], [])

  (* One test per arm because a jump table only pays off on a dense range *)
  and lower_match state (expr : S.texpr) (scrutinee : S.texpr) arms =
    let result =
      if expr.S.ty = TUnit || expr.S.ty = TNever then None
      else Some (B.add_local state Temp expr.S.ty expr.S.span)
    in
    (* The scrutinee is a place so a field pattern can project into it *)
    let subject = B.materialize state (lower_expr state scrutinee) in
    let join = B.new_block state in
    let wanted ty value =
      match resolve_ty ty with
      | TBool -> Bool (value <> 0L)
      | TChar -> Char (Int64.to_int value)
      | _ -> Int value
    in
    let bind (symbol, bound_ty, place) =
      let id =
        B.add_local state ~name:symbol.Symbol.name User bound_ty expr.S.span
      in
      B.bind_symbol state symbol id;
      B.assign state
        (B.local_place expr.S.span id)
        (B.copy expr.S.span bound_ty place)
    in
    let rec lower_tests fail = function
      | [] -> ()
      | (place, ty, value) :: rest ->
          let next = B.new_block state in
          let against = B.const_operand expr.S.span ty (wanted ty value) in
          let found = B.copy expr.S.span ty place in
          let equal =
            B.temp_value state TBool expr.S.span (Binary (Eq, found, against))
          in
          B.terminate state (Branch (equal, next, fail)) expr.S.span;
          B.switch state next;
          lower_tests fail rest
    in
    let rec lower_arms = function
      (* Nothing checks coverage so falling out of the last test is undefined *)
      | [] -> B.terminate state Unreachable expr.S.span
      | { S.tpat; tbody } :: rest -> (
          let tests, binds = pattern_plan subject scrutinee.S.ty tpat in
          let no = if tests = [] then None else Some (B.new_block state) in
          Option.iter (fun no -> lower_tests no tests) no;
          List.iter bind binds;
          lower_arm state expr result join tbody;
          match no with
          | Some no ->
              B.switch state no;
              lower_arms rest
          | None -> ())
    in
    lower_arms arms;
    B.switch state join;
    if expr.S.ty = TNever then B.terminate state Unreachable expr.S.span;
    match result with
    | Some result ->
        B.copy expr.S.span expr.S.ty (B.local_place expr.S.span result)
    | None -> B.constant expr Undef

  and lower_arm state (expr : S.texpr) result join body =
    match result with
    | None ->
        List.iter (lower_statement state) body;
        if B.is_live state then B.terminate state (Jump join) expr.S.span
    | Some result ->
        let value =
          match List.rev body with
          | [] -> B.constant expr Undef
          | last :: reversed ->
              List.iter (lower_statement state) (List.rev reversed);
              if B.is_live state then lower_expr state last
              else B.constant expr Undef
        in
        if B.is_live state then begin
          B.assign state (B.local_place expr.S.span result) value;
          B.terminate state (Jump join) expr.S.span
        end

  and lower_branch state (expr : S.texpr) yes no =
    match expr.S.desc with
    | S.TBool value ->
        B.terminate state (Jump (if value then yes else no)) expr.S.span
    | S.TUnOp (Ast.Not, inner) -> lower_branch state inner no yes
    | S.TBinOp (Ast.And, left, right) ->
        let middle = B.new_block state in
        lower_branch state left middle no;
        B.switch state middle;
        lower_branch state right yes no
    | S.TBinOp (Ast.Or, left, right) ->
        let middle = B.new_block state in
        lower_branch state left yes middle;
        B.switch state middle;
        lower_branch state right yes no
    | _ ->
        let condition = lower_expr state expr in
        if B.is_live state then
          B.terminate state (Branch (condition, yes, no)) expr.S.span

  and lower_while state span label condition body =
    let condition_block = B.new_block state in
    let body_block = B.new_block state in
    let exit_block = B.new_block state in
    B.terminate state (Jump condition_block) span;
    B.switch state condition_block;
    lower_branch state condition body_block exit_block;
    B.switch state body_block;
    B.with_loop state label ~continue_block:condition_block
      ~break_block:exit_block ~result:None (fun () ->
        List.iter (lower_statement state) body);
    if B.is_live state then B.terminate state (Jump condition_block) span;
    B.switch state exit_block

  and lower_for state span label symbol elem_ty (iter : S.texpr) body =
    match iter.S.desc with
    | S.TRange (lo, hi) ->
        lower_range_for state span label symbol elem_ty lo hi false body
    | S.TRangeInclusive (lo, hi) ->
        lower_range_for state span label symbol elem_ty lo hi true body
    | _ -> lower_each_for state span label symbol elem_ty iter body

  and lower_range_for state span label (symbol : Symbol.t) elem_ty lo hi
      inclusive body =
    let loop_id =
      B.add_local state ~name:symbol.Symbol.name User elem_ty symbol.Symbol.span
    in
    B.bind_symbol state symbol loop_id;
    let lo = lower_expr state lo in
    let hi = lower_expr state hi in
    B.assign state (B.local_place span loop_id) lo;
    let high_id = B.add_local state ~name:"for.hi" Temp elem_ty hi.span in
    B.assign state (B.local_place span high_id) hi;
    let condition_block = B.new_block state in
    let body_block = B.new_block state in
    let step_block = B.new_block state in
    let exit_block = B.new_block state in
    B.terminate state (Jump condition_block) span;
    B.switch state condition_block;
    let current = B.copy span elem_ty (B.local_place span loop_id) in
    let high = B.copy span elem_ty (B.local_place span high_id) in
    let op = if inclusive then Mir.Lte else Mir.Lt in
    let condition =
      B.temp_value state TBool span (Binary (op, current, high))
    in
    B.terminate state (Branch (condition, body_block, exit_block)) span;
    B.switch state body_block;
    B.with_loop state label ~continue_block:step_block ~break_block:exit_block
      ~result:None (fun () -> List.iter (lower_statement state) body);
    if B.is_live state then B.terminate state (Jump step_block) span;
    B.switch state step_block;
    let current = B.copy span elem_ty (B.local_place span loop_id) in
    let high = B.copy span elem_ty (B.local_place span high_id) in
    if inclusive then begin
      let increment_block = B.new_block state in
      let done_ =
        B.temp_value state TBool span (Binary (Mir.Eq, current, high))
      in
      B.terminate state (Branch (done_, exit_block, increment_block)) span;
      B.switch state increment_block
    end;
    let one = B.const_operand span elem_ty (Int 1L) in
    let next =
      B.temp_value state elem_ty span (Binary (Mir.Add, current, one))
    in
    B.assign state (B.local_place span loop_id) next;
    B.terminate state (Jump condition_block) span;
    B.switch state exit_block

  and lower_each_for state span label (symbol : Symbol.t) elem_ty
      (iter : S.texpr) body =
    let source = lower_expr state iter |> B.materialize state in
    let source =
      match resolve_ty iter.S.ty with
      | TSlice _ ->
          let id = B.add_local state Temp iter.S.ty iter.S.span in
          let snapshot = B.local_place iter.S.span id in
          B.assign state snapshot (B.copy iter.S.span iter.S.ty source);
          snapshot
      | _ -> source
    in
    let pointer_ty = TPointer elem_ty in
    let pointer_id = B.add_local state Temp pointer_ty span in
    let length_id = B.add_local state Temp (TInt Usize) span in
    let index_id = B.add_local state Temp (TInt Usize) span in
    let loop_id =
      B.add_local state ~name:symbol.Symbol.name User elem_ty symbol.Symbol.span
    in
    B.bind_symbol state symbol loop_id;
    let pointer = B.temp_value state pointer_ty span (DataPtr source) in
    let length = B.temp_value state (TInt Usize) span (Len source) in
    B.assign state (B.local_place span pointer_id) pointer;
    B.assign state (B.local_place span length_id) length;
    B.assign state
      (B.local_place span index_id)
      (B.const_operand span (TInt Usize) (Int 0L));
    let condition_block = B.new_block state in
    let body_block = B.new_block state in
    let step_block = B.new_block state in
    let exit_block = B.new_block state in
    B.terminate state (Jump condition_block) span;
    B.switch state condition_block;
    let index = B.copy span (TInt Usize) (B.local_place span index_id) in
    let length = B.copy span (TInt Usize) (B.local_place span length_id) in
    let condition =
      B.temp_value state TBool span (Binary (Mir.Lt, index, length))
    in
    B.terminate state (Branch (condition, body_block, exit_block)) span;
    B.switch state body_block;
    let element =
      {
        base = Local pointer_id;
        projections = [ Index index ];
        place_span = span;
      }
    in
    B.assign state (B.local_place span loop_id) (B.copy span elem_ty element);
    B.with_loop state label ~continue_block:step_block ~break_block:exit_block
      ~result:None (fun () -> List.iter (lower_statement state) body);
    if B.is_live state then B.terminate state (Jump step_block) span;
    B.switch state step_block;
    let current = B.copy span (TInt Usize) (B.local_place span index_id) in
    let one = B.const_operand span (TInt Usize) (Int 1L) in
    let next =
      B.temp_value state (TInt Usize) span (Binary (Mir.Add, current, one))
    in
    B.assign state (B.local_place span index_id) next;
    B.terminate state (Jump condition_block) span;
    B.switch state exit_block

  and lower_loop state span label ty body =
    let body_block = B.new_block state in
    let exit_block = B.new_block state in
    let result =
      match ty with
      | TUnit | TNever -> None
      | _ ->
          let id = B.add_local state Temp ty span in
          Some (B.local_place span id)
    in
    B.terminate state (Jump body_block) span;
    B.switch state body_block;
    B.with_loop state label ~continue_block:body_block ~break_block:exit_block
      ~result:(Option.map (fun place -> (place, ty)) result)
      (fun () -> List.iter (lower_statement state) body);
    if B.is_live state then B.terminate state (Jump body_block) span;
    B.switch state exit_block;
    match result with
    | Some result -> B.copy span ty result
    | None -> B.const_operand span ty Undef
end

module Mir_expr = struct
  open Types
  open Mir
  module S = Typed_ast
  module B = Mir_builder

  (* INT_MIN / -1 wraps back to INT_MIN and INT_MIN % -1 is 0 so a -1 divisor skips the divide *)
  let lower_guarded_div state span ty op (left : operand) (right : operand) =
    let result = B.add_local state Temp ty span in
    let destination = B.local_place span result in
    let negative_one = B.const_operand span right.ty (Int (-1L)) in
    let divisor_is_negative_one =
      B.temp_value state TBool span (Binary (Mir.Eq, right, negative_one))
    in
    let wrap_block = B.new_block state in
    let divide_block = B.new_block state in
    let join_block = B.new_block state in
    B.terminate state
      (Branch (divisor_is_negative_one, wrap_block, divide_block))
      span;
    B.switch state wrap_block;
    let wrapped =
      if op = Mir.Mod then B.const_operand span ty (Int 0L)
      else B.temp_value state ty span (Unary (Mir.Neg, left))
    in
    B.assign state destination wrapped;
    B.terminate state (Jump join_block) span;
    B.switch state divide_block;
    let divided = B.temp_value state ty span (Binary (op, left, right)) in
    B.assign state destination divided;
    B.terminate state (Jump join_block) span;
    B.switch state join_block;
    B.copy span ty destination

  (* A count at or past the type width shifts every bit out so the shift itself never runs *)
  let lower_guarded_shift state span ty op (left : operand) (right : operand) =
    let result = B.add_local state Temp ty span in
    let destination = B.local_place span result in
    let bits = 8 * int_kind_size (int_kind_of ty) in
    let width = B.const_operand span right.ty (Int (Int64.of_int bits)) in
    let count_in_range =
      B.temp_value state TBool span (Binary (Mir.Lt, right, width))
    in
    let shift_block = B.new_block state in
    let drained_block = B.new_block state in
    let join_block = B.new_block state in
    B.terminate state (Branch (count_in_range, shift_block, drained_block)) span;
    B.switch state shift_block;
    let shifted = B.temp_value state ty span (Binary (op, left, right)) in
    B.assign state destination shifted;
    B.terminate state (Jump join_block) span;
    B.switch state drained_block;
    (* A signed right shift keeps filling with the sign bit so it settles at the top bit *)
    let drained =
      if op = Mir.Rshift && not (is_unsigned ty) then
        let top =
          B.const_operand span right.ty (Int (Int64.of_int (bits - 1)))
        in
        B.temp_value state ty span (Binary (Mir.Rshift, left, top))
      else B.const_operand span ty (Int 0L)
    in
    B.assign state destination drained;
    B.terminate state (Jump join_block) span;
    B.switch state join_block;
    B.copy span ty destination

  let lower_binary state span ty (op : Ast.binop) (left : operand)
      (right : operand) =
    let lowered = Mir_op.binop op in
    match op with
    | (Ast.Div | Ast.Mod) when div_int_needs_check left.ty ->
        lower_guarded_div state span ty lowered left right
    | Ast.Lshift | Ast.Rshift ->
        lower_guarded_shift state span ty lowered left right
    | _ -> B.temp_value state ty span (Binary (lowered, left, right))

  let rec map_operands state = function
    | [] -> []
    | expr :: rest ->
        let value : operand = lower_expr state expr in
        if value.ty = TUnit then map_operands state rest
        else
          let value =
            if is_aggregate value.ty then
              B.copy value.span value.ty (B.materialize state value)
            else value
          in
          value :: map_operands state rest

  and lower_expr state (expr : S.texpr) =
    match expr.S.desc with
    | S.TErrorExpr ->
        Diagnostic.ice ~span:expr.S.span "error expression reached MIR"
    | S.TInt value -> B.constant expr (Int value)
    | S.TVariant (_, value) -> B.constant expr (Int value)
    | S.TFloat value -> B.constant expr (Float value)
    | S.TBool value -> B.constant expr (Bool value)
    | S.TNull -> B.constant expr Null
    | S.TCStr value -> B.constant expr (CStr value)
    | S.TStr value -> B.constant expr (Str value)
    | S.TChar value -> B.constant expr (Char value)
    | S.TZero -> B.constant expr Zero
    | S.TUndef -> B.constant expr Undef
    | S.TIdent _ when expr.S.ty = TUnit -> B.constant expr Undef
    | S.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
        B.constant expr (Function symbol.Symbol.link_name)
    | S.TIdent symbol
      when Symbol.is_comptime symbol.Symbol.kind
           || Mir_const.is_comptime_global state.B.const_context
                (Symbol.key symbol) ->
        B.evaluated_constant state expr
    | S.TIdent symbol ->
        B.copy expr.S.span expr.S.ty (B.symbol_place state expr.S.span symbol)
    | S.TCall (callee, args, variadic_start) ->
        lower_call state expr callee args variadic_start
    | S.TBinOp (Ast.And, left, right) ->
        Mir_control.lower_short_circuit state expr left right false
    | S.TBinOp (Ast.Or, left, right) ->
        Mir_control.lower_short_circuit state expr left right true
    | S.TBinOp (Ast.Assign, left, right) ->
        let assigned = lower_expr state right in
        if left.S.ty <> TUnit then begin
          let destination = lower_place state left in
          B.assign state destination assigned
        end;
        assigned
    | S.TBinOp (op, left, right) when base_binop op <> None ->
        lower_compound_assign state expr op left right
    | S.TBinOp (op, left, right) ->
        let left = lower_expr state left in
        let right = lower_expr state right in
        Mir_check.arith state op ~operand_ty:left.ty right expr.S.span;
        lower_binary state expr.S.span expr.S.ty op left right
    | S.TUnOp (Ast.Pos, inner) -> lower_expr state inner
    | S.TUnOp (Ast.AddressOf, { S.desc = S.TUnOp (Ast.Deref, inner); _ }) ->
        lower_expr state inner
    | S.TUnOp (Ast.AddressOf, ({ S.desc = S.TIdent symbol; _ } as inner))
      when Symbol.is_func symbol.Symbol.kind ->
        let function_value = lower_expr state inner in
        { function_value with ty = expr.S.ty; span = expr.S.span }
    | S.TUnOp (Ast.AddressOf, inner) ->
        let inner = lower_place state inner in
        B.temp_value state expr.S.ty expr.S.span (AddressOf inner)
    | S.TUnOp (Ast.Deref, _) ->
        let source = lower_place state expr in
        B.copy expr.S.span expr.S.ty source
    | S.TUnOp (op, inner) ->
        let inner = lower_expr state inner in
        B.temp_value state expr.S.ty expr.S.span (Unary (Mir_op.unop op, inner))
    | S.TFieldAccess _ | S.TIndex _ ->
        let source = lower_place state expr in
        B.copy expr.S.span expr.S.ty source
    | S.TCast inner ->
        let inner = lower_expr state inner in
        B.temp_value state expr.S.ty expr.S.span (Cast inner)
    | S.TSizeOf ty -> B.temp_value state expr.S.ty expr.S.span (SizeOf ty)
    | S.TRange _ | S.TRangeInclusive _ ->
        Diagnostic.ice ~span:expr.S.span "range outside a for loop"
    | S.TArrayLit elements -> lower_array_literal state expr elements
    | S.TLen inner ->
        let inner = lower_expr state inner |> B.materialize state in
        B.temp_value state expr.S.ty expr.S.span (Len inner)
    | S.TSliceExpr (base, lo, hi) -> lower_slice state expr base lo hi
    | S.TDataPtr inner ->
        let inner = lower_expr state inner |> B.materialize state in
        B.temp_value state expr.S.ty expr.S.span (DataPtr inner)
    | S.TStructLit (_, fields) -> lower_struct_literal state expr fields
    | S.TLocalDecl -> B.constant expr Undef
    | S.TLoop (label, body) ->
        Mir_control.lower_loop state expr.S.span label expr.S.ty body
    | S.TBlock body -> lower_block_value state expr body
    | S.TIf (branches, else_body) ->
        Mir_control.lower_if state expr branches else_body
    | S.TMatch (scrutinee, arms) ->
        Mir_control.lower_match state expr scrutinee arms
    | S.TWhile (label, condition, body) ->
        Mir_control.lower_while state expr.S.span label condition body;
        B.constant expr Undef
    | S.TFor (label, symbol, elem_ty, iter, body) ->
        Mir_control.lower_for state expr.S.span label symbol elem_ty iter body;
        B.constant expr Undef
    | S.TBinding _ | S.TReturn _ | S.TBreak _ | S.TContinue _ | S.TPairAssign _
      ->
        lower_statement state expr;
        B.constant expr Undef
    | S.TUnit -> B.constant expr Undef

  and lower_call state (expr : S.texpr) (callee : S.texpr) args variadic_start =
    let args = map_operands state args in
    let callee_value, kind =
      match callee.S.desc with
      | S.TIdent symbol when Symbol.is_func symbol.Symbol.kind ->
          let kind =
            match resolve_ty callee.S.ty with
            | TFunc (_, _, C) -> External
            | _ -> Internal
          in
          (Direct symbol.Symbol.link_name, kind)
      | _ ->
          let kind =
            match resolve_ty callee.S.ty with
            | TFunc (_, _, C) -> External
            | _ -> Internal
          in
          (Indirect (lower_expr state callee), kind)
    in
    let destination =
      if expr.S.ty = TUnit || expr.S.ty = TNever then None
      else
        let id = B.add_local state Temp expr.S.ty expr.S.span in
        Some (B.local_place expr.S.span id)
    in
    B.emit state
      (Call
         {
           destination;
           callee = callee_value;
           kind;
           args;
           return_ty = expr.S.ty;
           variadic_start;
         })
      expr.S.span;
    if expr.S.ty = TNever then B.terminate state Unreachable expr.S.span;
    match destination with
    | Some destination -> B.copy expr.S.span expr.S.ty destination
    | None -> B.constant expr Undef

  and lower_place state (expr : S.texpr) =
    match expr.S.desc with
    | S.TIdent symbol -> B.symbol_place state expr.S.span symbol
    | S.TUnOp (Ast.Deref, inner) ->
        let pointer = lower_expr state inner in
        Mir_check.null state pointer expr.S.span;
        let source = B.materialize state pointer in
        { source with projections = source.projections @ [ Deref ] }
    | S.TFieldAccess (base, field) ->
        let source =
          match resolve_ty base.S.ty with
          | TPointer _ ->
              let pointer = lower_expr state base in
              Mir_check.null state pointer base.S.span;
              let source = B.materialize state pointer in
              { source with projections = source.projections @ [ Deref ] }
          | _ -> lower_expr state base |> B.materialize state
        in
        { source with projections = source.projections @ [ Field field ] }
    | S.TIndex (base, index) ->
        let base_value = lower_expr state base in
        let source = B.materialize state base_value in
        let index = lower_expr state index in
        (match resolve_ty base.S.ty with
        | TArray _ | TSlice _ ->
            let length =
              B.temp_value state (TInt Usize) base.S.span (Len source)
            in
            Mir_check.bounds state index length expr.S.span
        | TPointer _ -> ()
        | _ ->
            let message = "index on non indexed MIR place" in
            Diagnostic.ice ~span:base.S.span message);
        { source with projections = source.projections @ [ Index index ] }
    | _ -> lower_expr state expr |> B.materialize state

  and lower_array_literal state (expr : S.texpr) elements =
    let element_ty =
      match resolve_ty expr.S.ty with
      | TArray (element, _) -> element
      | _ -> Diagnostic.ice ~span:expr.S.span "array literal has non array type"
    in
    let id = B.add_local state Temp expr.S.ty expr.S.span in
    let destination = B.local_place expr.S.span id in
    B.emit state
      (Assign
         (destination, { desc = Use (B.constant expr Undef); ty = expr.S.ty }))
      expr.S.span;
    List.iteri
      (fun index (element : S.texpr) ->
        let assigned = lower_expr state element in
        let index_operand =
          B.const_operand element.S.span (TInt Usize) (Int (Int64.of_int index))
        in
        let target =
          { destination with projections = [ Index index_operand ] }
        in
        let assigned = { assigned with ty = element_ty } in
        B.assign state target assigned)
      elements;
    B.copy expr.S.span expr.S.ty destination

  and lower_struct_literal state (expr : S.texpr) fields =
    let id = B.add_local state Temp expr.S.ty expr.S.span in
    let destination = B.local_place expr.S.span id in
    B.emit state
      (Assign
         (destination, { desc = Use (B.constant expr Zero); ty = expr.S.ty }))
      expr.S.span;
    List.iter
      (fun (field, value) ->
        let assigned = lower_expr state value in
        let target = { destination with projections = [ Field field ] } in
        B.assign state target assigned)
      fields;
    B.copy expr.S.span expr.S.ty destination

  and lower_slice state (expr : S.texpr) base lo hi =
    let base = lower_expr state base |> B.materialize state in
    let lo = lower_expr state lo in
    let hi = lower_expr state hi in
    let length = B.temp_value state (TInt Usize) expr.S.span (Len base) in
    Mir_check.slice_bounds state lo hi length expr.S.span;
    let id = B.add_local state Temp expr.S.ty expr.S.span in
    let destination = B.local_place expr.S.span id in
    B.emit state (Slice (destination, base, lo, hi)) expr.S.span;
    B.copy expr.S.span expr.S.ty destination

  and lower_block_value state (expr : S.texpr) body =
    match List.rev body with
    | [] -> B.constant expr Undef
    | last :: reversed ->
        List.iter (lower_statement state) (List.rev reversed);
        if B.is_live state then lower_expr state last else B.constant expr Undef

  and base_binop = function
    | Ast.AddAssign -> Some Ast.Add
    | Ast.SubAssign -> Some Ast.Sub
    | Ast.MulAssign -> Some Ast.Mul
    | Ast.DivAssign -> Some Ast.Div
    | Ast.ModAssign -> Some Ast.Mod
    | Ast.BitAndAssign -> Some Ast.BitAnd
    | Ast.BitOrAssign -> Some Ast.BitOr
    | Ast.BitXorAssign -> Some Ast.BitXor
    | Ast.LshiftAssign -> Some Ast.Lshift
    | Ast.RshiftAssign -> Some Ast.Rshift
    | _ -> None

  and lower_compound_assign state (expr : S.texpr) op (left : S.texpr) right =
    let target = lower_place state left in
    let old = B.copy left.S.span left.S.ty target in
    let right = lower_expr state right in
    let op = Option.get (base_binop op) in
    Mir_check.arith state op ~operand_ty:old.ty right expr.S.span;
    let updated = lower_binary state expr.S.span left.S.ty op old right in
    B.assign state target updated;
    updated

  and lower_pair_assign state first_target second_target first_value
      second_value =
    let first_value = lower_expr state first_value |> B.save_operand state in
    let second_value = lower_expr state second_value |> B.save_operand state in
    let first_target = lower_place state first_target in
    B.assign state first_target first_value;
    let second_target = lower_place state second_target in
    B.assign state second_target second_value

  and lower_statement state (expr : S.texpr) =
    if B.is_live state then
      match expr.S.desc with
      | S.TBinding (_, _, ty, init) when ty = TNever || init.S.ty = TNever ->
          ignore (lower_expr state init)
      (* A unit local still gets a slot so you can take its address, it just has no bytes to store *)
      | S.TBinding (_, symbol, TUnit, init) ->
          let id =
            B.add_local state ~name:symbol.Symbol.name User TUnit
              symbol.Symbol.span
          in
          B.bind_symbol state symbol id;
          ignore (lower_expr state init)
      | S.TBinding (Ast.Comptime, symbol, _, init) ->
          Hashtbl.replace state.B.consts (Symbol.key symbol)
            (B.eval_const state init)
      | S.TBinding (_, symbol, ty, init) ->
          let id =
            B.add_local state ~name:symbol.Symbol.name User ty
              symbol.Symbol.span
          in
          B.bind_symbol state symbol id;
          let init = lower_expr state init in
          if B.is_live state then
            B.assign state (B.local_place expr.S.span id) init
      | S.TReturn (Some value) when state.B.result <> None ->
          let returned = lower_expr state value in
          let result = Option.get state.B.result in
          if B.is_live state then begin
            B.assign state (B.local_place expr.S.span result) returned;
            B.terminate state (ReturnValue None) expr.S.span
          end
      | S.TReturn (Some value) when value.S.ty = TUnit ->
          ignore (lower_expr state value);
          if B.is_live state then
            B.terminate state (ReturnValue None) expr.S.span
      | S.TReturn returned ->
          let returned =
            match returned with
            | Some value -> Some (lower_expr state value)
            | None when state.B.bare_return_zero ->
                Some (B.const_operand expr.S.span (TInt I32) (Int 0L))
            | None -> None
          in
          if B.is_live state then
            B.terminate state (ReturnValue returned) expr.S.span
      | S.TBreak (label, value) ->
          let target = B.loop_target state label expr.S.span in
          Option.iter
            (fun value ->
              let lowered = lower_expr state value in
              match target.B.result with
              | Some (result, result_ty) ->
                  let lowered =
                    (* A later value can widen the inferred loop type *)
                    if lowered.ty = TNever || ty_equal lowered.ty result_ty then
                      lowered
                    else
                      B.temp_value state result_ty value.S.span (Cast lowered)
                  in
                  B.assign state result lowered
              | None when value.S.ty = TUnit -> ()
              | None -> Diagnostic.ice ~span:expr.S.span "loop has no result")
            value;
          B.terminate state (Jump target.B.break_block) expr.S.span
      | S.TContinue label ->
          B.terminate state
            (Jump (B.continue_target state label expr.S.span))
            expr.S.span
      | S.TWhile (label, condition, body) ->
          Mir_control.lower_while state expr.S.span label condition body
      | S.TFor (label, symbol, elem_ty, iter, body) ->
          Mir_control.lower_for state expr.S.span label symbol elem_ty iter body
      | S.TLoop (label, body) ->
          ignore (Mir_control.lower_loop state expr.S.span label expr.S.ty body)
      | S.TPairAssign (first_target, second_target, first_value, second_value)
        ->
          lower_pair_assign state first_target second_target first_value
            second_value
      | S.TBlock body -> List.iter (lower_statement state) body
      | _ ->
          ignore (lower_expr state expr);
          if expr.S.ty = TNever && B.is_live state then
            B.terminate state Unreachable expr.S.span

  let recur : B.recur = { B.expr = lower_expr; B.statement = lower_statement }
end

module S = Typed_ast
module B = Mir_builder

let build_func const_context globals (func : S.tfunc_def) : Mir.func =
  let state =
    B.make ~const_context ~globals ~recur:Mir_expr.recur
      ~bare_return_zero:
        (func.S.entry_point && func.S.ret_ty = Types.TInt Types.I32)
  in
  let span =
    match func.S.body with
    | first :: _ -> first.S.span
    | [] -> (
        match func.S.params with
        | (symbol, _) :: _ -> symbol.Symbol.span
        | [] -> Ast.dummy_span)
  in
  (* A returned aggregate needs somewhere to live that outlives the frame *)
  (* TODO: Only aggregates get one right now. If every function had one then inlining would just be dropping the body in and reading that slot. It would also save building a value into a temp and copying it over *)
  if Types.is_aggregate func.S.ret_ty then
    state.B.result <-
      Some (B.add_local state ~name:"result" Mir.Result func.S.ret_ty span);
  let params =
    List.filter_map
      (fun ((symbol : Symbol.t), ty) ->
        if ty = Types.TUnit then None
        else
          let id =
            B.add_local state ~name:symbol.Symbol.name Mir.Param ty
              symbol.Symbol.span
          in
          B.bind_symbol state symbol id;
          Some id)
      func.S.params
  in
  List.iter (Mir_expr.lower_statement state) func.S.body;
  if B.is_live state then
    if func.S.entry_point && func.S.ret_ty = Types.TInt Types.I32 then
      B.terminate state
        (Mir.ReturnValue
           (Some (B.const_operand span (Types.TInt Types.I32) (Mir.Int 0L))))
        span
    else if func.S.ret_ty = Types.TUnit then
      B.terminate state (Mir.ReturnValue None) span
    else B.terminate state Mir.Unreachable span;
  {
    Mir.name = func.S.name;
    source_name = func.S.source_name;
    public = List.mem Ast.Pub func.S.modifiers;
    params;
    result = state.B.result;
    locals = B.finish_locals state;
    blocks = B.finish state;
    return_ty = func.S.ret_ty;
    entry_point = func.S.entry_point;
    span;
  }

let build (declarations : S.tdecl list) : Mir.program =
  let const_context = Mir_const.make_context declarations in
  let globals_by_id = Hashtbl.create 16 in
  let structs_rev = ref [] in
  let globals_rev = ref [] in
  let functions_rev = ref [] in
  List.iter
    (function
      | S.TStruct (name, fields, _) ->
          structs_rev := { Mir.name; fields; local = false } :: !structs_rev
      | S.TLocalStruct (name, fields) ->
          structs_rev := { Mir.name; fields; local = true } :: !structs_rev
      | S.TGlobal global when global.S.ty = Types.TUnit -> ()
      | S.TGlobal global when global.S.kind <> Ast.Comptime ->
          Hashtbl.add globals_by_id global.S.key global.S.name;
          globals_rev :=
            {
              Mir.name = global.S.name;
              ty = global.S.ty;
              init =
                Option.map (Mir_const.global_init const_context) global.S.init;
              public = List.mem Ast.Pub global.S.modifiers;
            }
            :: !globals_rev
      | S.TFunc func -> functions_rev := func :: !functions_rev
      | S.TGlobal _ | S.TExtern _ | S.TTypeAlias _ | S.TNewtype _ | S.TEnum _ ->
          ())
    declarations;
  let structs = List.rev !structs_rev in
  let globals = List.rev !globals_rev in
  let functions =
    List.rev !functions_rev |> List.map (build_func const_context globals_by_id)
  in
  { Mir.structs; globals; functions }
