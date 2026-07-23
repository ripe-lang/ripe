(* SPDX-License-Identifier: GPL-2.0-only *)

(* https://c9x.me/compile/doc/il.html *)
open Types
open Const_eval
module T = Core

(* TODO(7e23): I need to think about the layout/offset/padding of Structs because C++ treats empty structs as size 1. *)

type qbe_base = W | L | S | D

let qbe_base (t : ty) : qbe_base =
  match resolve_ty t with
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TBool -> W
  (* FIXME(d969): Null terminated strings? Idk yet. *)
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ ->
      L
  | TFloat F32 -> S
  | TFloat F64 -> D
  | TStruct _ | TArray _ | TSlice _ -> L
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no QBE base type"
  | TNever -> Error.ice "TNever has no QBE base type"
  | TError -> Error.ice "TError has no QBE base type"

let qbe_ty (t : ty) : string =
  match qbe_base t with W -> "w" | L -> "l" | S -> "s" | D -> "d"

let qbe_id id = if id < 0 then Printf.sprintf "n%d" (-id) else string_of_int id

(* the QBE mnemonic prefix, u for unsigned int types and pointers, s otherwise *)
let signedness (t : ty) : string =
  match resolve_ty t with
  | TPointer _ | TOpaquePtr | TNull | TCStr -> "u"
  | t -> if is_unsigned t then "u" else "s"

let div_overflows_at_reg_width (t : ty) : bool =
  match resolve_ty t with TInt (I32 | I64 | Isize) -> true | _ -> false

(* past this many bytes a libc call beats emitting one instruction per word *)
let bulk_mem_threshold = 64

(* s_ for single, d_ for double *)
let float_lit (ty : ty) (f : float) : string =
  let prefix, digits =
    match resolve_ty ty with TFloat F32 -> ("s_", 9) | _ -> ("d_", 17)
  in
  prefix ^ Printf.sprintf "%.*g" digits f

let format_const_num (ty : ty) (n : const_num) : string =
  match n with
  | Ni32 n -> Int32.to_string n
  | Ni64 n -> Int64.to_string n
  | Nf f -> float_lit ty f

(* TODO(1aff): maybe look into escape analysis *)
let rec alloc_instr (t : ty) : string =
  match resolve_ty t with
  | TInt (I64 | U64 | Isize | Usize)
  | TFloat F64
  | TPointer _ | TOpaquePtr | TNull | TCStr | TStruct _ | TFunc _ ->
      "alloc8"
  | TArray (e, _) -> alloc_instr e
  | TSlice _ -> "alloc8"
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TFloat F32 | TBool -> "alloc4"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no alloc instruction"
  | TNever -> Error.ice "TNever has no alloc instruction"
  | TError -> Error.ice "TError has no alloc instruction"

let qbe_load (t : ty) : string =
  match resolve_ty t with
  | TInt I8 -> "loadsb"
  | TInt U8 | TBool -> "loadub"
  | TInt I16 -> "loadsh"
  | TInt U16 -> "loaduh"
  | TInt I32 -> "loadsw"
  | TInt U32 -> "loaduw"
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ ->
      "loadl"
  | TFloat F32 -> "loads"
  | TFloat F64 -> "loadd"
  | TStruct _ | TArray _ | TSlice _ -> "loadl"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no load instruction"
  | TNever -> Error.ice "TNever has no load instruction"
  | TError -> Error.ice "TError has no load instruction"

let qbe_store (t : ty) : string =
  match resolve_ty t with
  | TInt (I8 | U8) | TBool -> "storeb"
  | TInt (I16 | U16) -> "storeh"
  | TInt (I32 | U32) -> "storew"
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ ->
      "storel"
  | TFloat F32 -> "stores"
  | TFloat F64 -> "stored"
  | TStruct _ | TArray _ | TSlice _ -> "storel"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no store instruction"
  | TNever -> Error.ice "TNever has no store instruction"
  | TError -> Error.ice "TError has no store instruction"

type inline_frame = {
  name : string;
  tag : int;
  res_slot : string option;
  ret_ty : ty;
  end_lbl : string;
}

type ctx = {
  structs : (string, (string * ty) list) Hashtbl.t;
  locals : (Symbol.id, string) Hashtbl.t;
  used_slots : (string, unit) Hashtbl.t;
  globals : (string, unit) Hashtbl.t;
  buf : Buffer.t ref;
  strings : (string * string) list ref;
  tmp : int ref;
  str_ctr : int ref;
  loops : (string * string) list ref;
  terminated : bool ref;
  entry : Buffer.t ref;
  in_main : bool ref;
  inline_funcs : (string, T.cfunc_def) Hashtbl.t;
  inline_stack : inline_frame list ref; (* the pastes we are currently inside *)
}

(* Get fresh temporaries: %t0, %t1, ... *)
let fresh ctx =
  let n = !(ctx.tmp) in
  incr ctx.tmp;
  Printf.sprintf "%%t%d" n

(* Fresh label ID but counter is shared *)
let fresh_id ctx =
  let n = !(ctx.tmp) in
  incr ctx.tmp;
  n

(* A binder named t0 or t1 would collide with the temps. *)
let spelled_like_temp name =
  String.length name > 1
  && name.[0] = 't'
  && String.for_all
       (function '0' .. '9' -> true | _ -> false)
       (String.sub name 1 (String.length name - 1))

(* The slot keeps its source name so the IL stays readable and ids are unique
   anyway so only a second binder with the same name needs a suffix. *)
let bind_local ctx (s : Symbol.t) : string =
  let slot =
    match !(ctx.inline_stack) with
    (* the same binder id lands at every call site so the tag is what keeps their slots apart *)
    | f :: _ -> Printf.sprintf "%s.%s.inl%d" s.name (qbe_id s.id) f.tag
    | [] ->
        if Hashtbl.mem ctx.used_slots s.name || spelled_like_temp s.name then
          Printf.sprintf "%s.%d" s.name s.id
        else s.name
  in
  Hashtbl.replace ctx.used_slots slot ();
  Hashtbl.replace ctx.locals s.id slot;
  slot

let local_slot ctx (id : Symbol.id) = Hashtbl.find_opt ctx.locals id

let sym_addr ctx (s : Symbol.t) : string =
  match s.kind with
  | Global -> "$" ^ s.name
  | _ -> (
      match local_slot ctx s.id with
      | Some slot -> "%" ^ slot
      | None -> "%" ^ s.name)

let emit ctx fmt =
  if !(ctx.terminated) then Printf.ifprintf !(ctx.buf) fmt
  else Printf.bprintf !(ctx.buf) fmt

let emit_entry ctx fmt = Printf.bprintf !(ctx.entry) fmt

(* the data pointer sits at offset 0 in the fat pointer *)
let load_slice_ptr ctx addr =
  let ptr = fresh ctx in
  emit ctx "    %s =l loadl %s\n" ptr addr;
  ptr

(* the length sits at offset 8 in the fat pointer *)
let load_slice_len ctx addr =
  let lenp = fresh ctx in
  emit ctx "    %s =l add %s, 8\n" lenp addr;
  let len = fresh ctx in
  emit ctx "    %s =l loadl %s\n" len lenp;
  len

let load_slice_fields ctx addr =
  let ptr = load_slice_ptr ctx addr in
  let len = load_slice_len ctx addr in
  (ptr, len)

let intern_string ctx s =
  let lbl = Printf.sprintf "$str.%d" !(ctx.str_ctr) in
  incr ctx.str_ctr;
  ctx.strings := (lbl, s) :: !(ctx.strings);
  lbl

let emit_panic ctx msg =
  let lbl = intern_string ctx msg in
  emit ctx "    call $ripe_panic(l %s)\n" lbl;
  emit ctx "    hlt\n";
  ctx.terminated := true

(* start a new basic block and clear the terminated flag *)
let emit_label ctx lbl =
  ctx.terminated := false;
  emit ctx "%s\n" lbl

(* jmp/jnz end a block so anything emitted after until the next label is dropped *)
let emit_jmp ctx lbl =
  if not !(ctx.terminated) then emit ctx "    jmp %s\n" lbl;
  ctx.terminated := true

let emit_jnz ctx v then_lbl else_lbl =
  if not !(ctx.terminated) then
    emit ctx "    jnz %s, %s, %s\n" v then_lbl else_lbl;
  ctx.terminated := true

let field_offset structs fields fname =
  let rec go off = function
    | [] -> Error.ice (Printf.sprintf "unknown field %s" fname)
    | (n, ft) :: rest ->
        let a = ty_align structs ft in
        let off = align_to off a in
        if n = fname then off else go (off + ty_size structs ft) rest
  in
  go 0 fields

(* bytes between consecutive elements (element size rounded up to its alignment) *)
let stride structs elem =
  align_to (ty_size structs elem) (ty_align structs elem)

let offset_addr ctx base off =
  if off = 0 then base
  else
    let a = fresh ctx in
    emit ctx "    %s =l add %s, %d\n" a base off;
    a

let alloc_slot ctx t =
  Printf.sprintf "%s %d" (alloc_instr t) (ty_size ctx.structs t)

let array_elem_ty ~span t =
  match t with
  | TArray (el, _) -> el
  | _ -> Error.ice ~span "expected an array type"

let rec emit_expr (ctx : ctx) (e : T.cexpr) : string =
  let v = emit_expr_desc ctx e in
  (* A never value diverges so it ends the block and can't be consumed *)
  if e.T.ty = TNever && not !(ctx.terminated) then (
    emit ctx "    hlt\n";
    ctx.terminated := true);
  v

and emit_expr_desc (ctx : ctx) (e : T.cexpr) : string =
  let t = e.T.ty in
  match e.T.desc with
  | T.CInt n -> Int64.to_string n
  | T.CFloat f -> float_lit t f
  | T.CBool b -> if b then "1" else "0"
  | T.CNull -> "0"
  | T.CChar c -> string_of_int (Char.code c)
  (* aggregate: the slot itself is the value so yield its address *)
  | T.CIdent s when is_aggregate t -> sym_addr ctx s
  | T.CIdent s ->
      if Symbol.is_func s.kind then "$" ^ s.name
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %s\n" tmp (qbe_ty t) (qbe_load t)
          (sym_addr ctx s);
        tmp
  | T.CCStr s -> intern_string ctx s
  | T.CCall ({ desc = T.CIdent sym; _ }, args, _)
    when Symbol.is_func sym.kind
         && Hashtbl.mem ctx.inline_funcs sym.name
         (* a call to itself would paste forever so the recursive case stays a normal call *)
         && not (List.exists (fun f -> f.name = sym.name) !(ctx.inline_stack))
    ->
      emit_inline_call ctx (Hashtbl.find ctx.inline_funcs sym.name) args
  | T.CCall (_, args, _)
    when List.exists (fun (a : T.cexpr) -> a.T.ty = TNever) args ->
      (* the call is dead once a never argument diverges so only the args up to it get emitted *)
      let rec emit_until = function
        | [] -> ()
        | (a : T.cexpr) :: rest ->
            ignore (emit_expr ctx a);
            if a.T.ty <> TNever then emit_until rest
      in
      emit_until args;
      ""
  | T.CCall (callee, args, fixed_count) ->
      let ret_ty = t in
      let arg_strs =
        List.rev
          (List.rev_map
             (fun (a : T.cexpr) ->
               Printf.sprintf "%s %s" (qbe_ty a.T.ty) (emit_expr ctx a))
             args)
      in
      (* the ... marker between fixed and variadic args tells QBE to set the
         vararg register count on amd64 SysV *)
      let arg_strs =
        match fixed_count with
        | Some n ->
            let fixed = List.filteri (fun i _ -> i < n) arg_strs in
            let rest = List.filteri (fun i _ -> i >= n) arg_strs in
            fixed @ ("..." :: rest)
        | None -> arg_strs
      in
      (* a plain function name calls direct otherwise the callee holds a fn ptr *)
      let callee =
        match callee.T.desc with
        | T.CIdent sym when Symbol.is_func sym.kind -> "$" ^ sym.name
        | _ -> emit_expr ctx callee
      in
      if ret_ty = TVoid || ret_ty = TNever then (
        emit ctx "    call %s(%s)\n" callee (String.concat ", " arg_strs);
        "")
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s call %s(%s)\n" tmp (qbe_ty ret_ty) callee
          (String.concat ", " arg_strs);
        tmp
  | T.CBinOp (Ast.Assign, l, r) -> emit_assign ctx l r t
  | T.CIf (branches, else_body) -> emit_if ctx branches else_body t
  | T.CBinOp (op, l, r) -> emit_binop ctx op l r t
  | T.CUnOp (op, e) -> emit_unop ctx op e t
  | T.CCast (e, checked) ->
      let v = emit_expr ctx e in
      emit_cast ctx ~checked v e.T.ty t
  | T.CSizeOf sz -> string_of_int (ty_size ctx.structs sz)
  | T.CFieldAccess (e, field) ->
      let ft = t in
      let ptr = emit_field_addr ctx e field in
      (* aggregate field: its address is the value like TIndex below *)
      if is_aggregate ft then ptr
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %s\n" tmp (qbe_ty ft) (qbe_load ft) ptr;
        tmp
  | T.CIndex (base, idx) ->
      let elem = t in
      let addr = emit_index_addr ctx base idx elem in
      (* nested array: the element is itself an aggregate so yield its address *)
      if is_aggregate elem then addr
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %s\n" tmp (qbe_ty elem) (qbe_load elem) addr;
        tmp
  | T.CLen e -> (
      match resolve_ty e.T.ty with
      | TArray (_, n) -> string_of_int n
      | TSlice _ ->
          let addr = emit_expr ctx e in
          load_slice_len ctx addr
      | t ->
          Error.ice ~span:e.T.span
            (Printf.sprintf "TLen on non-array type: %s" (show_ty t)))
  | T.CDataPtr e -> (
      match resolve_ty e.T.ty with
      | TSlice _ ->
          let addr = emit_expr ctx e in
          load_slice_ptr ctx addr
      (* an array's base address is already the pointer to its first element *)
      | TArray _ -> emit_expr ctx e
      | t ->
          Error.ice ~span:e.T.span
            (Printf.sprintf "TDataPtr on non-array type: %s" (show_ty t)))
  | T.CToSlice arr ->
      let arr_addr = emit_expr ctx arr in
      let n =
        match resolve_ty arr.T.ty with
        | TArray (_, n) -> n
        | t ->
            Error.ice ~span:arr.T.span
              (Printf.sprintf "cannot coerce to slice: %s" (show_ty t))
      in
      (* build a { ptr = &arr[0], len = n } fat pointer on the stack *)
      let slot = fresh ctx in
      emit_entry ctx "    %s =l alloc8 16\n" slot;
      emit ctx "    storel %s, %s\n" arr_addr slot;
      let lenp = fresh ctx in
      emit ctx "    %s =l add %s, 8\n" lenp slot;
      emit ctx "    storel %d, %s\n" n lenp;
      slot
  | T.CSliceExpr (base, lo, hi) -> emit_slice_expr ctx e.T.span base lo hi t
  | T.CArrayLit elems ->
      (* as a value: materialize into a fresh stack slot and yield its address *)
      let elem = array_elem_ty ~span:e.T.span t in
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      emit_array_lit_into ctx slot elems elem;
      slot
  | T.CZero when is_aggregate t ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      emit_zero_into ctx slot t;
      slot
  | T.CZero -> "0"
  | T.CUndef when is_aggregate t ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      slot
  | T.CUndef -> "0"
  | T.CStructLit (sname, tfields) ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      emit_struct_lit_into ctx slot sname tfields;
      slot
  | T.CBlock body -> emit_block_value ctx body
  | T.CLoop body ->
      emit_loop ctx body;
      ""
  | T.CBinding (_, _, t, e) when t = TNever || e.T.ty = TNever ->
      ignore (emit_expr ctx e);
      ""
  | T.CBinding (_kind, s, t, e) ->
      (* stack slot sized by type (struct sizes resolved from context) *)
      let slot = bind_local ctx s in
      emit_entry ctx "    %%%s =l %s\n" slot (alloc_slot ctx t);
      (match e.T.desc with
      | T.CZero -> emit_zero_into ctx ("%" ^ slot) e.T.ty
      | T.CUndef -> ()
      | T.CArrayLit elems ->
          let elem = array_elem_ty ~span:e.T.span e.T.ty in
          emit_array_lit_into ctx ("%" ^ slot) elems elem
      | T.CStructLit (sname, tfields) ->
          emit_struct_lit_into ctx ("%" ^ slot) sname tfields
      | _ when is_aggregate t ->
          let src = emit_expr ctx e in
          emit_aggregate_copy ctx ("%" ^ slot) src (ty_size ctx.structs t)
      | _ ->
          let v = emit_expr ctx e in
          emit ctx "    %s %s, %%%s\n" (qbe_store t) v slot);
      ""
  (* a return here should leave the pasted body, not the function it was pasted into *)
  | T.CReturn e_opt when !(ctx.inline_stack) <> [] ->
      let frame = List.hd !(ctx.inline_stack) in
      (match (e_opt, frame.res_slot) with
      | Some e, Some slot ->
          let v = emit_expr ctx e in
          if not !(ctx.terminated) then
            emit ctx "    %s %s, %s\n" (qbe_store frame.ret_ty) v slot
      | Some e, None -> ignore (emit_expr ctx e)
      | None, _ -> ());
      emit_jmp ctx frame.end_lbl;
      ""
  | T.CReturn None ->
      (* a bare return in main exits with 0 like falling off the end *)
      if not !(ctx.terminated) then
        if !(ctx.in_main) then emit ctx "    ret 0\n" else emit ctx "    ret\n";
      ctx.terminated := true;
      ""
  | T.CReturn (Some e) ->
      let v = emit_expr ctx e in
      if not !(ctx.terminated) then emit ctx "    ret %s\n" v;
      ctx.terminated := true;
      ""
  | T.CBreak ->
      (match !(ctx.loops) with (_, brk) :: _ -> emit_jmp ctx brk | [] -> ());
      ""
  | T.CContinue ->
      (match !(ctx.loops) with (cont, _) :: _ -> emit_jmp ctx cont | [] -> ());
      ""

and emit_unop ctx op e t =
  match op with
  (* dereferencing a struct pointer just yields its address, same as any other aggregate lvalue *)
  | Ast.Deref when is_aggregate t ->
      let ptr = emit_expr ctx e in
      emit_null_check ctx ptr;
      ptr
  | Ast.Neg | Ast.Not | Ast.BitNot | Ast.Deref -> (
      let ev = emit_expr ctx e in
      let qt = qbe_ty t in
      let tmp = fresh ctx in
      (match op with
      | Ast.Neg -> emit ctx "    %s =%s neg %s\n" tmp qt ev
      | Ast.Not ->
          (* operand is always bool (w) after typechecking *)
          emit ctx "    %s =w ceqw %s, 0\n" tmp ev
      | Ast.BitNot -> emit ctx "    %s =%s xor %s, -1\n" tmp qt ev
      | Ast.Deref ->
          emit_null_check ctx ev;
          emit ctx "    %s =%s %s %s\n" tmp qt (qbe_load t) ev
      | _ -> Error.ice ~span:e.T.span "unexpected unary operator");
      match op with Ast.Neg | Ast.BitNot -> narrow_int_to ctx tmp t | _ -> tmp)
  | Ast.AddressOf ->
      let addr = emit_lvalue_addr ctx e in
      let tmp = fresh ctx in
      emit ctx "    %s =l copy %s\n" tmp addr;
      tmp

(* pointer math is 64-bit so widen a word-sized value to a long *)
and widen_to_l ctx v ty =
  if qbe_base ty = L then v
  else
    let t = fresh ctx in
    let ins = if is_unsigned ty then "extuw" else "extsw" in
    emit ctx "    %s =l %s %s\n" t ins v;
    t

(* pointer to the first element: an array's own address, or a slice's ptr field *)
and data_ptr ctx base =
  let addr = emit_expr ctx base in
  match resolve_ty base.T.ty with
  | TSlice _ ->
      let p = fresh ctx in
      emit ctx "    %s =l loadl %s\n" p addr;
      p
  | _ -> addr

(* address of arr[idx]: storage + idx * stride(elem) *)
and emit_slice_expr ctx (span : Ast.span) base lo hi t =
  let elem =
    match t with
    | TSlice e -> e
    | _ -> Error.ice ~span "slice expression on non-slice type"
  in
  let base_addr = emit_expr ctx base in
  let storage, blen =
    match resolve_ty base.T.ty with
    | TArray (_, n) -> (base_addr, string_of_int n)
    | TSlice _ -> load_slice_fields ctx base_addr
    | t ->
        Error.ice ~span:base.T.span
          (Printf.sprintf "cannot slice: %s" (show_ty t))
  in
  let lo_l = widen_to_l ctx (emit_expr ctx lo) lo.T.ty in
  let hi_l = widen_to_l ctx (emit_expr ctx hi) hi.T.ty in
  emit_slice_bounds_check ctx lo_l hi_l blen;
  let off = fresh ctx in
  emit ctx "    %s =l mul %s, %d\n" off lo_l (stride ctx.structs elem);
  let ptr = fresh ctx in
  emit ctx "    %s =l add %s, %s\n" ptr storage off;
  let len = fresh ctx in
  emit ctx "    %s =l sub %s, %s\n" len hi_l lo_l;
  let slot = fresh ctx in
  emit_entry ctx "    %s =l alloc8 16\n" slot;
  emit ctx "    storel %s, %s\n" ptr slot;
  let lenp = fresh ctx in
  emit ctx "    %s =l add %s, 8\n" lenp slot;
  emit ctx "    storel %s, %s\n" len lenp;
  slot

and emit_index_addr ctx base idx elem =
  let base_addr = emit_expr ctx base in
  let storage, len =
    match resolve_ty base.T.ty with
    | TArray (_, n) -> (base_addr, Some (string_of_int n))
    | TSlice _ ->
        let p, l = load_slice_fields ctx base_addr in
        (p, Some l)
    | _ -> (base_addr, None)
  in
  let iv = emit_expr ctx idx in
  let iw = widen_to_l ctx iv idx.T.ty in
  (match len with Some len -> emit_bounds_check ctx iw len | None -> ());
  let off = fresh ctx in
  emit ctx "    %s =l mul %s, %d\n" off iw (stride ctx.structs elem);
  let addr = fresh ctx in
  emit ctx "    %s =l add %s, %s\n" addr storage off;
  addr

(* every runtime check jumps to a panic when cond is nonzero and otherwise falls into the ok block *)
and emit_guard ctx ~tag ~cond ~panic =
  let id = fresh_id ctx in
  let fail_lbl = Printf.sprintf "@%s.fail.%d" tag id in
  let ok_lbl = Printf.sprintf "@%s.ok.%d" tag id in
  emit_jnz ctx cond fail_lbl ok_lbl;
  emit_label ctx fail_lbl;
  panic ();
  emit ctx "    hlt\n";
  ctx.terminated := true;
  emit_label ctx ok_lbl

and emit_bounds_check ctx idx len =
  let cond = fresh ctx in
  emit ctx "    %s =w cugel %s, %s\n" cond idx len;
  emit_guard ctx ~tag:"bounds" ~cond ~panic:(fun () ->
      emit ctx "    call $ripe_panic_bounds(l %s, l %s)\n" idx len)

and emit_slice_bounds_check ctx lo hi len =
  let hi_bad = fresh ctx in
  emit ctx "    %s =w cugtl %s, %s\n" hi_bad hi len;
  let lo_bad = fresh ctx in
  emit ctx "    %s =w cugtl %s, %s\n" lo_bad lo hi;
  let bad = fresh ctx in
  emit ctx "    %s =w or %s, %s\n" bad hi_bad lo_bad;
  emit_guard ctx ~tag:"slice" ~cond:bad ~panic:(fun () ->
      emit ctx "    call $ripe_panic_slice_bounds(l %s, l %s, l %s)\n" lo hi len)

and emit_divzero_check ctx divisor op_qt =
  let zero = fresh ctx in
  emit ctx "    %s =w ceq%s %s, 0\n" zero op_qt divisor;
  emit_guard ctx ~tag:"divzero" ~cond:zero ~panic:(fun () ->
      emit ctx "    call $ripe_panic_divzero()\n")

(* INT_MIN / -1 wraps to INT_MIN and INT_MIN % -1 is 0 so dodge the hardware divide when the divisor is -1 *)
and emit_div_overflow_guard ctx ~instr ~qt ~op_qt ~dest lv rv =
  let id = fresh_id ctx in
  let neg1_lbl = Printf.sprintf "@div.neg1.%d" id in
  let norm_lbl = Printf.sprintf "@div.norm.%d" id in
  let join_lbl = Printf.sprintf "@div.join.%d" id in
  let is_neg1 = fresh ctx in
  emit ctx "    %s =w ceq%s %s, -1\n" is_neg1 op_qt rv;
  emit_jnz ctx is_neg1 neg1_lbl norm_lbl;
  emit_label ctx neg1_lbl;
  let neg_res = fresh ctx in
  if instr = "rem" then emit ctx "    %s =%s copy 0\n" neg_res qt
  else emit ctx "    %s =%s sub 0, %s\n" neg_res qt lv;
  emit_jmp ctx join_lbl;
  emit_label ctx norm_lbl;
  let norm_res = fresh ctx in
  emit ctx "    %s =%s %s %s, %s\n" norm_res qt instr lv rv;
  emit_jmp ctx join_lbl;
  emit_label ctx join_lbl;
  emit ctx "    %s =%s phi %s %s, %s %s\n" dest qt neg1_lbl neg_res norm_lbl
    norm_res

and emit_null_check ctx ptr =
  let isnull = fresh ctx in
  emit ctx "    %s =w ceql %s, 0\n" isnull ptr;
  emit_guard ctx ~tag:"null" ~cond:isnull ~panic:(fun () ->
      emit ctx "    call $ripe_panic_null()\n")

and emit_negshift_check ctx count count_qt =
  let neg = fresh ctx in
  emit ctx "    %s =w cslt%s %s, 0\n" neg count_qt count;
  emit_guard ctx ~tag:"negshift" ~cond:neg ~panic:(fun () ->
      emit ctx "    call $ripe_panic_shift()\n")

(* address of any lvalue, for &e: a name's own slot/global, or the same address computation assign already uses for index/field/deref *)
and emit_lvalue_addr ctx (e : T.cexpr) =
  match e.T.desc with
  (* &funcname is already the function pointer. *)
  | T.CIdent s when Symbol.is_func s.kind -> "$" ^ s.name
  | T.CIdent s -> sym_addr ctx s
  | T.CIndex (base, idx) -> emit_index_addr ctx base idx e.T.ty
  | T.CFieldAccess (base, field) -> emit_field_addr ctx base field
  | T.CUnOp (Ast.Deref, inner) -> emit_expr ctx inner
  | _ -> Error.ice ~span:e.T.span "expected an lvalue"

(* address of e.field: base address (or loaded pointer, if base is a pointer) + field offset *)
and emit_field_addr ctx base field =
  let addr = emit_expr ctx base in
  (match resolve_ty base.T.ty with
  | TPointer _ -> emit_null_check ctx addr
  | _ -> ());
  let rec peel = function
    | TStruct (n, _) -> n
    | TAlias (_, inner) -> peel inner
    | TPointer t -> peel t
    | _ -> Error.ice ~span:base.T.span "field access on non-struct type"
  in
  let struct_name = peel base.T.ty in
  let fields = Hashtbl.find ctx.structs struct_name in
  let offset = field_offset ctx.structs fields field in
  offset_addr ctx addr offset

(* write a zero value of type t into the slot at dest *)
and emit_zero_into ctx dest t =
  match resolve_ty t with
  | TArray _ | TSlice _ | TStruct _ ->
      let size = ty_size ctx.structs t in
      if size > bulk_mem_threshold then
        emit ctx "    call $memset(l %s, w 0, l %d)\n" dest size
      else begin
        let off = ref 0 in
        let step w store =
          while !off + w <= size do
            emit ctx "    %s 0, %s\n" store (offset_addr ctx dest !off);
            off := !off + w
          done
        in
        step 8 "storel";
        step 4 "storew";
        step 2 "storeh";
        step 1 "storeb"
      end
  | _ -> emit ctx "    %s 0, %s\n" (qbe_store t) dest

(* copy size bytes from src to dest for by-value aggregate (slice) moves *)
and emit_aggregate_copy ctx dest src size =
  if size > bulk_mem_threshold then
    emit ctx "    call $memcpy(l %s, l %s, l %d)\n" dest src size
  else emit ctx "    blit %s, %s, %d\n" src dest size

(* store each element of a literal into an already-allocated array at base *)
and emit_array_lit_into ctx base elems elem =
  let strd = stride ctx.structs elem in
  List.iteri
    (fun i el ->
      let addr = offset_addr ctx base (i * strd) in
      match el.T.desc with
      (* nested literal (multi-dimensional array): recurse into the sub-array *)
      | T.CArrayLit sub ->
          let subelem = array_elem_ty ~span:el.T.span el.T.ty in
          emit_array_lit_into ctx addr sub subelem
      | _ when is_aggregate elem ->
          (* element is an aggregate value: copy its bytes into place *)
          let src = emit_expr ctx el in
          emit_aggregate_copy ctx addr src (ty_size ctx.structs elem)
      | _ ->
          let v = emit_expr ctx el in
          emit ctx "    %s %s, %s\n" (qbe_store elem) v addr)
    elems

and emit_struct_lit_into ctx base sname tfields =
  let fields = Hashtbl.find ctx.structs sname in
  List.iter
    (fun (fname, (fe : T.cexpr)) ->
      let ft = fe.T.ty in
      let offset = field_offset ctx.structs fields fname in
      let addr = offset_addr ctx base offset in
      match fe.T.desc with
      | T.CStructLit (sub, subfields) ->
          emit_struct_lit_into ctx addr sub subfields
      | T.CArrayLit sub ->
          let subelem = array_elem_ty ~span:fe.T.span ft in
          emit_array_lit_into ctx addr sub subelem
      | T.CZero -> emit_zero_into ctx addr ft
      | T.CUndef -> ()
      | _ when is_aggregate ft ->
          let src = emit_expr ctx fe in
          emit_aggregate_copy ctx addr src (ty_size ctx.structs ft)
      | _ ->
          let v = emit_expr ctx fe in
          emit ctx "    %s %s, %s\n" (qbe_store ft) v addr)
    tfields

(* split from emit_binop so the lhs is not re-evaluated into dead loads *)
and emit_assign ctx l r _t =
  match l.T.desc with
  | T.CIndex (base, idx) ->
      let elem = l.T.ty in
      let addr = emit_index_addr ctx base idx elem in
      emit_store_into ctx elem addr r
  | T.CIdent s -> emit_store_into ctx l.T.ty (sym_addr ctx s) r
  | T.CFieldAccess (base, field) ->
      let addr = emit_field_addr ctx base field in
      emit_store_into ctx l.T.ty addr r
  | T.CUnOp (Ast.Deref, inner) ->
      let addr = emit_expr ctx inner in
      emit_null_check ctx addr;
      emit_store_into ctx l.T.ty addr r
  | _ -> emit_expr ctx r

(* The caller already worked out the address so every lvalue shape can share
   this store. *)
and emit_store_into ctx ty addr r =
  if is_aggregate ty then begin
    (match (r.T.desc, ty) with
    | T.CArrayLit elems, TArray (elem, _) ->
        emit_array_lit_into ctx addr elems elem
    | T.CStructLit (sname, tfields), _ ->
        emit_struct_lit_into ctx addr sname tfields
    | _ ->
        let src = emit_expr ctx r in
        emit_aggregate_copy ctx addr src (ty_size ctx.structs ty));
    addr
  end
  else
    let rv = emit_expr ctx r in
    emit ctx "    %s %s, %s\n" (qbe_store ty) rv addr;
    rv

(* a condition that's itself a branch just jumps to its arms directly *)
and emit_branch ctx e true_lbl false_lbl =
  match e.T.desc with
  | T.CBool b -> emit_jmp ctx (if b then true_lbl else false_lbl)
  (* a short-circuit and/or lowers to a single-arm if so branch through it directly *)
  | T.CIf ([ (c, then_body) ], Some else_body) ->
      let id = fresh_id ctx in
      let then_lbl = Printf.sprintf "@sel.then%d" id in
      let else_lbl = Printf.sprintf "@sel.else%d" id in
      emit_branch ctx c then_lbl else_lbl;
      emit_label ctx then_lbl;
      emit_branch_block ctx then_body true_lbl false_lbl;
      emit_label ctx else_lbl;
      emit_branch_block ctx else_body true_lbl false_lbl
  | T.CUnOp (Ast.Not, inner) -> emit_branch ctx inner false_lbl true_lbl
  | _ ->
      let v = emit_expr ctx e in
      emit_jnz ctx v true_lbl false_lbl

and emit_branch_block ctx (body : T.cblock) true_lbl false_lbl =
  match List.rev body with
  | last :: rev_init ->
      List.iter (fun e -> ignore (emit_expr ctx e)) (List.rev rev_init);
      emit_branch ctx last true_lbl false_lbl
  | [] -> emit_jmp ctx true_lbl

(* only the taken arm runs and a value comes back unless the if is void *)
and emit_if ctx (branches : (T.cexpr * T.cblock) list)
    (else_body : T.cblock option) ty =
  let id = fresh_id ctx in
  let n = List.length branches in
  let cond_lbls = List.init n (fun i -> Printf.sprintf "@if.cond%d_%d" id i) in
  let then_lbls = List.init n (fun i -> Printf.sprintf "@if.then%d_%d" id i) in
  let else_lbl = Printf.sprintf "@if.else%d" id in
  let end_lbl = Printf.sprintf "@if.end%d" id in
  let never = ty = TNever in
  let is_value = (not never) && ty <> TVoid in
  let res = if is_value then fresh ctx else "" in
  let qt = if is_value then qbe_ty ty else "" in
  let emit_arm lbl body =
    emit_label ctx lbl;
    if is_value then (
      let v = emit_block_value ctx body in
      (* a never arm already terminated so only a live arm copies out and joins *)
      if not !(ctx.terminated) then (
        emit ctx "    %s =%s copy %s\n" res qt v;
        emit_jmp ctx end_lbl))
    else (
      emit_block ctx body;
      emit_jmp ctx end_lbl)
  in
  (match branches with
  | [] -> ( match else_body with Some b -> emit_block ctx b | None -> ())
  | _ -> (
      List.iteri
        (fun i (cond, body) ->
          let next_lbl =
            if i + 1 < n then List.nth cond_lbls (i + 1) else else_lbl
          in
          emit_label ctx (List.nth cond_lbls i);
          emit_branch ctx cond (List.nth then_lbls i) next_lbl;
          emit_arm (List.nth then_lbls i) body)
        branches;
      match else_body with
      | Some b -> emit_arm else_lbl b
      | None ->
          emit_label ctx else_lbl;
          emit_jmp ctx end_lbl));
  emit_label ctx end_lbl;
  (* every arm diverged so the join is unreachable but still needs a terminator *)
  if never then (
    emit ctx "    hlt\n";
    ctx.terminated := true);
  res

(* TODO(2cc1): the local arithmetic always emits instructions instead of
      folding through fold_const_num *)
and emit_binop ctx op l r t =
  let lv = emit_expr ctx l in
  let rv = emit_expr ctx r in
  let lty = l.T.ty in
  match op with
  | Ast.Lshift | Ast.Rshift ->
      let const_count = match r.T.desc with T.CInt n -> Some n | _ -> None in
      emit_shift ctx op ?const_count ~ty:t ~count_ty:r.T.ty
        ~unsigned:(is_unsigned lty) lv rv
  | _ ->
      emit_arith_binop ctx op ~result_ty:t ~operand_ty:lty ~span:l.T.span lv rv

and emit_arith_binop ctx op ~result_ty:t ~operand_ty:lty ~span lv rv =
  let qt = qbe_ty t in
  let op_qt = qbe_ty lty in
  let sign = signedness lty in
  let unsigned = is_unsigned lty in

  let tmp = fresh ctx in
  (match op with
  | Ast.Add -> emit ctx "    %s =%s add %s, %s\n" tmp qt lv rv
  | Ast.Sub -> emit ctx "    %s =%s sub %s, %s\n" tmp qt lv rv
  | Ast.Mul -> emit ctx "    %s =%s mul %s, %s\n" tmp qt lv rv
  | Ast.Div ->
      if is_float lty then emit ctx "    %s =%s div %s, %s\n" tmp qt lv rv
      else begin
        emit_divzero_check ctx rv op_qt;
        if unsigned then emit ctx "    %s =%s udiv %s, %s\n" tmp qt lv rv
        else if div_overflows_at_reg_width lty then
          emit_div_overflow_guard ctx ~instr:"div" ~qt ~op_qt ~dest:tmp lv rv
        else emit ctx "    %s =%s div %s, %s\n" tmp qt lv rv
      end
  | Ast.Mod ->
      emit_divzero_check ctx rv op_qt;
      if unsigned then emit ctx "    %s =%s urem %s, %s\n" tmp qt lv rv
      else if div_overflows_at_reg_width lty then
        emit_div_overflow_guard ctx ~instr:"rem" ~qt ~op_qt ~dest:tmp lv rv
      else emit ctx "    %s =%s rem %s, %s\n" tmp qt lv rv
  (* floats: ceqs, ceqd / ints: ceqw, ceql *)
  | Ast.Eq -> emit ctx "    %s =w ceq%s %s, %s\n" tmp op_qt lv rv
  | Ast.Neq -> emit ctx "    %s =w cne%s %s, %s\n" tmp op_qt lv rv
  (* floats: clts, cltd (no sign prefix) / ints: csltw, csltl, cultw, etc *)
  | Ast.Lt ->
      if is_float lty then emit ctx "    %s =w clt%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%slt%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Gt ->
      if is_float lty then emit ctx "    %s =w cgt%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%sgt%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Lte ->
      if is_float lty then emit ctx "    %s =w cle%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%sle%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Gte ->
      if is_float lty then emit ctx "    %s =w cge%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%sge%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.BitAnd -> emit ctx "    %s =%s and %s, %s\n" tmp qt lv rv
  | Ast.BitOr -> emit ctx "    %s =%s or %s, %s\n" tmp qt lv rv
  | Ast.BitXor -> emit ctx "    %s =%s xor %s, %s\n" tmp qt lv rv
  | _ -> Error.ice ~span "unexpected binary operator");
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Div -> narrow_int_to ctx tmp t
  | _ -> tmp

(* this rebuilds the go result where an oversized count clears every bit since qbe only masks the count like x86 *)
and emit_shift ctx op ?const_count ~ty ~count_ty ~unsigned lv rv =
  let qt = qbe_ty ty in
  let bits = match qbe_base ty with L -> 64 | _ -> 32 in
  match (op, const_count) with
  | Ast.Rshift, Some n when (not unsigned) && Int64.compare n 0L >= 0 ->
      let in_range = Int64.compare n (Int64.of_int bits) < 0 in
      let count = if in_range then n else Int64.of_int (bits - 1) in
      let res = fresh ctx in
      emit ctx "    %s =%s sar %s, %Ld\n" res qt lv count;
      res
  | _ -> (
      let known_nonneg =
        match const_count with
        | Some n -> Int64.compare n 0L >= 0
        | None -> is_unsigned count_ty
      in
      if not known_nonneg then emit_negshift_check ctx rv (qbe_ty count_ty);
      (* the range check runs in the count width so a huge count is caught before the truncation *)
      let in_range = fresh ctx in
      emit ctx "    %s =w cult%s %s, %d\n" in_range (qbe_ty count_ty) rv bits;
      let count = word_count ctx count_ty rv in
      match op with
      | Ast.Rshift when not unsigned ->
          (* an out of range count is forced to all ones so sar keeps filling with the sign bit *)
          let spill = fresh ctx in
          emit ctx "    %s =w sub %s, 1\n" spill in_range;
          let capped = fresh ctx in
          emit ctx "    %s =w or %s, %s\n" capped count spill;
          let res = fresh ctx in
          emit ctx "    %s =%s sar %s, %s\n" res qt lv capped;
          res
      | Ast.Lshift | Ast.Rshift ->
          let is_lshift = op = Ast.Lshift in
          let instr = if is_lshift then "shl" else "shr" in
          let raw = fresh ctx in
          emit ctx "    %s =%s %s %s, %s\n" raw qt instr lv count;
          (* an out of range count makes the mask all zeros so the result drops to 0 *)
          let res = fresh ctx in
          emit ctx "    %s =%s and %s, %s\n" res qt raw
            (shift_mask ctx ty in_range);
          if is_lshift then narrow_int_to ctx res ty else res
      | _ -> Error.ice "emit_shift on non-shift op")

(* the shift wants a word count so a long one drops to its low word *)
and word_count ctx count_ty rv =
  match qbe_base count_ty with
  | L ->
      let w = fresh ctx in
      emit ctx "    %s =w copy %s\n" w rv;
      w
  | _ -> rv

(* the in range flag spreads across the whole type so 1 becomes all ones and 0 becomes all zeros *)
and shift_mask ctx ty flag =
  let neg = fresh ctx in
  emit ctx "    %s =w sub 0, %s\n" neg flag;
  match qbe_base ty with
  | L ->
      let wide = fresh ctx in
      emit ctx "    %s =l extsw %s\n" wide neg;
      wide
  | _ -> neg

(* the extend that masks a narrow
   integer target back down to its width *)
and narrow_int_instr t =
  match resolve_ty t with
  | TInt I8 -> Some "extsb"
  | TInt U8 -> Some "extub"
  | TInt I16 -> Some "extsh"
  | TInt U16 -> Some "extuh"
  | _ -> None

(* a narrow int only fills the low bits of its w register
   so a cast has to mask it back down *)
and narrow_int_to ctx v target_ty =
  match narrow_int_instr target_ty with
  | None -> v
  | Some instr ->
      let tmp = fresh ctx in
      emit ctx "    %s =w %s %s\n" tmp instr v;
      tmp

(* TODO: `as` silently loses data like C. Add a safe cast that catches bad conversions at runtime. *)
and emit_checked_cast_guard ctx v src_ty target_ty =
  let src_k = int_kind_of src_ty in
  let tgt_k = int_kind_of target_ty in
  if cast_int_needs_check src_k tgt_k then begin
    (* the value moves up to 64 bits so both bounds fit and the compare sees its true value *)
    let v64 =
      if qbe_base src_ty = W then begin
        let t = fresh ctx in
        let ext = if is_unsigned src_ty then "extuw" else "extsw" in
        emit ctx "    %s =l %s %s\n" t ext v;
        t
      end
      else v
    in
    let target_is_u64 = match tgt_k with U64 | Usize -> true | _ -> false in
    (* an unsigned source is never below zero so it can only overflow the top *)
    let underflow =
      if is_unsigned src_ty then None
      else begin
        let t = fresh ctx in
        emit ctx "    %s =w csltl %s, %Ld\n" t v64
          (Int64.neg (int_kind_neg_limit tgt_k));
        Some t
      end
    in
    (* nothing at 64 bits overflows a u64 so that top bound never trips *)
    let overflow =
      if target_is_u64 then None
      else begin
        let cmp = if is_unsigned src_ty then "cugtl" else "csgtl" in
        let t = fresh ctx in
        emit ctx "    %s =w %s %s, %Ld\n" t cmp v64 (int_kind_pos_limit tgt_k);
        Some t
      end
    in
    let bad =
      match (underflow, overflow) with
      | Some a, Some b ->
          let t = fresh ctx in
          emit ctx "    %s =w or %s, %s\n" t a b;
          t
      | Some x, None | None, Some x -> x
      | None, None -> Error.ice "checked cast with no possible loss"
    in
    emit_guard ctx ~tag:"cast" ~cond:bad ~panic:(fun () ->
        emit ctx "    call $ripe_panic_cast()\n")
  end

and emit_cast ctx ?(checked = false) v src_ty target_ty =
  if checked then emit_checked_cast_guard ctx v src_ty target_ty;
  let tmp = fresh ctx in
  let tgt = qbe_ty target_ty in
  (* the extend already truncates so the copy would be redundant *)
  match (narrow_int_instr target_ty, qbe_base src_ty) with
  | Some instr, (W | L) ->
      emit ctx "    %s =w %s %s\n" tmp instr v;
      tmp
  | _ ->
      (match (qbe_base src_ty, qbe_base target_ty) with
      (* the same base type is a plain bit copy *)
      | W, W | L, L | S, S | D, D -> emit ctx "    %s =%s copy %s\n" tmp tgt v
      (* word widens to long, long truncates to word *)
      | W, L ->
          let instr = if is_unsigned src_ty then "extuw" else "extsw" in
          emit ctx "    %s =l %s %s\n" tmp instr v
      | L, W -> emit ctx "    %s =w copy %s\n" tmp v
      (* single and double swap precision *)
      | S, D -> emit ctx "    %s =d exts %s\n" tmp v
      | D, S -> emit ctx "    %s =s truncd %s\n" tmp v
      (* an integer turns into a float *)
      | (W | L), (S | D) ->
          let instr =
            match (qbe_base src_ty, is_unsigned src_ty) with
            | W, true -> "uwtof"
            | W, false -> "swtof"
            | _, true -> "ultof"
            | _, false -> "sltof"
          in
          emit ctx "    %s =%s %s %s\n" tmp tgt instr v
      (* a float turns into an integer *)
      | (S | D), (W | L) ->
          let instr =
            match (qbe_base src_ty, is_unsigned target_ty) with
            | S, true -> "stoui"
            | S, false -> "stosi"
            | _, true -> "dtoui"
            | _, false -> "dtosi"
          in
          emit ctx "    %s =%s %s %s\n" tmp tgt instr v);
      narrow_int_to ctx tmp target_ty

and emit_loop ctx body =
  let id = fresh_id ctx in
  let body_lbl = Printf.sprintf "@loop.body%d" id in
  let end_lbl = Printf.sprintf "@loop.end%d" id in
  emit_label ctx body_lbl;
  ctx.loops := (body_lbl, end_lbl) :: !(ctx.loops);
  emit_block ctx body;
  ctx.loops := List.tl !(ctx.loops);
  emit_jmp ctx body_lbl;
  emit_label ctx end_lbl

(* a block in statement position runs each element only for its effect *)
and emit_block ctx (body : T.cblock) : unit =
  List.iter (fun e -> ignore (emit_expr ctx e)) body

(* a block in value position yields its last element and is void when empty *)
and emit_block_value ctx (body : T.cblock) : string =
  let rec go = function
    | [] -> ""
    | [ last ] -> emit_expr ctx last
    | e :: rest ->
        ignore (emit_expr ctx e);
        go rest
  in
  go body

(* the callee body takes the place of the call and its returns write into a slot the join block reads back *)
and emit_inline_call ctx (tfd : T.cfunc_def) (args : T.cexpr list) : string =
  let ret_ty = tfd.ret_ty in
  let id = fresh_id ctx in
  let end_lbl = Printf.sprintf "@inline.end%d" id in
  (* the arguments still belong to the caller so they get read before any param slot exists *)
  let arg_vals =
    List.map (fun (a : T.cexpr) -> (emit_expr ctx a, a.T.ty)) args
  in
  let res_slot =
    if ret_ty = TVoid || ret_ty = TNever then None
    else
      let slot = Printf.sprintf "%%inl.res%d" id in
      let sz = if is_aggregate ret_ty then 8 else ty_size ctx.structs ret_ty in
      emit_entry ctx "    %s =l alloc8 %d\n" slot sz;
      Some slot
  in
  ctx.inline_stack :=
    { name = tfd.name; tag = id; res_slot; ret_ty; end_lbl }
    :: !(ctx.inline_stack);
  (* each argument gets copied into its param slot the way a real prologue would *)
  List.iter2
    (fun (ps, pt) (v, _) ->
      let slot = bind_local ctx ps in
      emit ctx "    %%%s =l %s\n" slot (alloc_slot ctx pt);
      if is_aggregate pt then
        emit_aggregate_copy ctx ("%" ^ slot) v (ty_size ctx.structs pt)
      else emit ctx "    %s %s, %%%s\n" (qbe_store pt) v slot)
    tfd.params arg_vals;
  emit_block ctx tfd.body;
  (* a body that just runs off the end still has to land on the join *)
  emit_jmp ctx end_lbl;
  emit_label ctx end_lbl;
  ctx.inline_stack := List.tl !(ctx.inline_stack);
  match res_slot with
  | None -> ""
  | Some slot ->
      let r = fresh ctx in
      emit ctx "    %s =%s %s %s\n" r (qbe_ty ret_ty) (qbe_load ret_ty) slot;
      r

let emit_func (ctx : ctx) (tfd : T.cfunc_def) =
  (* temporaries and locals are function scoped *)
  ctx.tmp := 0;
  Hashtbl.clear ctx.locals;
  Hashtbl.clear ctx.used_slots;

  (* Use temporary names for params to spill them to stack slots *)
  let param_tmps =
    List.map
      (fun (s, t) ->
        let tmp = fresh ctx in
        (s, t, tmp))
      tfd.params
  in
  let params_strs =
    List.map
      (fun (_, t, tmp) -> Printf.sprintf "%s %s" (qbe_ty t) tmp)
      param_tmps
  in

  (* TODO(6e33): Create a custom _start. *)
  let is_main = tfd.name = "main" && tfd.ret_ty = TInt I32 in
  ctx.in_main := is_main;
  let export_part = if is_main then "export " else "" in
  let ret_part =
    match tfd.ret_ty with TVoid | TNever -> "" | t -> qbe_ty t ^ " "
  in
  (* the previous function ended terminated so clear it before this header *)
  ctx.terminated := false;
  (* TODO(572b): export pub functions *)
  emit ctx "%sfunction %s$%s(%s) {\n" export_part ret_part tfd.name
    (String.concat ", " params_strs);
  emit_label ctx "@start";

  (* divert the body so hoisted @start allocs land ahead of it *)
  let out = !(ctx.buf) in
  let body = Buffer.create 256 in
  ctx.buf := body;
  ctx.entry := Buffer.create 64;

  (* Spill params to stack slots so they can be reassigned *)
  List.iter
    (fun (s, t, tmp) ->
      let slot = bind_local ctx s in
      emit ctx "    %%%s =l %s\n" slot (alloc_slot ctx t);
      (* aggregates arrive as a pointer so copy the value into the local slot *)
      if is_aggregate t then
        emit_aggregate_copy ctx ("%" ^ slot) tmp (ty_size ctx.structs t)
      else emit ctx "    %s %s, %%%s\n" (qbe_store t) tmp slot)
    param_tmps;

  emit_block ctx tfd.body;

  ctx.buf := out;
  Buffer.add_buffer out !(ctx.entry);
  Buffer.add_buffer out body;

  (* close the final block if control can still fall off the end *)
  if not !(ctx.terminated) then
    if is_main then emit ctx "    ret 0\n"
    else if tfd.ret_ty = TVoid then emit ctx "    ret\n"
      (* unreachable but QBE needs every block terminated *)
    else emit ctx "    hlt\n";
  (* TODO(aa3a): error in typechecker for non-void functions missing a return on all paths *)
  ctx.terminated := false;
  emit ctx "}\n\n"

let rec qbe_ext_ty (t : ty) : string =
  match resolve_ty t with
  | TInt (I8 | U8) | TBool -> "b"
  | TInt (I16 | U16) -> "h"
  | TInt (I32 | U32) -> "w"
  (* null is a pointer no type but all pointers are 64-bit *)
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ ->
      "l"
  | TFloat F32 -> "s"
  | TFloat F64 -> "d"
  | TStruct (sn, _) -> ":" ^ sn
  (* QBE repeats a field type n times: { w 3 } is three words *)
  | TArray (e, n) ->
      (* a QBE field cannot nest counts, so every dimension collapses into one *)
      let rec flatten t reps =
        match resolve_ty t with
        | TArray (e, n) -> flatten e (reps * n)
        | TSlice _ -> ("l", reps * 2)
        | base -> (qbe_ext_ty base, reps)
      in
      let unit_ty, reps = flatten e n in
      Printf.sprintf "%s %d" unit_ty reps
  (* fat pointer stored inline as two longs *)
  | TSlice _ -> "l 2"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no extended type"
  | TNever -> Error.ice "TNever has no extended type"
  | TError -> Error.ice "TError has no extended type"

let emit_struct_type (ctx : ctx) (name : string) (fields : (string * ty) list) =
  let field_strs = List.map (fun (_, t) -> qbe_ext_ty t) fields in
  emit ctx "type :%s = { %s }\n" name (String.concat ", " field_strs)

(* the typechecker already folded everything so this only formats values *)
let fold_const_value (ctx : ctx) (te : T.cexpr) : string =
  match te.T.desc with
  | T.CInt n -> Int64.to_string n
  | T.CBool b -> if b then "1" else "0"
  | T.CNull -> "0"
  | T.CFloat f -> format_const_num te.T.ty (Nf f)
  | T.CIdent s when Symbol.is_func s.kind -> "$" ^ s.name
  | T.CCStr s -> intern_string ctx s
  | T.CUnOp (Ast.AddressOf, { desc = T.CIdent s; _ })
    when Symbol.is_global s.kind ->
      "$" ^ s.name
  | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])

(* QBE data fields for a constant array literal, e.g. "w 1, w 2, w 3" *)
let rec const_array_fields (ctx : ctx) (te : T.cexpr) : string =
  match te.T.desc with
  | T.CArrayLit elems ->
      String.concat ", " (List.map (const_array_fields ctx) elems)
  | T.CStructLit (_, tfields) ->
      let off = ref 0 in
      let parts = ref [] in
      List.iter
        (fun (_, (fe : T.cexpr)) ->
          let ft = fe.T.ty in
          let aligned = align_to !off (ty_align ctx.structs ft) in
          if aligned > !off then
            parts := Printf.sprintf "z %d" (aligned - !off) :: !parts;
          (match fe.T.desc with
          | T.CZero ->
              parts := Printf.sprintf "z %d" (ty_size ctx.structs ft) :: !parts
          | _ -> parts := const_array_fields ctx fe :: !parts);
          off := aligned + ty_size ctx.structs ft)
        tfields;
      let total = ty_size ctx.structs te.T.ty in
      if total > !off then
        parts := Printf.sprintf "z %d" (total - !off) :: !parts;
      String.concat ", " (List.rev !parts)
  | _ -> Printf.sprintf "%s %s" (qbe_ext_ty te.T.ty) (fold_const_value ctx te)

let emit_global_data (ctx : ctx) (gd : T.cglobal_def) =
  let align = ty_align ctx.structs gd.ty in
  match gd.init with
  | None ->
      let size = ty_size ctx.structs gd.ty in
      emit ctx "data $%s = align %d { z %d }\n" gd.name align size
  | Some te -> (
      match resolve_ty gd.ty with
      | TArray _ | TStruct _ ->
          emit ctx "data $%s = align %d { %s }\n" gd.name align
            (const_array_fields ctx te)
      | _ ->
          let letter = qbe_ext_ty gd.ty in
          let value = fold_const_value ctx te in
          emit ctx "data $%s = align %d { %s %s }\n" gd.name align letter value)

let emit_string_data (ctx : ctx) (lbl : string) (content : string) =
  let buf = Buffer.create (String.length content) in
  String.iter
    (function
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    content;
  emit ctx "data %s = { b \"%s\", b 0 }\n" lbl (Buffer.contents buf)

let emit_qbe (tdecls : T.cdecl list) : string =
  (* Collect struct layouts for offset comp *)
  let structs = Hashtbl.create 8 in
  List.iter
    (function
      | T.CStruct (name, fields, _) -> Hashtbl.replace structs name fields
      | _ -> ())
    tdecls;

  let ctx =
    {
      structs;
      locals = Hashtbl.create 16;
      used_slots = Hashtbl.create 16;
      globals = Hashtbl.create 16;
      buf = ref (Buffer.create 1024);
      strings = ref [];
      tmp = ref 0;
      str_ctr = ref 0;
      loops = ref [];
      terminated = ref false;
      entry = ref (Buffer.create 64);
      in_main = ref false;
      inline_funcs = Hashtbl.create 16;
      inline_stack = ref [];
    }
  in

  List.iter
    (function
      | T.CGlobal gd -> Hashtbl.replace ctx.globals gd.name ()
      (* a variadic body cannot be pasted so those keep using an ordinary call *)
      | T.CFunc tfd when List.mem Ast.Inline tfd.modifiers && not tfd.variadic
        ->
          Hashtbl.replace ctx.inline_funcs tfd.name tfd
      | _ -> ())
    tdecls;

  (* TODO(ead2): enforce pub visibility on struct fields *)
  List.iter
    (function
      | T.CStruct (name, fields, _) -> emit_struct_type ctx name fields
      | _ -> ())
    tdecls;
  (* new line after struct(s) for clean emit output *)
  let has_structs =
    List.exists (function T.CStruct _ -> true | _ -> false) tdecls
  in
  (* No benefit only format *)
  if has_structs then emit ctx "\n";

  (* globals *)
  List.iter
    (function T.CGlobal gd -> emit_global_data ctx gd | _ -> ())
    tdecls;
  let has_globals =
    List.exists (function T.CGlobal _ -> true | _ -> false) tdecls
  in
  if has_globals then emit ctx "\n";

  (* Function defs (externs no body)  *)
  List.iter
    (function
      | T.CFunc tfd -> emit_func ctx tfd
      | T.CExtern _ | T.CStruct _ | T.CGlobal _ | T.CTypeAlias _ | T.CNewtype _
        ->
          ())
    tdecls;

  (* String literals (data sections) *)
  List.iter
    (fun (lbl, content) -> emit_string_data ctx lbl content)
    (List.rev !(ctx.strings));

  Buffer.contents !(ctx.buf)
