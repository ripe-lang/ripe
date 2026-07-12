(* SPDX-License-Identifier: GPL-2.0-only *)

(* https://c9x.me/compile/doc/il.html *)
open Types
module T = Typed_ast

(* TODO(7e23): I need to think about the layout/offset/padding of Structs because C++ treats empty structs as size 1. *)

type qbe_base = W | L | S | D

let qbe_base (t : ty) : qbe_base =
  match resolve_ty t with
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TBool -> W
  (* FIXME(d969): Null terminated strings? Idk yet. *)
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ -> L
  | TFloat F32 -> S
  | TFloat F64 -> D
  | TStruct _ | TArray _ | TSlice _ -> L
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no QBE base type"

let qbe_ty (t : ty) : string =
  match qbe_base t with W -> "w" | L -> "l" | S -> "s" | D -> "d"

let is_unsigned (t : ty) : bool =
  match resolve_ty t with
  | TInt (U8 | U16 | U32 | U64 | Usize) -> true
  | _ -> false

(* the QBE mnemonic prefix, u for unsigned int types and s otherwise *)
let signedness (t : ty) : string = if is_unsigned t then "u" else "s"

(* byte size of each integer kind: bit width / 8 *)
let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let float_kind_size = function F32 -> 4 | F64 -> 8

(* the integers keep their real width
   so folding wraps like the runtime type *)
type const_num = Ni32 of Int32.t | Ni64 of Int64.t | Nf of float

let const_bool b = Ni32 (if b then 1l else 0l)

(* the source signedness says whether the
   new high bits are zeros or the sign bit *)
let const_to_int64 (src_ty : ty) (n : const_num) : Int64.t =
  match n with
  | Ni64 n -> n
  | Ni32 n ->
      if is_unsigned src_ty then Int64.logand (Int64.of_int32 n) 0xFFFFFFFFL
      else Int64.of_int32 n
  | Nf f -> Int64.of_float f

let const_to_float (n : const_num) : float =
  match n with
  | Ni32 n -> Int32.to_float n
  | Ni64 n -> Int64.to_float n
  | Nf f -> f

(* the narrow kinds get masked back to width
   so the value wraps like the target type *)
let wrap_const (ty : ty) (n : Int64.t) : const_num =
  match resolve_ty ty with
  | TInt (I64 | U64 | Isize | Usize) -> Ni64 n
  | TInt kind ->
      let bits = int_kind_size kind * 8 in
      let fitted =
        if is_unsigned (TInt kind) then
          Int64.logand n (Int64.sub (Int64.shift_left 1L bits) 1L)
        else
          let shift = 64 - bits in
          Int64.shift_right (Int64.shift_left n shift) shift
      in
      Ni32 (Int64.to_int32 fitted)
  | _ -> if qbe_base ty = L then Ni64 n else Ni32 (Int64.to_int32 n)

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

(* undoes the s_/d_ tag format_const_num stamps on floats *)
let parse_const_num (ty : ty) (s : string) : const_num =
  match resolve_ty ty with
  | TFloat _ ->
      let n = String.length s in
      let s = if n > 2 && s.[1] = '_' then String.sub s 2 (n - 2) else s in
      Nf (float_of_string s)
  | _ ->
      if qbe_base ty = L then Ni64 (Int64.of_string s)
      else Ni32 (Int32.of_string s)

(* C ABI alignment and padding rules *)
(* TODO(4287): Reordering struct fields by alignment to minimize padding  *)
(* TODO(8969): Add a packed attr to strip padding for exact memory layout *)

let rec ty_align (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid -> Error.ice "TVoid has no alignment"
  | TStruct (name, _) -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          List.fold_left
            (fun acc (_, ft) -> max acc (ty_align structs ft))
            1 fields
      | None ->
          Error.ice (Printf.sprintf "no layout recorded for struct %s" name))
  | TArray (e, _) -> ty_align structs e
  | TSlice _ -> 8
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* n and a must be non-negative *)
let align_to n a = (n + a - 1) / a * a

let rec ty_size (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid -> Error.ice "TVoid has no size"
  | TStruct (name, _) -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          let struct_align = ty_align structs t in
          let offset =
            List.fold_left
              (fun off (_, ft) ->
                let a = ty_align structs ft in
                let off = align_to off a in
                off + ty_size structs ft)
              0 fields
          in
          align_to offset struct_align
      | None ->
          Error.ice (Printf.sprintf "no layout recorded for struct %s" name))
  | TArray (e, n) -> n * align_to (ty_size structs e) (ty_align structs e)
  (* fat pointer: { ptr, len } *)
  | TSlice _ -> 16
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* TODO(1aff): maybe look into escape analysis *)
let rec alloc_instr (t : ty) : string =
  match resolve_ty t with
  | TInt (I64 | U64 | Isize | Usize)
  | TFloat F64
  | TPointer _ | TNull | TCStr | TStruct _ | TFunc _ ->
      "alloc8"
  | TArray (e, _) -> alloc_instr e
  | TSlice _ -> "alloc8"
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TFloat F32 | TBool -> "alloc4"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no alloc instruction"

let qbe_load (t : ty) : string =
  match resolve_ty t with
  | TInt I8 -> "loadsb"
  | TInt U8 | TBool -> "loadub"
  | TInt I16 -> "loadsh"
  | TInt U16 -> "loaduh"
  | TInt I32 -> "loadsw"
  | TInt U32 -> "loaduw"
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ ->
      "loadl"
  | TFloat F32 -> "loads"
  | TFloat F64 -> "loadd"
  | TStruct _ | TArray _ | TSlice _ -> "loadl"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no load instruction"

let qbe_store (t : ty) : string =
  match resolve_ty t with
  | TInt (I8 | U8) | TBool -> "storeb"
  | TInt (I16 | U16) -> "storeh"
  | TInt (I32 | U32) -> "storew"
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ ->
      "storel"
  | TFloat F32 -> "stores"
  | TFloat F64 -> "stored"
  | TStruct _ | TArray _ | TSlice _ -> "storel"
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)
  | TVoid -> Error.ice "TVoid has no store instruction"

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
  const_vals : (string, string) Hashtbl.t;
  const_inits : (string, T.texpr) Hashtbl.t;
  entry : Buffer.t ref;
  in_main : bool ref;
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

let emit ctx fmt = Printf.bprintf !(ctx.buf) fmt
let emit_entry ctx fmt = Printf.bprintf !(ctx.entry) fmt

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
  emit ctx "%s\n" lbl;
  ctx.terminated := false

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

let lvalue_sym (e : T.texpr) : Symbol.t =
  match e.T.desc with
  | T.TIdent s -> s
  | _ -> Error.ice ~span:e.T.span "expected an lvalue"

(* aggregates are addressed by pointer: an ident of this type is its base address *)
let is_aggregate t =
  match resolve_ty t with TArray _ | TSlice _ | TStruct _ -> true | _ -> false

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

let rec emit_expr (ctx : ctx) (e : T.texpr) : string =
  let t = e.T.ty in
  match e.T.desc with
  | T.TInt n -> Int64.to_string n
  | T.TFloat f -> float_lit t f
  | T.TBool b -> if b then "1" else "0"
  | T.TNull -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  (* aggregate: the slot itself is the value so yield its address *)
  | T.TIdent s when is_aggregate t -> sym_addr ctx s
  | T.TIdent s ->
      if Symbol.is_func s.kind then "$" ^ s.name
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %s\n" tmp (qbe_ty t) (qbe_load t)
          (sym_addr ctx s);
        tmp
  | T.TCStr s -> intern_string ctx s
  | T.TCall (callee, args, fixed_count) ->
      let ret_ty = t in
      let arg_strs =
        List.rev
          (List.rev_map
             (fun (a : T.texpr) ->
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
        | T.TIdent sym when Symbol.is_func sym.kind -> "$" ^ sym.name
        | _ -> emit_expr ctx callee
      in
      if ret_ty = TVoid then (
        emit ctx "    call %s(%s)\n" callee (String.concat ", " arg_strs);
        "")
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s call %s(%s)\n" tmp (qbe_ty ret_ty) callee
          (String.concat ", " arg_strs);
        tmp
  | T.TBinOp (Ast.Assign, l, r) -> emit_assign ctx l r t
  | T.TBinOp
      ( ((Ast.AddAssign | Ast.SubAssign | Ast.MulAssign | Ast.DivAssign) as op),
        l,
        r ) ->
      emit_compound_assign ctx op l r
  | T.TBinOp ((Ast.And | Ast.Or), _, _) -> emit_bool_value ctx e
  | T.TBinOp (op, l, r) -> emit_binop ctx op l r t
  | T.TUnOp (op, e) -> emit_unop ctx op e t
  | T.TCast e ->
      let v = emit_expr ctx e in
      emit_cast ctx v e.T.ty t
  | T.TRange _ | T.TRangeInclusive _ ->
      raise
        (Diagnostic.Errors [ Error.unsupported e.T.span "range expressions" ])
  | T.TSizeOf sz -> string_of_int (ty_size ctx.structs sz)
  | T.TFieldAccess (e, field) ->
      let ft = t in
      let ptr = emit_field_addr ctx e field in
      (* aggregate field: its address is the value like TIndex below *)
      if is_aggregate ft then ptr
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %s\n" tmp (qbe_ty ft) (qbe_load ft) ptr;
        tmp
  | T.TIndex (base, idx) ->
      let elem = t in
      let addr = emit_index_addr ctx base idx elem in
      (* nested array: the element is itself an aggregate so yield its address *)
      if is_aggregate elem then addr
      else
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %s\n" tmp (qbe_ty elem) (qbe_load elem) addr;
        tmp
  | T.TLen e -> (
      match resolve_ty e.T.ty with
      | TArray (_, n) -> string_of_int n
      | TSlice _ ->
          (* len lives at offset 8 in the fat pointer *)
          let addr = emit_expr ctx e in
          let lenp = fresh ctx in
          emit ctx "    %s =l add %s, 8\n" lenp addr;
          let l = fresh ctx in
          emit ctx "    %s =l loadl %s\n" l lenp;
          l
      | t ->
          Error.ice ~span:e.T.span
            (Printf.sprintf "TLen on non-array type: %s" (show_ty t)))
  | T.TDataPtr e -> (
      match resolve_ty e.T.ty with
      | TSlice _ ->
          (* ptr lives at offset 0 in the fat pointer *)
          let addr = emit_expr ctx e in
          let p = fresh ctx in
          emit ctx "    %s =l loadl %s\n" p addr;
          p
      (* an array's base address is already the pointer to its first element *)
      | TArray _ -> emit_expr ctx e
      | t ->
          Error.ice ~span:e.T.span
            (Printf.sprintf "TDataPtr on non-array type: %s" (show_ty t)))
  | T.TToSlice arr ->
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
  | T.TSliceExpr (base, lo, hi) ->
      let elem =
        match t with
        | TSlice e -> e
        | _ -> Error.ice ~span:e.T.span "slice expression on non-slice type"
      in
      let base_addr = emit_expr ctx base in
      let storage, blen =
        match resolve_ty base.T.ty with
        | TArray (_, n) -> (base_addr, string_of_int n)
        | TSlice _ ->
            let p = fresh ctx in
            emit ctx "    %s =l loadl %s\n" p base_addr;
            let lenp = fresh ctx in
            emit ctx "    %s =l add %s, 8\n" lenp base_addr;
            let l = fresh ctx in
            emit ctx "    %s =l loadl %s\n" l lenp;
            (p, l)
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
  | T.TArrayLit elems ->
      (* as a value: materialize into a fresh stack slot and yield its address *)
      let elem = array_elem_ty ~span:e.T.span t in
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      emit_array_lit_into ctx slot elems elem;
      slot
  | T.TZero when is_aggregate t ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      emit_zero_into ctx slot t;
      slot
  | T.TZero -> "0"
  | T.TUndef when is_aggregate t ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      slot
  | T.TUndef -> "0"
  | T.TStructLit (sname, tfields) ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s\n" slot (alloc_slot ctx t);
      emit_struct_lit_into ctx slot sname tfields;
      slot

and emit_unop ctx op e t =
  match op with
  (* dereferencing a struct pointer just yields its address, same as any other aggregate lvalue *)
  | Ast.Deref when is_aggregate t -> emit_expr ctx e
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
      | Ast.Deref -> emit ctx "    %s =%s %s %s\n" tmp qt (qbe_load t) ev
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
and emit_index_addr ctx base idx elem =
  let base_addr = emit_expr ctx base in
  let storage, len =
    match resolve_ty base.T.ty with
    | TArray (_, n) -> (base_addr, Some (string_of_int n))
    | TSlice _ ->
        let p = fresh ctx in
        emit ctx "    %s =l loadl %s\n" p base_addr;
        let lenp = fresh ctx in
        emit ctx "    %s =l add %s, 8\n" lenp base_addr;
        let l = fresh ctx in
        emit ctx "    %s =l loadl %s\n" l lenp;
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

and emit_bounds_check ctx idx len =
  let id = fresh_id ctx in
  let fail_lbl = Printf.sprintf "@bounds.fail.%d" id in
  let ok_lbl = Printf.sprintf "@bounds.ok.%d" id in
  let cond = fresh ctx in
  emit ctx "    %s =w cugel %s, %s\n" cond idx len;
  emit_jnz ctx cond fail_lbl ok_lbl;
  emit_label ctx fail_lbl;
  emit ctx "    call $ripe_panic_bounds(l %s, l %s)\n" idx len;
  emit ctx "    hlt\n";
  ctx.terminated := true;
  emit_label ctx ok_lbl

and emit_slice_bounds_check ctx lo hi len =
  let id = fresh_id ctx in
  let fail_lbl = Printf.sprintf "@slice.fail.%d" id in
  let ok_lbl = Printf.sprintf "@slice.ok.%d" id in
  let hi_bad = fresh ctx in
  emit ctx "    %s =w cugtl %s, %s\n" hi_bad hi len;
  let lo_bad = fresh ctx in
  emit ctx "    %s =w cugtl %s, %s\n" lo_bad lo hi;
  let bad = fresh ctx in
  emit ctx "    %s =w or %s, %s\n" bad hi_bad lo_bad;
  emit_jnz ctx bad fail_lbl ok_lbl;
  emit_label ctx fail_lbl;
  emit ctx "    call $ripe_panic_slice_bounds(l %s, l %s, l %s)\n" lo hi len;
  emit ctx "    hlt\n";
  ctx.terminated := true;
  emit_label ctx ok_lbl

(* address of any lvalue, for &e: a name's own slot/global, or the same address computation assign already uses for index/field/deref *)
and emit_lvalue_addr ctx (e : T.texpr) =
  match e.T.desc with
  (* &funcname is already the function pointer. *)
  | T.TIdent s when Symbol.is_func s.kind -> "$" ^ s.name
  | T.TIdent s -> sym_addr ctx s
  | T.TIndex (base, idx) -> emit_index_addr ctx base idx e.T.ty
  | T.TFieldAccess (base, field) -> emit_field_addr ctx base field
  | T.TUnOp (Ast.Deref, inner) -> emit_expr ctx inner
  | _ -> Error.ice ~span:e.T.span "expected an lvalue"

(* address of e.field: base address (or loaded pointer, if base is a pointer) + field offset *)
and emit_field_addr ctx base field =
  let addr = emit_expr ctx base in
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
  | _ -> emit ctx "    %s 0, %s\n" (qbe_store t) dest

(* copy size bytes from src to dest for by-value aggregate (slice) moves *)
and emit_aggregate_copy ctx dest src size =
  let off = ref 0 in
  let step w letter load store =
    while !off + w <= size do
      let at base = offset_addr ctx base !off in
      let v = fresh ctx in
      emit ctx "    %s =%s %s %s\n" v letter load (at src);
      emit ctx "    %s %s, %s\n" store v (at dest);
      off := !off + w
    done
  in
  step 8 "l" "loadl" "storel";
  step 4 "w" "loaduw" "storew";
  step 2 "w" "loaduh" "storeh";
  step 1 "w" "loadub" "storeb"

(* store each element of a literal into an already-allocated array at base *)
and emit_array_lit_into ctx base elems elem =
  let strd = stride ctx.structs elem in
  List.iteri
    (fun i el ->
      let addr = offset_addr ctx base (i * strd) in
      match el.T.desc with
      (* nested literal (multi-dimensional array): recurse into the sub-array *)
      | T.TArrayLit sub ->
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
    (fun (fname, (fe : T.texpr)) ->
      let ft = fe.T.ty in
      let offset = field_offset ctx.structs fields fname in
      let addr = offset_addr ctx base offset in
      match fe.T.desc with
      | T.TStructLit (sub, subfields) ->
          emit_struct_lit_into ctx addr sub subfields
      | T.TArrayLit sub ->
          let subelem = array_elem_ty ~span:fe.T.span ft in
          emit_array_lit_into ctx addr sub subelem
      | T.TZero -> emit_zero_into ctx addr ft
      | T.TUndef -> ()
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
  | T.TIndex (base, idx) ->
      let elem = l.T.ty in
      let addr = emit_index_addr ctx base idx elem in
      emit_store_into ctx elem addr r
  | T.TIdent s -> emit_store_into ctx l.T.ty (sym_addr ctx s) r
  | T.TFieldAccess (base, field) ->
      let addr = emit_field_addr ctx base field in
      emit_store_into ctx l.T.ty addr r
  | T.TUnOp (Ast.Deref, inner) ->
      let addr = emit_expr ctx inner in
      emit_store_into ctx l.T.ty addr r
  | _ -> emit_expr ctx r

(* The caller already worked out the address so every lvalue shape can share
   this store. *)
and emit_store_into ctx ty addr r =
  if is_aggregate ty then begin
    (match (r.T.desc, ty) with
    | T.TArrayLit elems, TArray (elem, _) ->
        emit_array_lit_into ctx addr elems elem
    | T.TStructLit (sname, tfields), _ ->
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

(* x += rhs -> x = x + rhs *)
(* x -= rhs -> x = x - rhs *)
(* x *= rhs -> x = x * rhs *)
(* x /= rhs -> x = x / rhs *)
and compound_arith op lt =
  match op with
  | Ast.AddAssign -> "add"
  | Ast.SubAssign -> "sub"
  | Ast.MulAssign -> "mul"
  | Ast.DivAssign ->
      if is_float lt then "div" else if is_unsigned lt then "udiv" else "div"
  | _ -> Error.ice "unexpected compound assignment operator"

and emit_compound_assign ctx op l r =
  match l.T.desc with
  (* arr[i] += rhs: load through the element address, apply, store back *)
  | T.TIndex (base, idx) ->
      let elem = l.T.ty in
      let addr = emit_index_addr ctx base idx elem in
      emit_compound_via_addr ctx op elem addr r
  | T.TFieldAccess (base, field) ->
      let addr = emit_field_addr ctx base field in
      emit_compound_via_addr ctx op l.T.ty addr r
  | T.TUnOp (Ast.Deref, inner) ->
      let addr = emit_expr ctx inner in
      emit_compound_via_addr ctx op l.T.ty addr r
  | _ ->
      let s = lvalue_sym l in
      emit_compound_via_addr ctx op l.T.ty (sym_addr ctx s) r

(* load through addr, apply the compound op, store the result back *)
and emit_compound_via_addr ctx op elem addr r =
  let qt = qbe_ty elem in
  let cur = fresh ctx in
  emit ctx "    %s =%s %s %s\n" cur qt (qbe_load elem) addr;
  let rv = emit_expr ctx r in
  let new_val = fresh ctx in
  emit ctx "    %s =%s %s %s, %s\n" new_val qt (compound_arith op elem) cur rv;
  emit ctx "    %s %s, %s\n" (qbe_store elem) new_val addr;
  new_val

(* short circuit the condition so the rhs only runs if the lhs doesn't settle it, otherwise p != null && *p == 3 derefs null *)
and emit_branch ctx e true_lbl false_lbl =
  match e.T.desc with
  | T.TBinOp (Ast.And, l, r) ->
      let mid = Printf.sprintf "@and.rhs%d" (fresh_id ctx) in
      emit_branch ctx l mid false_lbl;
      emit_label ctx mid;
      emit_branch ctx r true_lbl false_lbl
  | T.TBinOp (Ast.Or, l, r) ->
      let mid = Printf.sprintf "@or.rhs%d" (fresh_id ctx) in
      emit_branch ctx l true_lbl mid;
      emit_label ctx mid;
      emit_branch ctx r true_lbl false_lbl
  | T.TUnOp (Ast.Not, inner) -> emit_branch ctx inner false_lbl true_lbl
  | _ ->
      let v = emit_expr ctx e in
      emit_jnz ctx v true_lbl false_lbl

(* same condition but wanted as a 0/1 value, e.g. let ok = a && b *)
and emit_bool_value ctx e =
  let id = fresh_id ctx in
  let true_lbl = Printf.sprintf "@bool.true%d" id in
  let false_lbl = Printf.sprintf "@bool.false%d" id in
  let join_lbl = Printf.sprintf "@bool.join%d" id in
  emit_branch ctx e true_lbl false_lbl;
  emit_label ctx true_lbl;
  emit_jmp ctx join_lbl;
  emit_label ctx false_lbl;
  emit_jmp ctx join_lbl;
  emit_label ctx join_lbl;
  let res = fresh ctx in
  emit ctx "    %s =w phi %s 1, %s 0\n" res true_lbl false_lbl;
  res

(* TODO(2cc1): the local arithmetic always emits instructions instead of
      folding through fold_const_num *)
and emit_binop ctx op l r t =
  let lv = emit_expr ctx l in
  let rv = emit_expr ctx r in
  let qt = qbe_ty t in
  let lty = l.T.ty in
  let op_qt = qbe_ty lty in
  let sign = signedness lty in
  let unsigned = is_unsigned lty in

  let tmp = fresh ctx in
  (match op with
  | Ast.Add -> emit ctx "    %s =%s add %s, %s\n" tmp qt lv rv
  | Ast.Sub -> emit ctx "    %s =%s sub %s, %s\n" tmp qt lv rv
  | Ast.Mul -> emit ctx "    %s =%s mul %s, %s\n" tmp qt lv rv
  | Ast.Div ->
      let instr =
        if is_float lty then "div" else if unsigned then "udiv" else "div"
      in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | Ast.Mod ->
      let instr = if unsigned then "urem" else "rem" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
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
  | Ast.Lshift -> emit ctx "    %s =%s shl %s, %s\n" tmp qt lv rv
  | Ast.Rshift ->
      let instr = if unsigned then "shr" else "sar" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | _ -> Error.ice ~span:l.T.span "unexpected binary operator");
  match op with
  | Ast.Add | Ast.Sub | Ast.Mul | Ast.Lshift -> narrow_int_to ctx tmp t
  | _ -> tmp

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

(* TODO(bdc9): `as` silently loses data like C. Add a safe cast that catches bad conversions at runtime. *)
and emit_cast ctx v src_ty target_ty =
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

let rec emit_stmt (ctx : ctx) (s : T.tstmt) : unit =
  match s.T.tsdesc with
  | T.TConst (s, t, e) | T.TVar (s, t, e) -> (
      (* stack slot sized by type (struct sizes resolved from context) *)
      let slot = bind_local ctx s in
      emit ctx "    %%%s =l %s\n" slot (alloc_slot ctx t);
      match e.T.desc with
      | T.TZero -> emit_zero_into ctx ("%" ^ slot) e.T.ty
      | T.TUndef -> ()
      | T.TArrayLit elems ->
          let elem = array_elem_ty ~span:e.T.span e.T.ty in
          emit_array_lit_into ctx ("%" ^ slot) elems elem
      | T.TStructLit (sname, tfields) ->
          emit_struct_lit_into ctx ("%" ^ slot) sname tfields
      | _ when is_aggregate t ->
          let src = emit_expr ctx e in
          emit_aggregate_copy ctx ("%" ^ slot) src (ty_size ctx.structs t)
      | _ ->
          let v = emit_expr ctx e in
          emit ctx "    %s %s, %%%s\n" (qbe_store t) v slot)
  | T.TReturn None ->
      (* a bare return in main exits with 0 like falling off the end *)
      if not !(ctx.terminated) then
        if !(ctx.in_main) then emit ctx "    ret 0\n" else emit ctx "    ret\n";
      ctx.terminated := true
  | T.TReturn (Some e) ->
      let v = emit_expr ctx e in
      if not !(ctx.terminated) then emit ctx "    ret %s\n" v;
      ctx.terminated := true
  (* TODO(d6df): emit_expr in statement position emits dead loads for idents *)
  | T.TExpr e ->
      let _ = emit_expr ctx e in
      ()
  | T.TFor (name, elem_ty, iter, body) -> emit_for ctx name elem_ty iter body
  | T.TIf (branches, else_body) -> (
      let id = fresh_id ctx in
      let n = List.length branches in
      let cond_lbls =
        List.init n (fun i -> Printf.sprintf "@if.cond%d_%d" id i)
      in
      let then_lbls =
        List.init n (fun i -> Printf.sprintf "@if.then%d_%d" id i)
      in
      let else_lbl = Printf.sprintf "@if.else%d" id in
      let end_lbl = Printf.sprintf "@if.end%d" id in
      match branches with
      | [] -> emit_scoped ctx else_body
      | _ ->
          List.iteri
            (fun i (cond, body) ->
              let next_lbl =
                if i + 1 < n then List.nth cond_lbls (i + 1) else else_lbl
              in
              emit_label ctx (List.nth cond_lbls i);
              emit_branch ctx cond (List.nth then_lbls i) next_lbl;
              emit_label ctx (List.nth then_lbls i);
              emit_scoped ctx body;
              emit_jmp ctx end_lbl)
            branches;
          emit_label ctx else_lbl;
          emit_scoped ctx else_body;
          emit_label ctx end_lbl)
  | T.TBlock stmts ->
      (* TODO(e1d8): blocks as expressions e.g. let x = { 5 } *)
      emit_scoped ctx stmts
  | T.TBreak -> (
      match !(ctx.loops) with (_, brk) :: _ -> emit_jmp ctx brk | [] -> ())
  | T.TContinue -> (
      match !(ctx.loops) with (cont, _) :: _ -> emit_jmp ctx cont | [] -> ())
  | T.TWhile (cond, body) ->
      let id = fresh_id ctx in
      let test_lbl = Printf.sprintf "@while.cond%d" id in
      let body_lbl = Printf.sprintf "@while.body%d" id in
      let end_lbl = Printf.sprintf "@while.end%d" id in
      emit_label ctx test_lbl;
      emit_branch ctx cond body_lbl end_lbl;
      emit_label ctx body_lbl;
      (* continue re-tests the condition and break exits *)
      ctx.loops := (test_lbl, end_lbl) :: !(ctx.loops);
      emit_scoped ctx body;
      ctx.loops := List.tl !(ctx.loops);
      emit_jmp ctx test_lbl;
      emit_label ctx end_lbl

(* for i in lo..hi and for x in arr *)
and emit_for ctx name elem_ty iter body =
  let id = fresh_id ctx in
  let cond_lbl = Printf.sprintf "@for.cond%d" id in
  let body_lbl = Printf.sprintf "@for.body%d" id in
  let cont_lbl = Printf.sprintf "@for.cont%d" id in
  let end_lbl = Printf.sprintf "@for.end%d" id in
  let qt = qbe_ty elem_ty in
  let sign = signedness elem_ty in
  let slot = bind_local ctx name in
  let run_body () =
    ctx.loops := (cont_lbl, end_lbl) :: !(ctx.loops);
    emit_stmts ctx body;
    ctx.loops := List.tl !(ctx.loops)
  in
  match iter.T.desc with
  | T.TRange (lo, hi) | T.TRangeInclusive (lo, hi) ->
      let inclusive =
        match iter.T.desc with T.TRangeInclusive _ -> true | _ -> false
      in
      (* loop var lives in a stack slot so the body can read and increment it *)
      emit ctx "    %%%s =l %s\n" slot (alloc_slot ctx elem_ty);
      let lov = emit_expr ctx lo in
      emit ctx "    %s %s, %%%s\n" (qbe_store elem_ty) lov slot;
      (* upper bound is evaluated once before the loop *)
      let hiv = emit_expr ctx hi in
      emit_label ctx cond_lbl;
      let cur = fresh ctx in
      emit ctx "    %s =%s %s %%%s\n" cur qt (qbe_load elem_ty) slot;
      let cmp = fresh ctx in
      let cmpop = if inclusive then "le" else "lt" in
      emit ctx "    %s =w c%s%s%s %s, %s\n" cmp sign cmpop qt cur hiv;
      emit_jnz ctx cmp body_lbl end_lbl;
      emit_label ctx body_lbl;
      run_body ();
      emit_label ctx cont_lbl;
      let cur2 = fresh ctx in
      emit ctx "    %s =%s %s %%%s\n" cur2 qt (qbe_load elem_ty) slot;
      let nxt = fresh ctx in
      emit ctx "    %s =%s add %s, 1\n" nxt qt cur2;
      emit ctx "    %s %s, %%%s\n" (qbe_store elem_ty) nxt slot;
      emit_jmp ctx cond_lbl;
      emit_label ctx end_lbl
  | _ ->
      (* array/slice iteration: walk index 0..len with a hidden counter.
         storage is the element pointer, len is a constant (array) or loaded (slice) *)
      let storage, len =
        let base = emit_expr ctx iter in
        match resolve_ty iter.T.ty with
        | TArray (_, n) -> (base, string_of_int n)
        | TSlice _ ->
            let p = fresh ctx in
            emit ctx "    %s =l loadl %s\n" p base;
            let lenp = fresh ctx in
            emit ctx "    %s =l add %s, 8\n" lenp base;
            let l = fresh ctx in
            emit ctx "    %s =l loadl %s\n" l lenp;
            (p, l)
        | t ->
            Error.ice ~span:iter.T.span
              (Printf.sprintf "cannot iterate over type: %s" (show_ty t))
      in
      let idx = Printf.sprintf "%%for.i%d" id in
      emit ctx "    %s =l alloc8 8\n" idx;
      emit ctx "    storel 0, %s\n" idx;
      (* element binding slot, refreshed each iteration *)
      emit ctx "    %%%s =l %s\n" slot (alloc_slot ctx elem_ty);
      emit_label ctx cond_lbl;
      let i = fresh ctx in
      emit ctx "    %s =l loadl %s\n" i idx;
      let cmp = fresh ctx in
      emit ctx "    %s =w csltl %s, %s\n" cmp i len;
      emit_jnz ctx cmp body_lbl end_lbl;
      emit_label ctx body_lbl;
      let off = fresh ctx in
      emit ctx "    %s =l mul %s, %d\n" off i (stride ctx.structs elem_ty);
      let addr = fresh ctx in
      emit ctx "    %s =l add %s, %s\n" addr storage off;
      (if is_aggregate elem_ty then
         (* aggregate element: copy its bytes into the loop variable *)
         emit_aggregate_copy ctx ("%" ^ slot) addr (ty_size ctx.structs elem_ty)
       else
         let v = fresh ctx in
         emit ctx "    %s =%s %s %s\n" v qt (qbe_load elem_ty) addr;
         emit ctx "    %s %s, %%%s\n" (qbe_store elem_ty) v slot);
      run_body ();
      emit_label ctx cont_lbl;
      let i2 = fresh ctx in
      emit ctx "    %s =l loadl %s\n" i2 idx;
      let nxt = fresh ctx in
      emit ctx "    %s =l add %s, 1\n" nxt i2;
      emit ctx "    storel %s, %s\n" nxt idx;
      emit_jmp ctx cond_lbl;
      emit_label ctx end_lbl

and emit_stmts ctx stmts = List.iter (emit_stmt ctx) stmts

(* Binder ids are unique so slots never collide and scopes don't need save
   and restore. *)
and emit_scoped ctx stmts = emit_stmts ctx stmts

let emit_func (ctx : ctx) (tfd : T.tfunc_def) =
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
  let ret_part = match tfd.ret_ty with TVoid -> "" | t -> qbe_ty t ^ " " in
  (* TODO(572b): export pub functions *)
  (* TODO(c561): emit inline functions at call site *)
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

  emit_stmts ctx tfd.body;

  ctx.buf := out;
  Buffer.add_buffer out !(ctx.entry);
  Buffer.add_buffer out body;

  (* close the final block if control can still fall off the end *)
  (* TODO(978e): Emit implicit return for non-void functions where the last expression is the return value. *)
  if not !(ctx.terminated) then
    if is_main then emit ctx "    ret 0\n"
    else if tfd.ret_ty = TVoid then emit ctx "    ret\n"
      (* unreachable but QBE needs every block terminated *)
    else emit ctx "    hlt\n";
  (* TODO(aa3a): error in typechecker for non-void functions missing a return on all paths *)
  emit ctx "}\n\n"

let rec qbe_ext_ty (t : ty) : string =
  match resolve_ty t with
  | TInt (I8 | U8) | TBool -> "b"
  | TInt (I16 | U16) -> "h"
  | TInt (I32 | U32) -> "w"
  (* null is a pointer no type but all pointers are 64-bit *)
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ ->
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

let emit_struct_type (ctx : ctx) (name : string) (fields : (string * ty) list) =
  let field_strs = List.map (fun (_, t) -> qbe_ext_ty t) fields in
  emit ctx "type :%s = { %s }\n" name (String.concat ", " field_strs)

let rec fold_const_value (ctx : ctx) (te : T.texpr) : string =
  match te.T.desc with
  | T.TInt n -> Int64.to_string n
  | T.TBool b -> if b then "1" else "0"
  | T.TNull -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  | T.TSizeOf t -> string_of_int (ty_size ctx.structs t)
  | T.TFloat f -> format_const_num te.T.ty (Nf f)
  | T.TCast e -> (
      match resolve_ty te.T.ty with
      | TInt _ | TFloat _ | TBool ->
          format_const_num te.T.ty (fold_const_num ctx te)
      | _ -> fold_const_value ctx e)
  | T.TIdent s when Symbol.is_func s.kind -> "$" ^ s.name
  | T.TIdent s -> resolve_const ctx s.name te.T.span
  | T.TCStr s -> intern_string ctx s
  | T.TBinOp _ | T.TUnOp _ -> format_const_num te.T.ty (fold_const_num ctx te)
  | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])

and unsupported_const span =
  Diagnostic.(
    error "unsupported constant expression"
    |> at span
    |> help "constant initializers must fold to a compile-time value")

and fold_const_num (ctx : ctx) (te : T.texpr) : const_num =
  match te.T.desc with
  | T.TInt n -> wrap_const te.T.ty n
  | T.TBool b -> const_bool b
  | T.TChar c -> Ni32 (Int32.of_int (Char.code c))
  | T.TFloat f -> Nf f
  | T.TSizeOf t -> wrap_const te.T.ty (Int64.of_int (ty_size ctx.structs t))
  | T.TIdent s -> parse_const_num te.T.ty (resolve_const ctx s.name te.T.span)
  | T.TCast e -> (
      let v = fold_const_num ctx e in
      match (resolve_ty te.T.ty, v) with
      | TFloat _, Nf f -> Nf f
      | TFloat _, _ -> Nf (const_to_float v)
      | _, Nf f -> wrap_const te.T.ty (Int64.of_float f)
      | _, _ -> wrap_const te.T.ty (const_to_int64 e.T.ty v))
  | T.TUnOp (Ast.Neg, e) -> (
      match fold_const_num ctx e with
      | Nf f -> Nf (-.f)
      | v -> wrap_const te.T.ty (Int64.neg (const_to_int64 e.T.ty v)))
  | T.TUnOp (Ast.BitNot, e) -> (
      match fold_const_num ctx e with
      | Nf _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])
      | v -> wrap_const te.T.ty (Int64.lognot (const_to_int64 e.T.ty v)))
  | T.TUnOp (Ast.Not, e) -> (
      match fold_const_num ctx e with
      | Nf _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])
      | v -> const_bool (const_to_int64 e.T.ty v = 0L))
  | T.TUnOp ((Ast.Deref | Ast.AddressOf), _) ->
      raise (Diagnostic.Errors [ unsupported_const te.T.span ])
  | T.TBinOp (op, l, r) ->
      fold_const_binop te.T.span op ~result_ty:te.T.ty ~operand_ty:l.T.ty
        (fold_const_num ctx l) (fold_const_num ctx r)
  | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])

and fold_const_binop (span : Ast.span) (op : Ast.binop) ~(result_ty : ty)
    ~(operand_ty : ty) (a : const_num) (b : const_num) : const_num =
  match (a, b) with
  | Nf _, _ | _, Nf _ -> (
      let x, y = (const_to_float a, const_to_float b) in
      match op with
      | Ast.Add -> Nf (x +. y)
      | Ast.Sub -> Nf (x -. y)
      | Ast.Mul -> Nf (x *. y)
      | Ast.Div -> Nf (x /. y)
      | Ast.Eq -> const_bool (x = y)
      | Ast.Neq -> const_bool (x <> y)
      | Ast.Lt -> const_bool (x < y)
      | Ast.Gt -> const_bool (x > y)
      | Ast.Lte -> const_bool (x <= y)
      | Ast.Gte -> const_bool (x >= y)
      | _ -> raise (Diagnostic.Errors [ unsupported_const span ]))
  | _ -> (
      let unsigned = is_unsigned operand_ty in
      let x = const_to_int64 operand_ty a and y = const_to_int64 operand_ty b in
      let wrap = wrap_const result_ty in
      let cmp =
        if unsigned then Int64.unsigned_compare x y else Int64.compare x y
      in
      match op with
      | Ast.Add -> wrap (Int64.add x y)
      | Ast.Sub -> wrap (Int64.sub x y)
      | Ast.Mul -> wrap (Int64.mul x y)
      | Ast.Div when y = 0L ->
          raise
            (Diagnostic.Errors
               [ Diagnostic.(error "division by zero in constant" |> at span) ])
      | Ast.Div ->
          wrap (if unsigned then Int64.unsigned_div x y else Int64.div x y)
      | Ast.Mod when y = 0L ->
          raise
            (Diagnostic.Errors
               [ Diagnostic.(error "remainder by zero in constant" |> at span) ])
      | Ast.Mod ->
          wrap (if unsigned then Int64.unsigned_rem x y else Int64.rem x y)
      | Ast.BitAnd -> wrap (Int64.logand x y)
      | Ast.BitOr -> wrap (Int64.logor x y)
      | Ast.BitXor -> wrap (Int64.logxor x y)
      | Ast.Lshift -> wrap (Int64.shift_left x (Int64.to_int y))
      | Ast.Rshift ->
          wrap
            (if unsigned then Int64.shift_right_logical x (Int64.to_int y)
             else Int64.shift_right x (Int64.to_int y))
      | Ast.Eq -> const_bool (x = y)
      | Ast.Neq -> const_bool (x <> y)
      | Ast.Lt -> const_bool (cmp < 0)
      | Ast.Gt -> const_bool (cmp > 0)
      | Ast.Lte -> const_bool (cmp <= 0)
      | Ast.Gte -> const_bool (cmp >= 0)
      | Ast.And -> const_bool (x <> 0L && y <> 0L)
      | Ast.Or -> const_bool (x <> 0L || y <> 0L)
      | Ast.Assign | Ast.AddAssign | Ast.SubAssign | Ast.MulAssign
      | Ast.DivAssign ->
          raise (Diagnostic.Errors [ unsupported_const span ]))

(* yank it from the table first so a cycle dies here instead of looping forever *)
and resolve_const (ctx : ctx) (name : string) (span : Ast.span) : string =
  match Hashtbl.find_opt ctx.const_vals name with
  | Some v -> v
  | None -> (
      match Hashtbl.find_opt ctx.const_inits name with
      | None ->
          raise (Diagnostic.Errors [ Error.named span "cyclic constant" name ])
      | Some init ->
          Hashtbl.remove ctx.const_inits name;
          let v = fold_const_value ctx init in
          Hashtbl.replace ctx.const_vals name v;
          v)

(* QBE data fields for a constant array literal, e.g. "w 1, w 2, w 3" *)
let rec const_array_fields (ctx : ctx) (te : T.texpr) : string =
  match te.T.desc with
  | T.TArrayLit elems ->
      String.concat ", " (List.map (const_array_fields ctx) elems)
  | T.TStructLit (_, tfields) ->
      let off = ref 0 in
      let parts = ref [] in
      List.iter
        (fun (_, (fe : T.texpr)) ->
          let ft = fe.T.ty in
          let aligned = align_to !off (ty_align ctx.structs ft) in
          if aligned > !off then
            parts := Printf.sprintf "z %d" (aligned - !off) :: !parts;
          (match fe.T.desc with
          | T.TZero ->
              parts := Printf.sprintf "z %d" (ty_size ctx.structs ft) :: !parts
          | _ -> parts := const_array_fields ctx fe :: !parts);
          off := aligned + ty_size ctx.structs ft)
        tfields;
      let total = ty_size ctx.structs te.T.ty in
      if total > !off then
        parts := Printf.sprintf "z %d" (total - !off) :: !parts;
      String.concat ", " (List.rev !parts)
  | _ -> Printf.sprintf "%s %s" (qbe_ext_ty te.T.ty) (fold_const_value ctx te)

let emit_global_data (ctx : ctx) (gd : T.tglobal_def) =
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
    (fun c ->
      match c with
      | '"' -> Buffer.add_string buf "\\\""
      | '\\' -> Buffer.add_string buf "\\\\"
      | '\n' -> Buffer.add_string buf "\\n"
      | '\t' -> Buffer.add_string buf "\\t"
      | c -> Buffer.add_char buf c)
    content;
  emit ctx "data %s = { b \"%s\", b 0 }\n" lbl (Buffer.contents buf)

let emit_qbe (tdecls : T.tdecl list) : string =
  (* Collect struct layouts for offset comp *)
  let structs = Hashtbl.create 8 in
  List.iter
    (function
      | T.TStruct (name, fields, _) -> Hashtbl.replace structs name fields
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
      const_vals = Hashtbl.create 16;
      const_inits = Hashtbl.create 16;
      entry = ref (Buffer.create 64);
      in_main = ref false;
    }
  in

  List.iter
    (function
      | T.TGlobal gd -> (
          Hashtbl.replace ctx.globals gd.name ();
          match gd.init with
          | Some te when gd.is_const ->
              Hashtbl.replace ctx.const_inits gd.name te
          | _ -> ())
      | _ -> ())
    tdecls;

  (* TODO(ead2): enforce pub visibility on struct fields *)
  List.iter
    (function
      | T.TStruct (name, fields, _) -> emit_struct_type ctx name fields
      | _ -> ())
    tdecls;
  (* new line after struct(s) for clean emit output *)
  let has_structs =
    List.exists (function T.TStruct _ -> true | _ -> false) tdecls
  in
  (* No benefit only format *)
  if has_structs then emit ctx "\n";

  (* globals *)
  List.iter
    (function T.TGlobal gd -> emit_global_data ctx gd | _ -> ())
    tdecls;
  let has_globals =
    List.exists (function T.TGlobal _ -> true | _ -> false) tdecls
  in
  if has_globals then emit ctx "\n";

  (* Function defs (externs no body)  *)
  List.iter
    (function
      | T.TFunc tfd -> emit_func ctx tfd
      | TExtern _ | T.TStruct _ | T.TGlobal _ | T.TTypeAlias _ | T.TNewtype _ ->
          ())
    tdecls;

  (* String literals (data sections) *)
  List.iter
    (fun (lbl, content) -> emit_string_data ctx lbl content)
    (List.rev !(ctx.strings));

  Buffer.contents !(ctx.buf)
