(* SPDX-License-Identifier: GPL-2.0-only *)

(* https://c9x.me/compile/doc/il.html *)
open Types
module T = Typed_ast

(* TODO(7e23): I need to think about the layout/offset/padding of Structs because C++ treats empty structs as size 1. *)

type qbe_base = W | L | S | D

let qbe_base (t : ty) : qbe_base =
  match t with
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TBool -> W
  (* FIXME(d969): Null terminated strings? Idk yet. *)
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ -> L
  | TFloat F32 -> S
  | TFloat F64 -> D
  | TStruct _ | TArray _ | TSlice _ -> L
  | TVoid ->
      raise (Diagnostic.Errors [ Error.internal "TVoid has no QBE base type" ])

let qbe_ty (t : ty) : string =
  match qbe_base t with W -> "w" | L -> "l" | S -> "s" | D -> "d"

let is_unsigned (t : ty) : bool =
  match t with TInt (U8 | U16 | U32 | U64 | Usize) -> true | _ -> false

(* the QBE mnemonic prefix, u for unsigned int types and s otherwise *)
let signedness (t : ty) : string = if is_unsigned t then "u" else "s"

(* byte size of each integer kind: bit width / 8 *)
let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let float_kind_size = function F32 -> 4 | F64 -> 8

(* C ABI alignment and padding rules *)
(* TODO(4287): Reordering struct fields by alignment to minimize padding  *)
(* TODO(8969): Add a packed attr to strip padding for exact memory layout *)

let rec ty_align (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid ->
      raise (Diagnostic.Errors [ Error.internal "TVoid has no alignment" ])
  | TStruct name -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          List.fold_left
            (fun acc (_, ft) -> max acc (ty_align structs ft))
            1 fields
      | None ->
          raise
            (Diagnostic.Errors
               [
                 Error.internal
                   (Printf.sprintf "no layout recorded for struct %s" name);
               ]))
  | TArray (e, _) -> ty_align structs e
  | TSlice _ -> 8

(* n and a must be non-negative *)
let align_to n a = (n + a - 1) / a * a

let rec ty_size (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid -> raise (Diagnostic.Errors [ Error.internal "TVoid has no size" ])
  | TStruct name -> (
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
          raise
            (Diagnostic.Errors
               [
                 Error.internal
                   (Printf.sprintf "no layout recorded for struct %s" name);
               ]))
  | TArray (e, n) -> n * align_to (ty_size structs e) (ty_align structs e)
  (* fat pointer: { ptr, len } *)
  | TSlice _ -> 16

(* TODO(1aff): maybe look into escape analysis *)
let rec alloc_instr (t : ty) : string =
  match t with
  | TInt (I64 | U64 | Isize | Usize)
  | TFloat F64
  | TPointer _ | TNull | TCStr | TStruct _ | TFunc _ ->
      "alloc8"
  | TArray (e, _) -> alloc_instr e
  | TSlice _ -> "alloc8"
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TFloat F32 | TBool -> "alloc4"
  | TVoid ->
      raise
        (Diagnostic.Errors [ Error.internal "TVoid has no alloc instruction" ])

let qbe_load (t : ty) : string =
  match t with
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
  | TVoid ->
      raise
        (Diagnostic.Errors [ Error.internal "TVoid has no load instruction" ])

let qbe_store (t : ty) : string =
  match t with
  | TInt (I8 | U8) | TBool -> "storeb"
  | TInt (I16 | U16) -> "storeh"
  | TInt (I32 | U32) -> "storew"
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ ->
      "storel"
  | TFloat F32 -> "stores"
  | TFloat F64 -> "stored"
  | TStruct _ | TArray _ | TSlice _ -> "storel"
  | TVoid ->
      raise
        (Diagnostic.Errors [ Error.internal "TVoid has no store instruction" ])

type ctx = {
  structs : (string, (string * ty) list) Hashtbl.t;
  locals : (string, string) Hashtbl.t;
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

let new_slot ctx name =
  if Hashtbl.mem ctx.locals name then Printf.sprintf "%s.%d" name (fresh_id ctx)
  else name

let bind_local ctx name slot = Hashtbl.replace ctx.locals name slot
let local_slot ctx name = Hashtbl.find_opt ctx.locals name
let save_locals ctx = Hashtbl.copy ctx.locals

let restore_locals ctx saved =
  Hashtbl.reset ctx.locals;
  Hashtbl.iter (Hashtbl.replace ctx.locals) saved

let emit ctx fmt = Printf.bprintf !(ctx.buf) fmt
let emit_entry ctx fmt = Printf.bprintf !(ctx.entry) fmt

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
    | [] ->
        raise
          (Diagnostic.Errors
             [ Error.internal (Printf.sprintf "unknown field %s" fname) ])
    | (n, ft) :: rest ->
        let a = ty_align structs ft in
        let off = align_to off a in
        if n = fname then off else go (off + ty_size structs ft) rest
  in
  go 0 fields

let lvalue_name (e : T.texpr) : string =
  match e.T.desc with
  | T.TIdent name -> name
  | _ ->
      raise
        (Diagnostic.Errors
           [ Error.internal ~span:e.T.span "expected an lvalue" ])

(* aggregates are addressed by pointer: an ident of this type is its base address *)
let is_aggregate = function
  | TArray _ | TSlice _ | TStruct _ -> true
  | _ -> false

(* bytes between consecutive elements (element size rounded up to its alignment) *)
let stride structs elem =
  align_to (ty_size structs elem) (ty_align structs elem)

let rec emit_expr (ctx : ctx) (e : T.texpr) : string =
  let t = e.T.ty in
  match e.T.desc with
  | T.TInt n -> string_of_int n
  | T.TFloat f ->
      let prefix, digits =
        match t with TFloat F32 -> ("s_", 9) | _ -> ("d_", 17)
      in
      prefix ^ Printf.sprintf "%.*g" digits f
  | T.TBool b -> if b then "1" else "0"
  | T.TNull -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  (* aggregate: the slot itself is the value so yield its address *)
  | T.TIdent name when is_aggregate t -> (
      match local_slot ctx name with
      | Some slot -> "%" ^ slot
      | None -> if Hashtbl.mem ctx.globals name then "$" ^ name else "%" ^ name)
  | T.TIdent name -> (
      match local_slot ctx name with
      | Some slot ->
          let tmp = fresh ctx in
          emit ctx "    %s =%s %s %%%s\n" tmp (qbe_ty t) (qbe_load t) slot;
          tmp
      | None -> (
          if Hashtbl.mem ctx.globals name then (
            let tmp = fresh ctx in
            emit ctx "    %s =%s %s $%s\n" tmp (qbe_ty t) (qbe_load t) name;
            tmp)
          else match t with TFunc _ -> "$" ^ name | _ -> "%" ^ name))
  | T.TCStr s ->
      let lbl = Printf.sprintf "$str%d" !(ctx.str_ctr) in
      incr ctx.str_ctr;
      ctx.strings := (lbl, s) :: !(ctx.strings);
      lbl
  | T.TCall (name, args) ->
      let ret_ty = t in
      let arg_strs =
        List.rev
          (List.rev_map
             (fun (a : T.texpr) ->
               Printf.sprintf "%s %s" (qbe_ty a.T.ty) (emit_expr ctx a))
             args)
      in
      (* local var holding a fn ptr: load then call indirectly *)
      let callee =
        match local_slot ctx name with
        | Some slot ->
            let tmp = fresh ctx in
            emit ctx "    %s =l loadl %%%s\n" tmp slot;
            tmp
        | None -> "$" ^ name
      in
      if ret_ty = TVoid then (
        (* void: no result to capture so just emit the call *)
        emit ctx "    call %s(%s)\n" callee (String.concat ", " arg_strs);
        "")
      else
        (* non-void: capture result in a fresh temporary *)
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
  (* TODO(c75e): codegen for string interpolation *)
  | T.TInterpString _ ->
      raise
        (Diagnostic.Errors [ Error.unsupported e.T.span "string interpolation" ])
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
      match e.T.ty with
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
          raise
            (Diagnostic.Errors
               [
                 Error.internal ~span:e.T.span
                   (Printf.sprintf "TLen on non-array type: %s" (show_ty t));
               ]))
  | T.TDataPtr e -> (
      match e.T.ty with
      | TSlice _ ->
          (* ptr lives at offset 0 in the fat pointer *)
          let addr = emit_expr ctx e in
          let p = fresh ctx in
          emit ctx "    %s =l loadl %s\n" p addr;
          p
      (* an array's base address is already the pointer to its first element *)
      | TArray _ -> emit_expr ctx e
      | t ->
          raise
            (Diagnostic.Errors
               [
                 Error.internal ~span:e.T.span
                   (Printf.sprintf "TDataPtr on non-array type: %s" (show_ty t));
               ]))
  | T.TToSlice arr ->
      let arr_addr = emit_expr ctx arr in
      let n =
        match arr.T.ty with
        | TArray (_, n) -> n
        | t ->
            raise
              (Diagnostic.Errors
                 [
                   Error.internal ~span:arr.T.span
                     (Printf.sprintf "cannot coerce to slice: %s" (show_ty t));
                 ])
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
        | _ ->
            raise
              (Diagnostic.Errors
                 [
                   Error.internal ~span:e.T.span
                     "slice expression on non-slice type";
                 ])
      in
      let storage = data_ptr ctx base in
      let lo_l = widen_to_l ctx (emit_expr ctx lo) lo.T.ty in
      let hi_l = widen_to_l ctx (emit_expr ctx hi) hi.T.ty in
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
      let elem =
        match t with
        | TArray (el, _) -> el
        | _ ->
            raise
              (Diagnostic.Errors
                 [
                   Error.internal ~span:e.T.span
                     "array literal on non-array type";
                 ])
      in
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s %d\n" slot (alloc_instr t)
        (ty_size ctx.structs t);
      emit_array_lit_into ctx slot elems elem;
      slot
  | T.TZero when is_aggregate t ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s %d\n" slot (alloc_instr t)
        (ty_size ctx.structs t);
      emit_zero_into ctx slot t;
      slot
  | T.TZero -> "0"
  | T.TUndef when is_aggregate t ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s %d\n" slot (alloc_instr t)
        (ty_size ctx.structs t);
      slot
  | T.TUndef -> "0"
  | T.TStructLit (sname, tfields) ->
      let slot = fresh ctx in
      emit_entry ctx "    %s =l %s %d\n" slot (alloc_instr t)
        (ty_size ctx.structs t);
      emit_struct_lit_into ctx slot sname tfields;
      slot

and emit_unop ctx op e t =
  match op with
  (* dereferencing a struct pointer just yields its address, same as any other aggregate lvalue *)
  | Ast.Deref when is_aggregate t -> emit_expr ctx e
  | Ast.Neg | Ast.Not | Ast.BitNot | Ast.Deref ->
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
      | _ ->
          raise
            (Diagnostic.Errors
               [ Error.internal ~span:e.T.span "unexpected unary operator" ]));
      tmp
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
  match base.T.ty with
  | TSlice _ ->
      let p = fresh ctx in
      emit ctx "    %s =l loadl %s\n" p addr;
      p
  | _ -> addr

(* address of arr[idx]: storage + idx * stride(elem) *)
and emit_index_addr ctx base idx elem =
  let storage = data_ptr ctx base in
  let iv = emit_expr ctx idx in
  let iw = widen_to_l ctx iv idx.T.ty in
  let off = fresh ctx in
  emit ctx "    %s =l mul %s, %d\n" off iw (stride ctx.structs elem);
  let addr = fresh ctx in
  emit ctx "    %s =l add %s, %s\n" addr storage off;
  addr

(* address of any lvalue, for &e: a name's own slot/global, or the same address computation assign already uses for index/field/deref *)
and emit_lvalue_addr ctx (e : T.texpr) =
  match e.T.desc with
  | T.TIdent name -> (
      match local_slot ctx name with
      | Some slot -> "%" ^ slot
      | None -> if Hashtbl.mem ctx.globals name then "$" ^ name else "%" ^ name)
  | T.TIndex (base, idx) -> emit_index_addr ctx base idx e.T.ty
  | T.TFieldAccess (base, field) -> emit_field_addr ctx base field
  | T.TUnOp (Ast.Deref, inner) -> emit_expr ctx inner
  | _ ->
      raise
        (Diagnostic.Errors
           [ Error.internal ~span:e.T.span "expected an lvalue" ])

(* address of e.field: base address (or loaded pointer, if base is a pointer) + field offset *)
and emit_field_addr ctx base field =
  let addr = emit_expr ctx base in
  let rec peel = function
    | TStruct n -> n
    | TPointer t -> peel t
    | _ ->
        raise
          (Diagnostic.Errors
             [
               Error.internal ~span:base.T.span
                 "field access on non-struct type";
             ])
  in
  let struct_name = peel base.T.ty in
  let fields = Hashtbl.find ctx.structs struct_name in
  let offset = field_offset ctx.structs fields field in
  if offset = 0 then addr
  else
    let p = fresh ctx in
    emit ctx "    %s =l add %s, %d\n" p addr offset;
    p

(* write a zero value of type t into the slot at dest *)
and emit_zero_into ctx dest t =
  match t with
  | TArray _ | TSlice _ | TStruct _ ->
      let size = ty_size ctx.structs t in
      let off = ref 0 in
      let step w store =
        while !off + w <= size do
          let dp =
            if !off = 0 then dest
            else
              let a = fresh ctx in
              emit ctx "    %s =l add %s, %d\n" a dest !off;
              a
          in
          emit ctx "    %s 0, %s\n" store dp;
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
      let at base =
        if !off = 0 then base
        else
          let a = fresh ctx in
          emit ctx "    %s =l add %s, %d\n" a base !off;
          a
      in
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
      let addr =
        if i = 0 then base
        else
          let a = fresh ctx in
          emit ctx "    %s =l add %s, %d\n" a base (i * strd);
          a
      in
      match el.T.desc with
      (* nested literal (multi-dimensional array): recurse into the sub-array *)
      | T.TArrayLit sub ->
          let subelem =
            match el.T.ty with
            | TArray (e, _) -> e
            | _ ->
                raise
                  (Diagnostic.Errors
                     [
                       Error.internal ~span:el.T.span
                         "nested array literal on non-array type";
                     ])
          in
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
      let addr =
        if offset = 0 then base
        else
          let a = fresh ctx in
          emit ctx "    %s =l add %s, %d\n" a base offset;
          a
      in
      match fe.T.desc with
      | T.TStructLit (sub, subfields) ->
          emit_struct_lit_into ctx addr sub subfields
      | T.TArrayLit sub ->
          let subelem =
            match ft with
            | TArray (e, _) -> e
            | _ ->
                raise
                  (Diagnostic.Errors
                     [
                       Error.internal ~span:fe.T.span
                         "array literal field on non-array type";
                     ])
          in
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
      let rv = emit_expr ctx r in
      emit ctx "    %s %s, %s\n" (qbe_store elem) rv addr;
      rv
  | T.TIdent name when is_aggregate l.T.ty ->
      let t = l.T.ty in
      let base =
        match local_slot ctx name with
        | Some slot -> "%" ^ slot
        | None ->
            if Hashtbl.mem ctx.globals name then "$" ^ name else "%" ^ name
      in
      (match (r.T.desc, t) with
      | T.TArrayLit elems, TArray (elem, _) ->
          emit_array_lit_into ctx base elems elem
      | T.TStructLit (sname, tfields), _ ->
          emit_struct_lit_into ctx base sname tfields
      | _ ->
          (* aggregate: copy the value's bytes into the target slot *)
          let src = emit_expr ctx r in
          emit_aggregate_copy ctx base src (ty_size ctx.structs t));
      base
  | T.TFieldAccess (base, field) ->
      let addr = emit_field_addr ctx base field in
      emit_store_into ctx l.T.ty addr r
  | T.TUnOp (Ast.Deref, inner) ->
      let addr = emit_expr ctx inner in
      emit_store_into ctx l.T.ty addr r
  | _ ->
      let rv = emit_expr ctx r in
      (match l.T.desc with
      | T.TIdent name -> (
          match local_slot ctx name with
          | Some slot -> emit ctx "    %s %s, %%%s\n" (qbe_store l.T.ty) rv slot
          | None ->
              if Hashtbl.mem ctx.globals name then
                emit ctx "    %s %s, $%s\n" (qbe_store l.T.ty) rv name
              else emit ctx "    %%%s =%s copy %s\n" name (qbe_ty r.T.ty) rv)
      | _ -> ());
      rv

(* store rhs into an address computed for a pointer or field lvalue *)
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
      if is_float_ty lt then "div" else if is_unsigned lt then "udiv" else "div"
  | _ ->
      raise
        (Diagnostic.Errors
           [ Error.internal "unexpected compound assignment operator" ])

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
      let name = lvalue_name l in
      let lt = l.T.ty in
      let qt = qbe_ty lt in
      let cur = fresh ctx in
      (match local_slot ctx name with
      | Some slot -> emit ctx "    %s =%s %s %%%s\n" cur qt (qbe_load lt) slot
      | None ->
          if Hashtbl.mem ctx.globals name then
            emit ctx "    %s =%s %s $%s\n" cur qt (qbe_load lt) name
          else emit ctx "    %s =%s copy %%%s\n" cur qt name);
      let rv = emit_expr ctx r in
      let new_val = fresh ctx in
      emit ctx "    %s =%s %s %s, %s\n" new_val qt (compound_arith op lt) cur rv;
      (match local_slot ctx name with
      | Some slot -> emit ctx "    %s %s, %%%s\n" (qbe_store lt) new_val slot
      | None ->
          if Hashtbl.mem ctx.globals name then
            emit ctx "    %s %s, $%s\n" (qbe_store lt) new_val name
          else emit ctx "    %%%s =%s copy %s\n" name qt new_val);
      new_val

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

and is_float_ty = function TFloat _ -> true | _ -> false

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
  (* recursively evaluate *)
  let lv = emit_expr ctx l in
  let rv = emit_expr ctx r in
  (* type translation *)
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
        if is_float_ty lty then "div" else if unsigned then "udiv" else "div"
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
      if is_float_ty lty then
        emit ctx "    %s =w clt%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%slt%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Gt ->
      if is_float_ty lty then
        emit ctx "    %s =w cgt%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%sgt%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Lte ->
      if is_float_ty lty then
        emit ctx "    %s =w cle%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%sle%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Gte ->
      if is_float_ty lty then
        emit ctx "    %s =w cge%s %s, %s\n" tmp op_qt lv rv
      else emit ctx "    %s =w c%sge%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.BitAnd -> emit ctx "    %s =%s and %s, %s\n" tmp qt lv rv
  | Ast.BitOr -> emit ctx "    %s =%s or %s, %s\n" tmp qt lv rv
  | Ast.BitXor -> emit ctx "    %s =%s xor %s, %s\n" tmp qt lv rv
  | Ast.Lshift -> emit ctx "    %s =%s shl %s, %s\n" tmp qt lv rv
  | Ast.Rshift ->
      let instr = if unsigned then "shr" else "sar" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | _ ->
      raise
        (Diagnostic.Errors
           [ Error.internal ~span:l.T.span "unexpected binary operator" ]));
  tmp

(* TODO(bdc9): `as` silently loses data like C. Add a safe cast that catches bad conversions at runtime. *)
and emit_cast ctx v src_ty target_ty =
  let tmp = fresh ctx in
  let src_q = qbe_ty src_ty in
  let tgt_q = qbe_ty target_ty in
  (match (src_q, tgt_q) with
  (* same QBE base type *)
  | s, t when s = t -> emit ctx "    %s =%s copy %s\n" tmp t v
  (* word -> long: sign/zero extend *)
  | "w", "l" ->
      let instr = if signedness src_ty = "u" then "extuw" else "extsw" in
      emit ctx "    %s =l %s %s\n" tmp instr v
  (* long -> word: truncate *)
  | "l", "w" -> emit ctx "    %s =w copy %s\n" tmp v
  (* single -> double *)
  | "s", "d" -> emit ctx "    %s =d exts %s\n" tmp v
  (* double -> single *)
  | "d", "s" -> emit ctx "    %s =s truncd %s\n" tmp v
  (* word -> float *)
  | "w", "s" ->
      let instr = if signedness src_ty = "u" then "uwtof" else "swtof" in
      emit ctx "    %s =s %s %s\n" tmp instr v
  | "w", "d" ->
      let instr = if signedness src_ty = "u" then "uwtof" else "swtof" in
      emit ctx "    %s =d %s %s\n" tmp instr v
  (* long -> float *)
  | "l", "s" ->
      let instr = if signedness src_ty = "u" then "ultof" else "sltof" in
      emit ctx "    %s =s %s %s\n" tmp instr v
  | "l", "d" ->
      let instr = if signedness src_ty = "u" then "ultof" else "sltof" in
      emit ctx "    %s =d %s %s\n" tmp instr v
  (* float -> word *)
  | "s", "w" ->
      let instr = if signedness target_ty = "u" then "stoui" else "stosi" in
      emit ctx "    %s =w %s %s\n" tmp instr v
  | "d", "w" ->
      let instr = if signedness target_ty = "u" then "dtoui" else "dtosi" in
      emit ctx "    %s =w %s %s\n" tmp instr v
  (* float -> long *)
  | "s", "l" ->
      let instr = if signedness target_ty = "u" then "stoui" else "stosi" in
      emit ctx "    %s =l %s %s\n" tmp instr v
  | "d", "l" ->
      let instr = if signedness target_ty = "u" then "dtoui" else "dtosi" in
      emit ctx "    %s =l %s %s\n" tmp instr v
  | _ -> emit ctx "    %s =%s copy %s\n" tmp tgt_q v);
  tmp

let rec emit_stmt (ctx : ctx) (s : T.tstmt) : unit =
  match s.T.tsdesc with
  | T.TConst (name, t, e) | T.TVar (name, t, e) ->
      (* stack slot sized by type (struct sizes resolved from context) *)
      let slot = new_slot ctx name in
      emit ctx "    %%%s =l %s %d\n" slot (alloc_instr t)
        (ty_size ctx.structs t);
      (* init runs before binding so `var x = x + 1` reads the shadowed outer x *)
      (match e.T.desc with
      | T.TZero -> emit_zero_into ctx ("%" ^ slot) e.T.ty
      | T.TUndef -> ()
      | T.TArrayLit elems ->
          let elem =
            match e.T.ty with
            | TArray (el, _) -> el
            | _ ->
                raise
                  (Diagnostic.Errors
                     [
                       Error.internal ~span:e.T.span
                         "array literal on non-array type";
                     ])
          in
          emit_array_lit_into ctx ("%" ^ slot) elems elem
      | T.TStructLit (sname, tfields) ->
          emit_struct_lit_into ctx ("%" ^ slot) sname tfields
      | _ when is_aggregate t ->
          let src = emit_expr ctx e in
          emit_aggregate_copy ctx ("%" ^ slot) src (ty_size ctx.structs t)
      | _ ->
          let v = emit_expr ctx e in
          emit ctx "    %s %s, %%%s\n" (qbe_store t) v slot);
      bind_local ctx name slot
  | T.TReturn None ->
      if not !(ctx.terminated) then emit ctx "    ret\n";
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
  (* same as for loop for while *)
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
(* | _ -> () *)

(* for i in lo..hi and for x in arr *)
and emit_for ctx name elem_ty iter body =
  let id = fresh_id ctx in
  let cond_lbl = Printf.sprintf "@for.cond%d" id in
  let body_lbl = Printf.sprintf "@for.body%d" id in
  let cont_lbl = Printf.sprintf "@for.cont%d" id in
  let end_lbl = Printf.sprintf "@for.end%d" id in
  let qt = qbe_ty elem_ty in
  let sign = signedness elem_ty in
  let slot = new_slot ctx name in
  let saved = save_locals ctx in
  let run_body () =
    ctx.loops := (cont_lbl, end_lbl) :: !(ctx.loops);
    emit_stmts ctx body;
    ctx.loops := List.tl !(ctx.loops)
  in
  (match iter.T.desc with
  | T.TRange (lo, hi) | T.TRangeInclusive (lo, hi) ->
      let inclusive =
        match iter.T.desc with T.TRangeInclusive _ -> true | _ -> false
      in
      (* loop var lives in a stack slot so the body can read and increment it *)
      emit ctx "    %%%s =l %s %d\n" slot (alloc_instr elem_ty)
        (ty_size ctx.structs elem_ty);
      let lov = emit_expr ctx lo in
      bind_local ctx name slot;
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
        match iter.T.ty with
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
            raise
              (Diagnostic.Errors
                 [
                   Error.internal ~span:iter.T.span
                     (Printf.sprintf "cannot iterate over type: %s" (show_ty t));
                 ])
      in
      let idx = Printf.sprintf "%%for.i%d" id in
      emit ctx "    %s =l alloc8 8\n" idx;
      emit ctx "    storel 0, %s\n" idx;
      (* element binding slot, refreshed each iteration *)
      emit ctx "    %%%s =l %s %d\n" slot (alloc_instr elem_ty)
        (ty_size ctx.structs elem_ty);
      bind_local ctx name slot;
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
      emit_label ctx end_lbl);
  restore_locals ctx saved
(* | _ -> () *)

and emit_stmts ctx stmts = List.iter (emit_stmt ctx) stmts

and emit_scoped ctx stmts =
  let saved = save_locals ctx in
  emit_stmts ctx stmts;
  restore_locals ctx saved

let emit_func (ctx : ctx) (tfd : T.tfunc_def) =
  (* temporaries and locals are function scoped *)
  ctx.tmp := 0;
  Hashtbl.clear ctx.locals;

  (* Use temporary names for params to spill them to stack slots *)
  let param_tmps =
    List.map
      (fun (name, t) ->
        let tmp = fresh ctx in
        (name, t, tmp))
      tfd.params
  in
  let params_strs =
    List.map
      (fun (_, t, tmp) -> Printf.sprintf "%s %s" (qbe_ty t) tmp)
      param_tmps
  in

  (* TODO(6e33): Create a custom _start. *)
  let is_main = tfd.name = "main" && tfd.ret_ty = TInt I32 in
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
    (fun (name, t, tmp) ->
      emit ctx "    %%%s =l %s %d\n" name (alloc_instr t)
        (ty_size ctx.structs t);
      (* aggregates arrive as a pointer so copy the value into the local slot *)
      if is_aggregate t then
        emit_aggregate_copy ctx ("%" ^ name) tmp (ty_size ctx.structs t)
      else emit ctx "    %s %s, %%%s\n" (qbe_store t) tmp name;
      bind_local ctx name name)
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
  match t with
  | TInt (I8 | U8) | TBool -> "b"
  | TInt (I16 | U16) -> "h"
  | TInt (I32 | U32) -> "w"
  (* null is a pointer no type but all pointers are 64-bit *)
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ ->
      "l"
  | TFloat F32 -> "s"
  | TFloat F64 -> "d"
  | TStruct sn -> ":" ^ sn
  (* QBE repeats a field type n times: { w 3 } is three words *)
  | TArray (e, n) -> Printf.sprintf "%s %d" (qbe_ext_ty e) n
  (* fat pointer stored inline as two longs *)
  | TSlice _ -> "l 2"
  | TVoid ->
      raise (Diagnostic.Errors [ Error.internal "TVoid has no extended type" ])

let emit_struct_type (ctx : ctx) (name : string) (fields : (string * ty) list) =
  let field_strs = List.map (fun (_, t) -> qbe_ext_ty t) fields in
  emit ctx "type :%s = { %s }\n" name (String.concat ", " field_strs)

type const_num = CInt of int | CFloat of float

let format_const_num (ty : ty) (n : const_num) : string =
  match n with
  | CInt n -> string_of_int n
  | CFloat f ->
      let prefix, digits =
        match ty with TFloat F32 -> ("s_", 9) | _ -> ("d_", 17)
      in
      prefix ^ Printf.sprintf "%.*g" digits f

(* undoes the s_/d_ tag format_const_num stamps on floats *)
let parse_const_num (ty : ty) (s : string) : const_num =
  match ty with
  | TFloat _ ->
      let n = String.length s in
      let s = if n > 2 && s.[1] = '_' then String.sub s 2 (n - 2) else s in
      CFloat (float_of_string s)
  | _ -> CInt (int_of_string s)

let rec fold_const_value (ctx : ctx) (te : T.texpr) : string =
  match te.T.desc with
  | T.TInt n -> string_of_int n
  | T.TBool b -> if b then "1" else "0"
  | T.TNull -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  | T.TSizeOf t -> string_of_int (ty_size ctx.structs t)
  | T.TFloat f -> format_const_num te.T.ty (CFloat f)
  | T.TCast e -> (
      match te.T.ty with
      | TInt _ | TFloat _ | TBool ->
          format_const_num te.T.ty (fold_const_num ctx te)
      | _ -> fold_const_value ctx e)
  | T.TIdent name -> resolve_const ctx name te.T.span
  | T.TCStr s ->
      let lbl = Printf.sprintf "$str%d" !(ctx.str_ctr) in
      incr ctx.str_ctr;
      ctx.strings := (lbl, s) :: !(ctx.strings);
      lbl
  | T.TBinOp _ | T.TUnOp _ -> format_const_num te.T.ty (fold_const_num ctx te)
  | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])

and unsupported_const span =
  Diagnostic.(
    error "unsupported constant expression"
    |> at span
    |> help "constant initializers must fold to a compile-time value")

and fold_const_num (ctx : ctx) (te : T.texpr) : const_num =
  match te.T.desc with
  | T.TInt n -> CInt n
  | T.TBool b -> CInt (if b then 1 else 0)
  | T.TChar c -> CInt (Char.code c)
  | T.TFloat f -> CFloat f
  | T.TSizeOf t -> CInt (ty_size ctx.structs t)
  | T.TIdent name -> parse_const_num te.T.ty (resolve_const ctx name te.T.span)
  | T.TCast e -> (
      match (te.T.ty, fold_const_num ctx e) with
      | TFloat _, CInt n -> CFloat (float_of_int n)
      | TFloat _, CFloat f -> CFloat f
      | _, CFloat f -> CInt (int_of_float f)
      | _, CInt n -> CInt n)
  | T.TUnOp (Ast.Neg, e) -> (
      match fold_const_num ctx e with
      | CInt n -> CInt (-n)
      | CFloat f -> CFloat (-.f))
  | T.TUnOp (Ast.BitNot, e) -> (
      match fold_const_num ctx e with
      | CInt n -> CInt (lnot n)
      | CFloat _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ]))
  | T.TUnOp (Ast.Not, e) -> (
      match fold_const_num ctx e with
      | CInt n -> CInt (if n = 0 then 1 else 0)
      | CFloat _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ]))
  | T.TUnOp ((Ast.Deref | Ast.AddressOf), _) ->
      raise (Diagnostic.Errors [ unsupported_const te.T.span ])
  | T.TBinOp (op, l, r) ->
      fold_const_binop te.T.span op (fold_const_num ctx l)
        (fold_const_num ctx r)
  | _ -> raise (Diagnostic.Errors [ unsupported_const te.T.span ])

and fold_const_binop (span : Ast.span) (op : Ast.binop) (a : const_num)
    (b : const_num) : const_num =
  match (a, b) with
  | CInt x, CInt y -> (
      match op with
      | Ast.Add -> CInt (x + y)
      | Ast.Sub -> CInt (x - y)
      | Ast.Mul -> CInt (x * y)
      | Ast.Div when y = 0 ->
          raise
            (Diagnostic.Errors
               [ Diagnostic.(error "division by zero in constant" |> at span) ])
      | Ast.Div -> CInt (x / y)
      | Ast.Mod when y = 0 ->
          raise
            (Diagnostic.Errors
               [ Diagnostic.(error "remainder by zero in constant" |> at span) ])
      | Ast.Mod -> CInt (x mod y)
      | Ast.BitAnd -> CInt (x land y)
      | Ast.BitOr -> CInt (x lor y)
      | Ast.BitXor -> CInt (x lxor y)
      | Ast.Lshift -> CInt (x lsl y)
      | Ast.Rshift -> CInt (x asr y)
      | Ast.Eq -> CInt (if x = y then 1 else 0)
      | Ast.Neq -> CInt (if x <> y then 1 else 0)
      | Ast.Lt -> CInt (if x < y then 1 else 0)
      | Ast.Gt -> CInt (if x > y then 1 else 0)
      | Ast.Lte -> CInt (if x <= y then 1 else 0)
      | Ast.Gte -> CInt (if x >= y then 1 else 0)
      | Ast.And -> CInt (if x <> 0 && y <> 0 then 1 else 0)
      | Ast.Or -> CInt (if x <> 0 || y <> 0 then 1 else 0)
      | Ast.Assign | Ast.AddAssign | Ast.SubAssign | Ast.MulAssign
      | Ast.DivAssign ->
          raise (Diagnostic.Errors [ unsupported_const span ]))
  | (CInt _ | CFloat _), (CInt _ | CFloat _) -> (
      let as_float = function CInt n -> float_of_int n | CFloat f -> f in
      let x, y = (as_float a, as_float b) in
      match op with
      | Ast.Add -> CFloat (x +. y)
      | Ast.Sub -> CFloat (x -. y)
      | Ast.Mul -> CFloat (x *. y)
      | Ast.Div -> CFloat (x /. y)
      | Ast.Eq -> CInt (if x = y then 1 else 0)
      | Ast.Neq -> CInt (if x <> y then 1 else 0)
      | Ast.Lt -> CInt (if x < y then 1 else 0)
      | Ast.Gt -> CInt (if x > y then 1 else 0)
      | Ast.Lte -> CInt (if x <= y then 1 else 0)
      | Ast.Gte -> CInt (if x >= y then 1 else 0)
      | _ -> raise (Diagnostic.Errors [ unsupported_const span ]))

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
      match gd.ty with
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

  (* Struct type def *)
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
      | TExtern _ | T.TStruct _ | T.TGlobal _ | T.TTypeAlias _ -> ())
    tdecls;

  (* String literals (data sections) *)
  List.iter
    (fun (lbl, content) -> emit_string_data ctx lbl content)
    (List.rev !(ctx.strings));

  Buffer.contents !(ctx.buf)
