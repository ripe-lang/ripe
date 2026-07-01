(* SPDX-License-Identifier: GPL-2.0-only *)

(* https://c9x.me/compile/doc/il.html *)
open Types
module T = Typed_ast

(* TODO(7e23): I need to think about the layout/offset/padding of Structs because C++ treats empty structs as size 1. *)

let qbe_ty (t : ty) : string =
  match t with
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TBool -> "w"
  (* FIXME(d969): Null terminated strings? Idk yet. *)
  | TInt (I64 | U64) | TPointer _ | TNull | TCStr | TFunc _ -> "l"
  | TFloat F32 -> "s"
  | TFloat F64 -> "d"
  | TStruct _ -> "l"
  | TArray _ | TSlice _ -> failwith "TODO: array/slice codegen"
  | TVoid -> assert false

(* Returns "u" for unsigned integer types, "s" for signed/other *)
let signedness (t : ty) : string =
  match t with TInt (U8 | U16 | U32 | U64) -> "u" | _ -> "s"

(* byte size of each integer kind: bit width / 8 *)
let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 -> 8

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
  | TVoid -> assert false
  | TStruct name -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          List.fold_left
            (fun acc (_, ft) -> max acc (ty_align structs ft))
            1 fields
      | None -> assert false)
  | TArray _ | TSlice _ -> failwith "TODO: array/slice codegen"

(* n and a must be non-negative *)
let align_to n a = (n + a - 1) / a * a

let rec ty_size (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid -> assert false
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
      | None -> 0)
  | TArray _ | TSlice _ -> failwith "TODO: array/slice codegen"

(* TODO(1aff): maybe look into escape analysis *)
let alloc_instr (t : ty) : string =
  match t with
  | TInt (I64 | U64)
  | TFloat F64
  | TPointer _ | TNull | TCStr | TStruct _ | TFunc _ ->
      "alloc8"
  | _ -> "alloc4"

let qbe_load (t : ty) : string =
  match t with
  | TInt I8 -> "loadsb"
  | TInt U8 | TBool -> "loadub"
  | TInt I16 -> "loadsh"
  | TInt U16 -> "loaduh"
  | TInt I32 -> "loadsw"
  | TInt U32 -> "loaduw"
  | TInt (I64 | U64) | TPointer _ | TNull | TCStr | TFunc _ -> "loadl"
  | TFloat F32 -> "loads"
  | TFloat F64 -> "loadd"
  | TStruct _ -> "loadl"
  | TArray _ | TSlice _ -> failwith "TODO: array/slice codegen"
  | TVoid -> assert false

let qbe_store (t : ty) : string =
  match t with
  | TInt (I8 | U8) | TBool -> "storeb"
  | TInt (I16 | U16) -> "storeh"
  | TInt (I32 | U32) -> "storew"
  | TInt (I64 | U64) | TPointer _ | TNull | TCStr | TFunc _ -> "storel"
  | TFloat F32 -> "stores"
  | TFloat F64 -> "stored"
  | TStruct _ -> "storel"
  | TArray _ | TSlice _ -> failwith "TODO: array/slice codegen"
  | TVoid -> assert false

type ctx = {
  structs : (string, (string * ty) list) Hashtbl.t;
  locals : (string, unit) Hashtbl.t;
  globals : (string, unit) Hashtbl.t;
  buf : Buffer.t;
  strings : (string * string) list ref;
  tmp : int ref;
  str_ctr : int ref;
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

let emit ctx fmt = Printf.bprintf ctx.buf fmt

let field_offset structs fields fname =
  let rec go off = function
    | [] -> failwith ("unknown field: " ^ fname)
    | (n, ft) :: rest ->
        let a = ty_align structs ft in
        let off = align_to off a in
        if n = fname then off else go (off + ty_size structs ft) rest
  in
  go 0 fields

(* TODO(07aa): handle @(^ptr) (deref) and @s.field (struct field access) *)
let lvalue_name (e : T.texpr) : string =
  match e with T.TIdent (name, _) -> name | _ -> failwith "expected lvalue"

let rec emit_expr (ctx : ctx) (e : T.texpr) : string =
  match e with
  | T.TInt (n, _) -> string_of_int n
  | T.TFloat (f, t) ->
      let prefix, digits =
        match t with TFloat F32 -> ("s_", 9) | _ -> ("d_", 17)
      in
      prefix ^ Printf.sprintf "%.*g" digits f
  | T.TBool b -> if b then "1" else "0"
  | T.TNull _ -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  | T.TIdent (name, t) -> (
      if Hashtbl.mem ctx.locals name then (
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %%%s\n" tmp (qbe_ty t) (qbe_load t) name;
        tmp)
      else if Hashtbl.mem ctx.globals name then (
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s $%s\n" tmp (qbe_ty t) (qbe_load t) name;
        tmp)
      else match t with TFunc _ -> "$" ^ name | _ -> "%" ^ name)
  | T.TCStr s ->
      let lbl = Printf.sprintf "$str%d" !(ctx.str_ctr) in
      incr ctx.str_ctr;
      ctx.strings := (lbl, s) :: !(ctx.strings);
      lbl
  | T.TCall (name, args, ret_ty) ->
      (* recursively emit each arg (nested calls produce temps) *)
      let arg_strs =
        List.map
          (fun a ->
            Printf.sprintf "%s %s" (qbe_ty (T.ty_of_texpr a)) (emit_expr ctx a))
          args
      in
      (* local var holding a fn ptr: load then call indirectly *)
      let callee =
        if Hashtbl.mem ctx.locals name then (
          let tmp = fresh ctx in
          emit ctx "    %s =l loadl %%%s\n" tmp name;
          tmp)
        else "$" ^ name
      in
      if ret_ty = TVoid then (
        (* void: no result to capture, just emit the call *)
        emit ctx "    call %s(%s)\n" callee (String.concat ", " arg_strs);
        "")
      else
        (* non-void: capture result in a fresh temporary *)
        let tmp = fresh ctx in
        emit ctx "    %s =%s call %s(%s)\n" tmp (qbe_ty ret_ty) callee
          (String.concat ", " arg_strs);
        tmp
  | T.TBinOp (Ast.Assign, l, r, t) -> emit_assign ctx l r t
  | T.TBinOp
      ( ((Ast.AddAssign | Ast.SubAssign | Ast.MulAssign | Ast.DivAssign) as op),
        l,
        r,
        _t ) ->
      emit_compound_assign ctx op l r
  | T.TBinOp (op, l, r, t) -> emit_binop ctx op l r t
  | T.TUnOp (op, e, t) -> emit_unop ctx op e t
  | T.TCast (e, target_ty) ->
      let v = emit_expr ctx e in
      emit_cast ctx v (T.ty_of_texpr e) target_ty
  | T.TRange _ | T.TRangeInclusive _ -> failwith "TODO(41e0): range codegen"
  | T.TSizeOf t -> string_of_int (ty_size ctx.structs t)
  (* TODO(e68f): explicit deref on a struct pointer (p^.x) emits an extra loadl, fix once struct value semantics are implemented. *)
  | T.TFieldAccess (e, field, ft) ->
      let base = emit_expr ctx e in
      let rec peel = function
        | TStruct n -> n
        | TPointer t -> peel t
        | _ -> assert false
      in
      let struct_name = peel (T.ty_of_texpr e) in
      let fields = Hashtbl.find ctx.structs struct_name in
      let offset = field_offset ctx.structs fields field in
      let ptr =
        if offset = 0 then base
        else
          let p = fresh ctx in
          emit ctx "    %s =l add %s, %d\n" p base offset;
          p
      in
      let tmp = fresh ctx in
      emit ctx "    %s =%s %s %s\n" tmp (qbe_ty ft) (qbe_load ft) ptr;
      tmp
  (* TODO(c75e): codegen for string interpolation *)
  | T.TInterpString _ -> failwith "TODO(b65f): interp string codegen"
  | T.TArrayLit _ | T.TIndex _ | T.TLen _ | T.TToSlice _ | T.TSliceExpr _
  | T.TDataPtr _ | T.TZero _ ->
      failwith "TODO: array/slice codegen"

and emit_unop ctx op e t =
  match op with
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
      | _ -> assert false);
      tmp
  | Ast.AddressOf ->
      let name = lvalue_name e in
      let tmp = fresh ctx in
      if Hashtbl.mem ctx.globals name then
        emit ctx "    %s =l copy $%s\n" tmp name
      else emit ctx "    %s =l copy %%%s\n" tmp name;
      tmp

(* separated from emit_binop to stop evaluating the lhs, it emit dead loads *)
and emit_assign ctx l r _t =
  let rv = emit_expr ctx r in
  (match l with
  | T.TIdent (name, lt) when Hashtbl.mem ctx.locals name ->
      emit ctx "    %s %s, %%%s\n" (qbe_store lt) rv name
  | T.TIdent (name, lt) when Hashtbl.mem ctx.globals name ->
      emit ctx "    %s %s, $%s\n" (qbe_store lt) rv name
  | T.TIdent (name, _) ->
      emit ctx "    %%%s =%s copy %s\n" name (qbe_ty (T.ty_of_texpr r)) rv
  | _ -> ());
  rv

(* x += rhs -> x = x + rhs *)
(* x -= rhs -> x = x - rhs *)
(* x *= rhs -> x = x * rhs *)
(* x /= rhs -> x = x / rhs *)
and emit_compound_assign ctx op l r =
  let name = lvalue_name l in
  let lt = T.ty_of_texpr l in
  let qt = qbe_ty lt in
  let sign = signedness lt in
  let cur = fresh ctx in
  if Hashtbl.mem ctx.locals name then
    emit ctx "    %s =%s %s %%%s\n" cur qt (qbe_load lt) name
  else if Hashtbl.mem ctx.globals name then
    emit ctx "    %s =%s %s $%s\n" cur qt (qbe_load lt) name
  else emit ctx "    %s =%s copy %%%s\n" cur qt name;
  let rv = emit_expr ctx r in
  let arith =
    match op with
    | Ast.AddAssign -> "add"
    | Ast.SubAssign -> "sub"
    | Ast.MulAssign -> "mul"
    | Ast.DivAssign ->
        if is_float_ty lt then "div" else if sign = "u" then "udiv" else "div"
    | _ -> assert false
  in
  let new_val = fresh ctx in
  emit ctx "    %s =%s %s %s, %s\n" new_val qt arith cur rv;
  if Hashtbl.mem ctx.locals name then
    emit ctx "    %s %s, %%%s\n" (qbe_store lt) new_val name
  else if Hashtbl.mem ctx.globals name then
    emit ctx "    %s %s, $%s\n" (qbe_store lt) new_val name
  else emit ctx "    %%%s =%s copy %s\n" name qt new_val;
  new_val

and is_float_ty = function TFloat _ -> true | _ -> false

and emit_binop ctx op l r t =
  (* recursively evaluate *)
  let lv = emit_expr ctx l in
  let rv = emit_expr ctx r in
  (* type translation *)
  let qt = qbe_ty t in
  let lty = T.ty_of_texpr l in
  let op_qt = qbe_ty lty in
  let sign = signedness lty in

  let tmp = fresh ctx in
  (match op with
  | Ast.Add -> emit ctx "    %s =%s add %s, %s\n" tmp qt lv rv
  | Ast.Sub -> emit ctx "    %s =%s sub %s, %s\n" tmp qt lv rv
  | Ast.Mul -> emit ctx "    %s =%s mul %s, %s\n" tmp qt lv rv
  | Ast.Div ->
      let instr =
        if is_float_ty lty then "div" else if sign = "u" then "udiv" else "div"
      in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | Ast.Mod ->
      let instr = if sign = "u" then "urem" else "rem" in
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
  | Ast.And -> emit ctx "    %s =w and %s, %s\n" tmp lv rv
  | Ast.Or -> emit ctx "    %s =w or %s, %s\n" tmp lv rv
  | Ast.BitAnd -> emit ctx "    %s =%s and %s, %s\n" tmp qt lv rv
  | Ast.BitOr -> emit ctx "    %s =%s or %s, %s\n" tmp qt lv rv
  | Ast.BitXor -> emit ctx "    %s =%s xor %s, %s\n" tmp qt lv rv
  | Ast.Lshift -> emit ctx "    %s =%s shl %s, %s\n" tmp qt lv rv
  | Ast.Rshift ->
      let instr = if sign = "s" then "sar" else "shr" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | _ -> assert false);
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
  match s with
  | T.TConst (name, t, e) | T.TVar (name, t, e) ->
      (* stack slot sized by type (struct sizes resolved from context) *)
      emit ctx "    %%%s =l %s %d\n" name (alloc_instr t)
        (ty_size ctx.structs t);
      Hashtbl.replace ctx.locals name ();
      let v = emit_expr ctx e in
      emit ctx "    %s %s, %%%s\n" (qbe_store t) v name
  | T.TReturn None -> emit ctx "    ret\n"
  | T.TReturn (Some e) ->
      let v = emit_expr ctx e in
      emit ctx "    ret %s\n" v
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
      | [] -> emit_stmts ctx else_body
      | _ ->
          List.iteri
            (fun i (cond, body) ->
              let next_lbl =
                if i + 1 < n then List.nth cond_lbls (i + 1) else else_lbl
              in
              emit ctx "%s\n" (List.nth cond_lbls i);
              let cv = emit_expr ctx cond in
              emit ctx "    jnz %s, %s, %s\n" cv (List.nth then_lbls i) next_lbl;
              emit ctx "%s\n" (List.nth then_lbls i);
              emit_stmts ctx body;
              emit ctx "    jmp %s\n" end_lbl)
            branches;
          emit ctx "%s\n" else_lbl;
          emit_stmts ctx else_body;
          emit ctx "%s\n" end_lbl)
  | T.TBlock stmts ->
      (* TODO(e1d8): blocks as expressions e.g. let x = { 5 } *)
      (* locals is a flat hashtable, save and restore for block scope *)
      let saved = Hashtbl.copy ctx.locals in
      emit_stmts ctx stmts;
      let to_remove =
        Hashtbl.fold
          (fun k () acc -> if Hashtbl.mem saved k then acc else k :: acc)
          ctx.locals []
      in
      List.iter (Hashtbl.remove ctx.locals) to_remove
  | T.TBreak | T.TContinue -> () (* TODO(d426): target labels *)
  (* same as for loop for while *)
  | T.TWhile (cond, body) ->
      let id = fresh_id ctx in
      let test_lbl = Printf.sprintf "@while.cond%d" id in
      let body_lbl = Printf.sprintf "@while.body%d" id in
      let end_lbl = Printf.sprintf "@while.end%d" id in
      emit ctx "%s\n" test_lbl;
      let cv = emit_expr ctx cond in
      emit ctx "    jnz %s, %s, %s\n" cv body_lbl end_lbl;
      emit ctx "%s\n" body_lbl;
      emit_stmts ctx body;
      emit ctx "    jmp %s\n" test_lbl;
      emit ctx "%s\n" end_lbl
(* | _ -> () *)

(* for i in lo..hi *)
and emit_for ctx name elem_ty iter body =
  let id = fresh_id ctx in
  let cond_lbl = Printf.sprintf "@for.cond%d" id in
  let body_lbl = Printf.sprintf "@for.body%d" id in
  let cont_lbl = Printf.sprintf "@for.cont%d" id in
  let end_lbl = Printf.sprintf "@for.end%d" id in
  let qt = qbe_ty elem_ty in
  let sign = signedness elem_ty in
  match iter with
  | T.TRange (lo, hi) | T.TRangeInclusive (lo, hi) ->
      let inclusive =
        match iter with T.TRangeInclusive _ -> true | _ -> false
      in
      (* loop var lives in a stack slot so the body can read and increment it *)
      emit ctx "    %%%s =l %s %d\n" name (alloc_instr elem_ty)
        (ty_size ctx.structs elem_ty);
      Hashtbl.replace ctx.locals name ();
      let lov = emit_expr ctx lo in
      emit ctx "    %s %s, %%%s\n" (qbe_store elem_ty) lov name;
      (* upper bound is evaluated once before the loop *)
      let hiv = emit_expr ctx hi in
      emit ctx "%s\n" cond_lbl;
      let cur = fresh ctx in
      emit ctx "    %s =%s %s %%%s\n" cur qt (qbe_load elem_ty) name;
      let cmp = fresh ctx in
      let cmpop = if inclusive then "le" else "lt" in
      emit ctx "    %s =w c%s%s%s %s, %s\n" cmp sign cmpop qt cur hiv;
      emit ctx "    jnz %s, %s, %s\n" cmp body_lbl end_lbl;
      emit ctx "%s\n" body_lbl;
      emit_stmts ctx body;
      emit ctx "%s\n" cont_lbl;
      let cur2 = fresh ctx in
      emit ctx "    %s =%s %s %%%s\n" cur2 qt (qbe_load elem_ty) name;
      let nxt = fresh ctx in
      emit ctx "    %s =%s add %s, 1\n" nxt qt cur2;
      emit ctx "    %s %s, %%%s\n" (qbe_store elem_ty) nxt name;
      emit ctx "    jmp %s\n" cond_lbl;
      emit ctx "%s\n" end_lbl
  | _ -> failwith "TODO: array/slice iteration codegen"

and emit_stmts ctx stmts = List.iter (emit_stmt ctx) stmts

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
  emit ctx "@start\n";

  (* Spill params to stack slots so they can be reassigned *)
  List.iter
    (fun (name, t, tmp) ->
      emit ctx "    %%%s =l %s %d\n" name (alloc_instr t)
        (ty_size ctx.structs t);
      emit ctx "    %s %s, %%%s\n" (qbe_store t) tmp name;
      Hashtbl.replace ctx.locals name ())
    param_tmps;

  emit_stmts ctx tfd.body;
  let already_returns =
    match List.rev tfd.body with T.TReturn _ :: _ -> true | _ -> false
  in
  (* The ret ends the last block *)
  (* TODO(978e): Emit implicit return for non-void functions where the last expression is the return value. *)
  if not already_returns then
    if is_main then emit ctx "    ret 0\n"
    else if tfd.ret_ty = TVoid then emit ctx "    ret\n";
  (* TODO(aa3a): error in typechecker for non-void functions missing a return on all paths *)
  emit ctx "}\n\n"

let qbe_ext_ty (t : ty) : string =
  match t with
  | TInt (I8 | U8) | TBool -> "b"
  | TInt (I16 | U16) -> "h"
  | TInt (I32 | U32) -> "w"
  (* null is a pointer no type but all pointers are 64-bit *)
  | TInt (I64 | U64) | TPointer _ | TNull | TCStr | TFunc _ -> "l"
  | TFloat F32 -> "s"
  | TFloat F64 -> "d"
  | TStruct sn -> ":" ^ sn
  | TArray _ | TSlice _ -> failwith "TODO: array/slice codegen"
  (* its nothing. like actually nothing. *)
  | TVoid -> assert false

let emit_struct_type (ctx : ctx) (name : string) (fields : (string * ty) list) =
  let field_strs = List.map (fun (_, t) -> qbe_ext_ty t) fields in
  emit ctx "type :%s = { %s }\n" name (String.concat ", " field_strs)

(* TODO(ab17): fold arithmetic *)
let rec fold_const_value (ctx : ctx) (te : T.texpr) : string =
  match te with
  | T.TInt (n, _) -> string_of_int n
  | T.TBool b -> if b then "1" else "0"
  | T.TNull _ -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  | T.TSizeOf t -> string_of_int (ty_size ctx.structs t)
  | T.TFloat (f, t) ->
      let prefix, digits =
        match t with TFloat F32 -> ("s_", 9) | _ -> ("d_", 17)
      in
      prefix ^ Printf.sprintf "%.*g" digits f
  | T.TCast (e, _) -> fold_const_value ctx e
  | T.TIdent (name, _) -> "$" ^ name
  | _ -> failwith "non-trivial constant initializer"

let emit_global_data (ctx : ctx) (gd : T.tglobal_def) =
  let align = ty_align ctx.structs gd.ty in
  match gd.init with
  | None ->
      let size = ty_size ctx.structs gd.ty in
      emit ctx "data $%s = align %d { z %d }\n" gd.name align size
  | Some te ->
      let letter = qbe_ext_ty gd.ty in
      let value = fold_const_value ctx te in
      emit ctx "data $%s = align %d { %s %s }\n" gd.name align letter value

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
      buf = Buffer.create 1024;
      strings = ref [];
      tmp = ref 0;
      str_ctr = ref 0;
    }
  in

  List.iter
    (function
      | T.TGlobal gd -> Hashtbl.replace ctx.globals gd.name () | _ -> ())
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

  Buffer.contents ctx.buf
