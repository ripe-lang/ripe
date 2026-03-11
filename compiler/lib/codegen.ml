(* SPDX-License-Identifier: GPL-2.0-only *)

(* https://c9x.me/compile/doc/il.html *)
open Types
module T = Typed_ast

(* TODO: I need to think about the layout/offset/padding of Structs because C++ treats empty structs as size 1. *)

let qbe_ty (t : ty) : string =
  match t with
  | TInt (I8 | I16 | I32 | U8 | U16 | U32) | TBool -> "w"
  (* FIXME: Null terminated strings? Idk yet. *)
  | TInt (I64 | U64) | TPointer _ | TNull | TString -> "l"
  (* TODO: Add float *)
  (* | TFloat -> "s"  32-bit float *)
  (* | TDouble -> "d" 64-bit float *)
  | TStruct name -> ":" ^ name
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

(* C ABI alignment and padding rules *)
(* TODO: Reordering struct fields by alignment to minimize padding  *)
(* TODO: Add a packed attr to strip padding for exact memory layout *)
let rec ty_align (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match t with
  | TInt k -> int_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TString -> 8
  | TVoid -> assert false
  | TStruct name -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          List.fold_left
            (fun acc (_, ft) -> max acc (ty_align structs ft))
            1 fields
      | None -> assert false)

(* n and a must be non-negative *)
let align_to n a = (n + a - 1) / a * a

let rec ty_size (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match t with
  | TInt k -> int_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TString -> 8
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

(* TODO: maybe look into escape analysis *)
let alloc_instr (t : ty) : string =
  match t with
  | TInt (I64 | U64) | TPointer _ | TNull | TString | TStruct _ -> "alloc8"
  | _ -> "alloc4"

let qbe_load (t : ty) : string =
  match t with
  | TInt I8 -> "loadsb"
  | TInt U8 | TBool -> "loadub"
  | TInt I16 -> "loadsh"
  | TInt U16 -> "loaduh"
  | TInt I32 -> "loadsw"
  | TInt U32 -> "loaduw"
  | TInt (I64 | U64) | TPointer _ | TNull | TString -> "loadl"
  | TStruct _ | TVoid -> assert false

let qbe_store (t : ty) : string =
  match t with
  | TInt (I8 | U8) | TBool -> "storeb"
  | TInt (I16 | U16) -> "storeh"
  | TInt (I32 | U32) -> "storew"
  | TInt (I64 | U64) | TPointer _ | TNull | TString -> "storel"
  | TStruct _ | TVoid -> assert false

type ctx = {
  structs : (string, (string * ty) list) Hashtbl.t;
  locals : (string, unit) Hashtbl.t;
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

let rec emit_expr (ctx : ctx) (e : T.texpr) : string =
  match e with
  | T.TInt (n, _) -> string_of_int n
  | T.TBool b -> if b then "1" else "0"
  | T.TNull _ -> "0"
  | T.TChar c -> string_of_int (Char.code c)
  | T.TIdent (name, t) ->
      if Hashtbl.mem ctx.locals name then (
        let tmp = fresh ctx in
        emit ctx "    %s =%s %s %%%s\n" tmp (qbe_ty t) (qbe_load t) name;
        tmp)
      else "%" ^ name
  (* TODO: TString should become TCStr (null terminated) *)
  (* TSlice (fat pointer {ptr, len}) it would need a second data section *)
  | T.TString s ->
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
      if ret_ty = TVoid then (
        (* void: no result to capture, just emit the call *)
        emit ctx "    call $%s(%s)\n" name (String.concat ", " arg_strs);
        "")
      else
        (* non-void: capture result in a fresh temporary *)
        let tmp = fresh ctx in
        emit ctx "    %s =%s call $%s(%s)\n" tmp (qbe_ty ret_ty) name
          (String.concat ", " arg_strs);
        tmp
  | T.TBinOp (Ast.Assign, l, r, t) -> emit_assign ctx l r t
  | T.TBinOp (op, l, r, t) -> emit_binop ctx op l r t
  | T.TUnOp (op, e, t) -> emit_unop ctx op e t
  | T.TRange _ -> failwith "TODO: range codegen"
  | T.TSizeOf t -> string_of_int (ty_size ctx.structs t)
  | _ -> ""

and emit_unop ctx op e t =
  let ev = emit_expr ctx e in
  let qt = qbe_ty t in
  let tmp = fresh ctx in
  (match op with
  | Ast.Neg -> emit ctx "    %s =%s neg %s\n" tmp qt ev
  | _ -> failwith "Not impl");
  tmp

(* separated from emit_binop to stop evaluating the lhs, it emit dead loads *)
and emit_assign ctx l r _t =
  let rv = emit_expr ctx r in
  (match l with
  | T.TIdent (name, lt) when Hashtbl.mem ctx.locals name ->
      emit ctx "    %s %s, %%%s\n" (qbe_store lt) rv name
  | T.TIdent (name, _) ->
      emit ctx "    %%%s =%s copy %s\n" name (qbe_ty (T.ty_of_texpr r)) rv
  | _ -> ());
  rv

and emit_binop ctx op l r t =
  (* recursively evaluate *)
  let lv = emit_expr ctx l in
  let rv = emit_expr ctx r in
  (* type translation *)
  let qt = qbe_ty t in
  let op_qt = qbe_ty (T.ty_of_texpr l) in
  let sign = signedness (T.ty_of_texpr l) in

  let tmp = fresh ctx in
  (match op with
  | Ast.Add -> emit ctx "    %s =%s add %s, %s\n" tmp qt lv rv
  | Ast.Sub -> emit ctx "    %s =%s sub %s, %s\n" tmp qt lv rv
  | Ast.Mul -> emit ctx "    %s =%s mul %s, %s\n" tmp qt lv rv
  | Ast.Div ->
      let instr = if sign = "u" then "udiv" else "div" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | Ast.Mod ->
      let instr = if sign = "u" then "urem" else "rem" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | Ast.Eq -> emit ctx "    %s =w ceq%s %s, %s\n" tmp op_qt lv rv
  | Ast.Neq -> emit ctx "    %s =w cne%s %s, %s\n" tmp op_qt lv rv
  | Ast.Lt -> emit ctx "    %s =w c%slt%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Gt -> emit ctx "    %s =w c%sgt%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Lte -> emit ctx "    %s =w c%sle%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.Gte -> emit ctx "    %s =w c%sge%s %s, %s\n" tmp sign op_qt lv rv
  | Ast.And -> emit ctx "    %s =w and %s, %s\n" tmp lv rv
  | Ast.Or -> emit ctx "    %s =w or %s, %s\n" tmp lv rv
  | Ast.BitAnd -> emit ctx "    %s =%s and %s, %s\n" tmp qt lv rv
  | Ast.BitOr -> emit ctx "    %s =%s or %s, %s\n" tmp qt lv rv
  | Ast.BitXor -> emit ctx "    %s =%s xor %s, %s\n" tmp qt lv rv
  | Ast.Lshift -> emit ctx "    %s =%s shl %s, %s\n" tmp qt lv rv
  | Ast.Rshift ->
      let instr = if sign = "s" then "sar" else "shr" in
      emit ctx "    %s =%s %s %s, %s\n" tmp qt instr lv rv
  | _ -> failwith "Not impl");
  tmp

let rec emit_stmt (ctx : ctx) (s : T.tstmt) : unit =
  match s with
  | T.TLet (name, t, e) | T.TVar (name, t, e) ->
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
  (* TODO: emit_expr in statement position emits dead loads for idents *)
  | T.TExpr e ->
      let _ = emit_expr ctx e in
      ()
  | T.TFor (name, elem_ty, _iter, body) ->
      (* TODO: proper range iteration *)
      let _ = name in
      let _ = elem_ty in
      emit_stmts ctx body
  | T.TIf (branches, else_body) -> (
      let id = fresh_id ctx in
      let n = List.length branches in
      let cond_lbls = List.init n (fun i -> Printf.sprintf "@if.cond%d_%d" id i) in
      let then_lbls = List.init n (fun i -> Printf.sprintf "@if.then%d_%d" id i) in
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
  | T.TBreak | T.TContinue -> () (* TODO: target labels *)
  | T.TCFor (init, cond, post, body) ->
      let id = fresh_id ctx in
      let test_lbl = Printf.sprintf "@for.cond%d" id in
      let body_lbl = Printf.sprintf "@for.body%d" id in
      let end_lbl = Printf.sprintf "@for.end%d" id in
      emit_stmt ctx init;
      emit ctx "%s\n" test_lbl;
      let cv = emit_expr ctx cond in
      emit ctx "    jnz %s, %s, %s\n" cv body_lbl end_lbl;
      emit ctx "%s\n" body_lbl;
      emit_stmts ctx body;
      let _ = emit_expr ctx post in
      ();
      emit ctx "    jmp %s\n" test_lbl;
      emit ctx "%s\n" end_lbl
  | _ -> ()

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

  (* FIXME: main(): with a trailing colon and no return type syntax error *)
  (* TODO: Create a custom _start. *)
  let is_main = tfd.name = "main" && tfd.ret_ty = TInt I32 in
  let ret_part = match tfd.ret_ty with TVoid -> "" | t -> qbe_ty t ^ " " in
  (* TODO: Add export for pub(?) *)
  emit ctx "function %s$%s(%s) {\n" ret_part tfd.name
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
  (* TODO: Emit implicit return for non-void functions where the last expression is the return value. *)
  if not already_returns then
    if is_main then emit ctx "    ret 0\n"
    else if tfd.ret_ty = TVoid then emit ctx "    ret\n";
  emit ctx "}\n\n"

let emit_struct_type (ctx : ctx) (name : string) (fields : (string * ty) list) =
  let field_strs =
    List.map
      (fun (_, t) ->
        match t with
        | TInt (I8 | U8) -> "b"
        | TInt (I16 | U16) -> "h"
        | TInt (I32 | U32) -> "w"
        | TBool -> "b"
        (* null is a pointer no type but all pointers are  64-bit *)
        | TInt (I64 | U64) | TPointer _ | TNull | TString -> "l"
        | TStruct sn -> ":" ^ sn
        (* its nothing. like actually nothing. *)
        | TVoid -> assert false)
      fields
  in
  emit ctx "type :%s = { %s }\n" name (String.concat ", " field_strs)

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
      | T.TStruct (name, fields) -> Hashtbl.replace structs name fields
      | _ -> ())
    tdecls;

  let ctx =
    {
      structs;
      locals = Hashtbl.create 16;
      buf = Buffer.create 1024;
      strings = ref [];
      tmp = ref 0;
      str_ctr = ref 0;
    }
  in

  (* Struct type def *)
  List.iter
    (function
      | T.TStruct (name, fields) -> emit_struct_type ctx name fields | _ -> ())
    tdecls;
  (* new line after struct(s) for clean emit output *)
  let has_structs =
    List.exists (function T.TStruct _ -> true | _ -> false) tdecls
  in
  (* No benefit only format *)
  if has_structs then emit ctx "\n";

  (* TODO: function defs (externs no body)  *)
  List.iter
    (function
      | T.TFunc tfd -> emit_func ctx tfd | TExtern _ | T.TStruct _ -> ())
    tdecls;

  (* String literals (data sections) *)
  List.iter
    (fun (lbl, content) -> emit_string_data ctx lbl content)
    (List.rev !(ctx.strings));

  Buffer.contents ctx.buf
