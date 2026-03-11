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

(* TODO: band aid approach for now for adding everything up. I need to consider how structs are aligned/padding *)
let rec ty_size (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match t with
  | TInt k -> int_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TString -> 8
  | TVoid -> 0
  | TStruct name -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          List.fold_left (fun acc (_, ft) -> acc + ty_size structs ft) 0 fields
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
  | TInt (I32 | U32) -> "loadsw"
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
  | T.TBinOp (op, l, r, t) -> emit_binop ctx op l r t
  | T.TUnOp (op, e, t) -> emit_unop ctx op e t
  | T.TRange _ -> failwith "TODO: range codegen"
  | _ -> ""

and emit_unop ctx op e t =
  let ev = emit_expr ctx e in
  let qt = qbe_ty t in
  let tmp = fresh ctx in
  (match op with
  | Ast.Neg -> emit ctx "    %s =%s neg %s\n" tmp qt ev
  | _ -> failwith "Not impl");
  tmp

and emit_binop ctx op l r t =
  (* TODO: avoid evaluating lv for Assign, it emits a dead load *)
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
  | Ast.Div -> emit ctx "    %s =%s div %s, %s\n" tmp qt lv rv
  | Ast.Mod -> emit ctx "    %s =%s rem %s, %s\n" tmp qt lv rv
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
  (* x = rhs *)
  | Ast.Assign -> (
      match l with
      | T.TIdent (name, lt) when Hashtbl.mem ctx.locals name ->
          emit ctx "    %s %s, %%%s\n" (qbe_store lt) rv name;
          emit ctx "    %s =%s %s %%%s\n" tmp (qbe_ty lt) (qbe_load lt) name
      | T.TIdent (name, _) ->
          emit ctx "    %%%s =%s copy %s\n" name qt rv;
          emit ctx "    %s =%s copy %%%s\n" tmp qt name
      | _ -> emit ctx "    %s =%s copy %s\n" tmp qt rv)
  (* x += rhs -> x = x + rhs *)
  (* | Ast.AddAssign -> (
      match l with
      | T.TIdent (name, _) ->
          emit ctx "    %%%s =%s add %%%s, %s\n" name qt name rv;
          emit ctx "    %s =%s copy %%%s\n" tmp qt name
      | _ -> emit ctx "    %s =%s add %s, %s\n" tmp qt lv rv) *)
  (* TODO: I'll come back to the compound ops since I have to deal with
  field access, etc foo.bar += 1? *)
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
  | T.TExpr e ->
      let _ = emit_expr ctx e in
      ()
  | T.TBreak | T.TContinue -> () (* TODO: target labels *)
  | _ -> ()

and emit_stmts ctx stmts = List.iter (emit_stmt ctx) stmts

let emit_func (ctx : ctx) (tfd : T.tfunc_def) =
  let params_strs =
    List.map
      (fun (name, t) -> Printf.sprintf "%s %%%s" (qbe_ty t) name)
      tfd.params
  in
  (* print_endline (String.concat ", " params_strs) *)
  (* temporaries and locals are function scoped *)
  ctx.tmp := 0;
  Hashtbl.clear ctx.locals;

  (* FIXME: main(): with a trailing colon and no return type syntax error *)
  (* TODO: Create a custom _start. *)
  let is_main = tfd.name = "main" && tfd.ret_ty = TInt I32 in
  let ret_part = match tfd.ret_ty with TVoid -> "" | t -> qbe_ty t ^ " " in
  (* TODO: Add export for pub(?) *)
  emit ctx "function %s$%s(%s) {\n" ret_part tfd.name
    (String.concat ", " params_strs);
  emit ctx "@start\n";
  emit_stmts ctx tfd.body;
  let already_returns =
    match List.rev tfd.body with T.TReturn _ :: _ -> true | _ -> false
  in
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
