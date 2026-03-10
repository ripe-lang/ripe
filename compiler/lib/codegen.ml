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

type ctx = {
  structs : (string, (string * ty) list) Hashtbl.t;
  buf : Buffer.t;
  strings : (string * string) list ref;
  tmp : int ref;
}

let emit ctx fmt = Printf.bprintf ctx.buf fmt

let rec emit_expr (ctx : ctx) (e : T.texpr) : string =
  match e with
  (* TODO: TString should become TCStr (null terminated) *)
  (* TSlice (fat pointer {ptr, len}) it would need a second data section *)
  | T.TString s ->
      let lbl = Printf.sprintf "$str%d" !(ctx.tmp) in
      incr ctx.tmp;
      ctx.strings := (lbl, s) :: !(ctx.strings);
      lbl
  | T.TInt (n, _) -> string_of_int n
  | _ -> ""

let rec emit_stmt (ctx : ctx) (s : T.tstmt) : unit =
  match s with
  | T.TLet (name, t, e) | T.TVar (name, t, e) ->
      let v = emit_expr ctx e in
      emit ctx "    %%%s =%s copy %s\n" name (qbe_ty t) v
  | T.TReturn None -> emit ctx "    ret\n"
  | T.TReturn (Some e) ->
      let v = emit_expr ctx e in
      emit ctx "    ret %s\n" v
  | _ -> ()

and emit_stmts ctx stmts = List.iter (emit_stmt ctx) stmts

let emit_func (ctx : ctx) (tfd : T.tfunc_def) =
  let params_strs =
    List.map
      (fun (name, t) -> Printf.sprintf "%s %%%s" (qbe_ty t) name)
      tfd.params
  in
  (* print_endline (String.concat ", " params_strs) *)
  (* reset temporaries *)
  ctx.tmp := 0;

  (* FIXME: main(): with a trailing colon and no return type syntax error *)
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
        | TInt (I32 | U32) | TBool -> "w"
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
    { structs; buf = Buffer.create 1024; strings = ref []; tmp = ref 0 }
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
