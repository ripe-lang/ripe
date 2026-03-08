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

type ctx = { structs : (string, (string * ty) list) Hashtbl.t; buf : Buffer.t }

let emit ctx fmt = Printf.bprintf ctx.buf fmt

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

let emit_qbe (tdecls : T.tdecl list) : string =
  (* Collect struct layouts for offset comp *)
  let structs = Hashtbl.create 8 in
  List.iter
    (function
      | T.TStruct (name, fields) -> Hashtbl.replace structs name fields
      | _ -> ())
    tdecls;

  let ctx = { structs; buf = Buffer.create 1024 } in

  (* Struct type def *)
  List.iter
    (function
      | T.TStruct (name, fields) -> emit_struct_type ctx name fields | _ -> ())
    tdecls;
  Buffer.contents ctx.buf
