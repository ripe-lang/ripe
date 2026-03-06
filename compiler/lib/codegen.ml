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

type ctx = { structs : (string, (string * ty) list) Hashtbl.t }

let emit_qbe (tdecls : T.tdecl list) : string =
  (* Collect struct layouts for offset comp *)
  let structs = Hashtbl.create 8 in
  List.iter
    (function
      | T.TStruct (name, fields) -> Hashtbl.replace structs name fields
      | _ -> ())
    tdecls;

  let _ctx = { structs } in

  Hashtbl.iter
    (fun name fields ->
      Printf.printf "struct %s: [%s]\n" name
        (String.concat ", " (List.map (fun (f, _) -> f) fields)))
    structs;

  ignore tdecls;
  ""
