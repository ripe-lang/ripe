(* SPDX-License-Identifier: GPL-2.0-only *)

type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Isize | Usize
[@@deriving show { with_path = false }]

type float_kind = F32 | F64 [@@deriving show { with_path = false }]

type ty =
  | TInt of int_kind
  | TFloat of float_kind
  | TBool
  | TCStr
  | TVoid
  | TNull
  | TPointer of ty
  | TStruct of string
  | TFunc of ty list * ty
  | TArray of ty * int
  | TSlice of ty
[@@deriving show { with_path = false }]

let int_kinds = [ I8; I16; I32; I64; U8; U16; U32; U64; Isize; Usize ]
let float_kinds = [ F32; F64 ]

(* single source for builtin type names used by both parsing and printing *)
let builtin_tys =
  List.map
    (fun k -> (String.lowercase_ascii (show_int_kind k), TInt k))
    int_kinds
  @ List.map
      (fun k -> (String.lowercase_ascii (show_float_kind k), TFloat k))
      float_kinds
  @ [ ("bool", TBool); ("cstr", TCStr) ]

let rec show_ty = function
  | TInt k -> String.lowercase_ascii (show_int_kind k)
  | TFloat k -> String.lowercase_ascii (show_float_kind k)
  | TBool -> "bool"
  | TCStr -> "cstr"
  | TVoid -> "void"
  | TNull -> "null"
  | TPointer t -> "*" ^ show_ty t
  | TStruct name -> name
  | TArray (t, n) -> Printf.sprintf "[%d]%s" n (show_ty t)
  | TSlice t -> "[]" ^ show_ty t
  | TFunc (ps, r) ->
      let p_str = String.concat ", " (List.map show_ty ps) in
      let r_str = match r with TVoid -> "" | t -> " " ^ show_ty t in
      Printf.sprintf "(%s)%s" p_str r_str
