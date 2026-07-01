(* SPDX-License-Identifier: GPL-2.0-only *)

type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64
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

let rec show_ty = function
  | TInt I8 -> "i8"
  | TInt I16 -> "i16"
  | TInt I32 -> "i32"
  | TInt I64 -> "i64"
  | TInt U8 -> "u8"
  | TInt U16 -> "u16"
  | TInt U32 -> "u32"
  | TInt U64 -> "u64"
  | TFloat F32 -> "f32"
  | TFloat F64 -> "f64"
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
