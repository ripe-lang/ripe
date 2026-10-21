(* SPDX-License-Identifier: Apache-2.0 *)

type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Isize | Usize
[@@deriving show { with_path = false }]

let int_kinds = [ I8; I16; I32; I64; U8; U16; U32; U64; Isize; Usize ]

let int_kind_of_string s =
  List.find_opt
    (fun k -> String.lowercase_ascii (show_int_kind k) = s)
    int_kinds

let int_kind_is_unsigned = function
  | U8 | U16 | U32 | U64 | Usize -> true
  | I8 | I16 | I32 | I64 | Isize -> false

let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let int_kind_pos_limit = function
  | I8 -> 127L
  | I16 -> 32767L
  | I32 -> 2147483647L
  | I64 | Isize -> Int64.max_int
  | U8 -> 255L
  | U16 -> 65535L
  | U32 -> 4294967295L
  | U64 | Usize -> -1L

let int_kind_neg_limit = function
  | I8 -> 128L
  | I16 -> 32768L
  | I32 -> 2147483648L
  | I64 | Isize -> Int64.min_int
  | U8 | U16 | U32 | U64 | Usize -> 0L

let int_kind_fits kind neg magnitude =
  let limit =
    if neg then int_kind_neg_limit kind else int_kind_pos_limit kind
  in
  Int64.unsigned_compare magnitude limit <= 0

type float_kind = F32 | F64 [@@deriving show { with_path = false }]

let float_kinds = [ F32; F64 ]

let float_kind_of_string s =
  List.find_opt
    (fun k -> String.lowercase_ascii (show_float_kind k) = s)
    float_kinds

let float_kind_size = function F32 -> 4 | F64 -> 8

let float_kind_exact_limit = function
  | F32 -> 16777216L
  | F64 -> 9007199254740992L

type func_abi = Ripe | C | AbiError [@@deriving show { with_path = false }]
type func_arity = Fixed | Variadic [@@deriving show { with_path = false }]
type adt_kind = Struct | Enum [@@deriving show { with_path = false }]

let func_abi_of_string = function
  | "Ripe" -> Some Ripe
  | "C" -> Some C
  | _ -> None

(* TODO(39ca): the ty list stays empty until generics land *)
type ty =
  | TInt of int_kind
  | TFloat of float_kind
  | TBool
  | TChar
  | TCStr
  | TStr
  | TNever
  | TNull
  | TPointer of ty
  | TPtr
  | TAdt of Keyname.t * ty list
  | TFunc of func_ty
  | TArray of ty * int
  | TSlice of ty
  | TAlias of Keyname.t * ty
  | TError
  | TUnit
  | TType

and func_ty = {
  sig_params : ty list;
  sig_ret : ty;
  sig_abi : func_abi;
  sig_arity : func_arity;
}
[@@deriving show { with_path = false }]

let builtins =
  List.map
    (fun k -> (String.lowercase_ascii (show_int_kind k), TInt k))
    int_kinds
  @ List.map
      (fun k -> (String.lowercase_ascii (show_float_kind k), TFloat k))
      float_kinds
  @ [
      ("bool", TBool);
      ("char", TChar);
      ("cstr", TCStr);
      ("str", TStr);
      ("never", TNever);
      ("ptr", TPtr);
      ("type", TType);
    ]

let rec show_ty t =
  let tok = Tokens.show_token in
  let list ts = String.concat (tok COMMA ^ " ") ts in
  match t with
  | TInt _ | TFloat _ | TBool | TChar | TCStr | TStr | TNever | TPtr | TType ->
      fst (List.find (fun (_, b) -> b = t) builtins)
  | TNull -> tok NULL
  | TPointer t -> tok STAR ^ show_ty t
  | TAdt (name, []) | TAlias (name, _) -> Keyname.show name
  | TAdt (name, args) ->
      Keyname.show name ^ tok LBRACKET
      ^ list (List.map show_ty args)
      ^ tok RBRACKET
  | TArray (t, n) -> tok LBRACKET ^ string_of_int n ^ tok RBRACKET ^ show_ty t
  | TSlice t -> tok LBRACKET ^ tok RBRACKET ^ show_ty t
  | TFunc f ->
      let rest =
        match f.sig_arity with Fixed -> [] | Variadic -> [ tok DOTDOT ]
      in
      (* The ABI is part of the type so two funcs must not read the same *)
      let abi =
        match f.sig_abi with
        | Ripe -> ""
        | C -> tok EXTERN ^ " \"C\" "
        | AbiError -> tok EXTERN ^ " "
      in
      abi ^ tok LPAREN
      ^ list (List.map show_ty f.sig_params @ rest)
      ^ tok RPAREN ^ " " ^ tok ARROW ^ " " ^ show_ty f.sig_ret
  | TError -> "<unknown type>"
  | TUnit -> tok LPAREN ^ tok RPAREN

let rec resolve_ty = function TAlias (_, base) -> resolve_ty base | t -> t

let rec has_error = function
  | TError -> true
  | TAlias (_, t) | TPointer t | TSlice t | TArray (t, _) -> has_error t
  | TAdt (_, args) -> List.exists has_error args
  | TFunc { sig_params; sig_ret; _ } ->
      List.exists has_error sig_params || has_error sig_ret
  | _ -> false

(* An alias is just another name for its base type so it doesn't make two types *)
let rec same_shape a b =
  match (a, b) with
  | TAlias (_, a), b | a, TAlias (_, b) -> same_shape a b
  | TPointer a, TPointer b | TSlice a, TSlice b -> same_shape a b
  | TArray (a, n), TArray (b, m) -> n = m && same_shape a b
  | TAdt (x, xs), TAdt (y, ys) -> x = y && List.equal same_shape xs ys
  | TFunc f, TFunc g ->
      f.sig_abi = g.sig_abi && f.sig_arity = g.sig_arity
      && List.equal same_shape f.sig_params g.sig_params
      && same_shape f.sig_ret g.sig_ret
  | a, b -> a = b

let ty_equal a b = has_error a || has_error b || same_shape a b
let is_float t = match resolve_ty t with TFloat _ -> true | _ -> false

let is_unsigned t =
  match resolve_ty t with TInt k -> int_kind_is_unsigned k | _ -> false

let is_wide t =
  match resolve_ty t with
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TPtr | TNull | TCStr | TFunc _ ->
      true
  | _ -> false

let is_never t = resolve_ty t = TNever
let is_unit t = resolve_ty t = TUnit

(* A narrow int divides in a wider register so its INT_MIN / -1 lands in range and gets masked back down *)
let div_int_needs_check t =
  match resolve_ty t with TInt (I32 | I64 | Isize) -> true | _ -> false
