(* SPDX-License-Identifier: Apache-2.0 *)

type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Isize | Usize
[@@deriving show { with_path = false }]

type float_kind = F32 | F64 [@@deriving show { with_path = false }]
type func_abi = Ripe | C | AbiError [@@deriving show { with_path = false }]

let func_abi_of_name = function
  | "Ripe" -> Some Ripe
  | "C" -> Some C
  | _ -> None

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
  | TOpaquePtr
  (* TODO(39ca): the ty list stays empty until generics land *)
  | TStruct of Qname.t * ty list
  | TFunc of ty list * ty * func_abi
  | TArray of ty * int
  | TSlice of ty
  | TAlias of Qname.t * ty
  | TError
  (* TODO: every enum is an i32 until a backing type can be written down *)
  | TEnum of Qname.t
  | TUnit
[@@deriving show { with_path = false }]

let int_kinds = [ I8; I16; I32; I64; U8; U16; U32; U64; Isize; Usize ]
let float_kinds = [ F32; F64 ]

let int_kind_pos_limit = function
  | I8 -> 127L
  | I16 -> 32767L
  | I32 -> 2147483647L
  | I64 | Isize -> Int64.max_int
  | U8 -> 255L
  | U16 -> 65535L
  | U32 -> 4294967295L
  | U64 | Usize -> -1L

let float_kind_exact_limit = function
  | F32 -> 16777216L
  | F64 -> 9007199254740992L

let int_kind_neg_limit = function
  | I8 -> 128L
  | I16 -> 32768L
  | I32 -> 2147483648L
  | I64 | Isize -> Int64.min_int
  | U8 | U16 | U32 | U64 | Usize -> 0L

type builtin = BTy of ty | BOpaque

let builtins =
  List.map
    (fun k -> (String.lowercase_ascii (show_int_kind k), BTy (TInt k)))
    int_kinds
  @ List.map
      (fun k -> (String.lowercase_ascii (show_float_kind k), BTy (TFloat k)))
      float_kinds
  @ [
      ("bool", BTy TBool);
      ("char", BTy TChar);
      ("cstr", BTy TCStr);
      ("str", BTy TStr);
      ("never", BTy TNever);
      ("opaque", BOpaque);
      ("int", BTy (TInt I64));
      ("float", BTy (TFloat F64));
    ]

let int_kind_of_string s =
  List.find_opt
    (fun k -> String.lowercase_ascii (show_int_kind k) = s)
    int_kinds

let float_kind_of_string s =
  List.find_opt
    (fun k -> String.lowercase_ascii (show_float_kind k) = s)
    float_kinds

let rec show_ty_with show_name t =
  let show_ty = show_ty_with show_name in
  match t with
  | TInt k -> String.lowercase_ascii (show_int_kind k)
  | TFloat k -> String.lowercase_ascii (show_float_kind k)
  | TBool -> "bool"
  | TChar -> "char"
  | TCStr -> "cstr"
  | TStr -> "str"
  | TNever -> "never"
  | TNull -> "null"
  | TPointer t -> "*" ^ show_ty t
  | TOpaquePtr -> "*opaque"
  | TStruct (name, []) -> show_name name
  | TStruct (name, args) ->
      Printf.sprintf "%s[%s]" (show_name name)
        (String.concat ", " (List.map show_ty args))
  | TArray (t, n) -> Printf.sprintf "[%d]%s" n (show_ty t)
  | TSlice t -> "[]" ^ show_ty t
  | TAlias (name, _) -> show_name name
  | TEnum name -> show_name name
  | TFunc (ps, r, _) ->
      let p_str = String.concat ", " (List.map show_ty ps) in
      let r_str = match r with TUnit -> " ()" | t -> " " ^ show_ty t in
      Printf.sprintf "func (%s)%s" p_str r_str
  | TError -> "<error>"
  | TUnit -> "()"

let show_ty t = show_ty_with Qname.show t

(* A reader inside the module a name belongs to doesn't need its path *)
let show_ty_in current t = show_ty_with (Qname.show_in current) t

let rec resolve_ty = function TAlias (_, base) -> resolve_ty base | t -> t

let is_float t = match resolve_ty t with TFloat _ -> true | _ -> false

let int_kind_unsigned = function
  | U8 | U16 | U32 | U64 | Usize -> true
  | I8 | I16 | I32 | I64 | Isize -> false

let is_unsigned t =
  match resolve_ty t with TInt k -> int_kind_unsigned k | _ -> false

(* A byte size of each integer kind: bit width / 8 *)
let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let float_kind_size = function F32 -> 4 | F64 -> 8

let int_kind_of t =
  match resolve_ty t with
  | TInt k -> k
  | _ -> Diagnostic.ice "expected an integer type"

let float_kind_of t =
  match resolve_ty t with
  | TFloat k -> k
  | _ -> Diagnostic.ice "expected a float type"

(* A narrow int divides in a wider register so its INT_MIN / -1 lands in range and gets masked back down *)
let div_int_needs_check t =
  match resolve_ty t with TInt (I32 | I64 | Isize) -> true | _ -> false

let rec ty_align (structs : ty list Symbol.Table.t) (t : ty) =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TChar -> 4
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ -> 8
  | TNever -> Diagnostic.ice "TNever has no alignment"
  | TError -> Diagnostic.ice "TError has no alignment"
  | TStruct (name, _) -> (
      match Symbol.Table.find_opt structs (Qname.key name) with
      | Some fields ->
          List.fold_left (fun acc ft -> max acc (ty_align structs ft)) 1 fields
      | None ->
          Diagnostic.ice
            (Printf.sprintf "no layout recorded for struct %s" (Qname.show name))
      )
  | TArray (e, _) -> ty_align structs e
  | TSlice _ | TStr -> 8
  | TEnum _ -> 4
  | TAlias _ -> assert false (* resolve_ty strips these *)
  | TUnit -> 1

(* `n` and `a` MUST be non-negative *)
let align_to n a = Int.cdiv n a * a

let rec ty_size (structs : ty list Symbol.Table.t) (t : ty) =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TChar -> 4
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ -> 8
  | TNever -> Diagnostic.ice "TNever has no size"
  | TError -> Diagnostic.ice "TError has no size"
  | TStruct (name, _) -> (
      match Symbol.Table.find_opt structs (Qname.key name) with
      | Some fields ->
          let offset = field_offset structs fields (List.length fields) in
          align_to offset (ty_align structs t)
      | None ->
          Diagnostic.ice
            (Printf.sprintf "no layout recorded for struct %s" (Qname.show name))
      )
  | TArray (e, n) -> n * stride structs e
  (* Fat pointer: { ptr, len } *)
  | TSlice _ | TStr -> 16
  | TEnum _ -> 4
  | TAlias _ -> assert false (* resolve_ty strips these *)
  | TUnit -> 0

(* Walking one past the last index gives where the whole thing ends *)
and field_offset (structs : ty list Symbol.Table.t) (fields : ty list)
    (index : int) =
  let rec go i off = function
    | [] ->
        if i = index then off
        else Diagnostic.ice (Printf.sprintf "unknown field index %d" index)
    | ft :: rest ->
        let off = align_to off (ty_align structs ft) in
        if i = index then off else go (i + 1) (off + ty_size structs ft) rest
  in
  go 0 0 fields

(* The number of bytes from one element to the next after round the alignment *)
and stride (structs : ty list Symbol.Table.t) (elem : ty) =
  align_to (ty_size structs elem) (ty_align structs elem)

(* Aggregates are addressed by pointer: an ident of this type is its base address *)
let is_aggregate t =
  match resolve_ty t with
  | TArray _ | TSlice _ | TStr | TStruct _ -> true
  | _ -> false

(* A const can only use types comptime evaluation knows how to compute *)
let is_scalar t =
  match resolve_ty t with
  | TInt _ | TFloat _ | TBool | TChar | TError -> true
  | _ -> false

(* Wide values use 8 bytes so comptime eval uses a 64 bit result *)
let is_wide_ty t =
  match resolve_ty t with
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ ->
      true
  | _ -> false

(* An alias is just another name for its base type so it doesn't make two types *)
let rec erase_aliases = function
  | TAlias (_, base) -> erase_aliases base
  | TPointer t -> TPointer (erase_aliases t)
  | TStruct (name, args) -> TStruct (name, List.map erase_aliases args)
  | TFunc (ps, r, abi) -> TFunc (List.map erase_aliases ps, erase_aliases r, abi)
  | TArray (t, n) -> TArray (erase_aliases t, n)
  | TSlice t -> TSlice (erase_aliases t)
  | t -> t

let ty_equal a b =
  match (erase_aliases a, erase_aliases b) with
  | TError, _ | _, TError -> true
  | x, y -> x = y
