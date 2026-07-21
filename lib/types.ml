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
  | TStruct of string * ty list
  | TFunc of ty list * ty
  | TArray of ty * int
  | TSlice of ty
  | TNewtype of string * ty
  | TAlias of string * ty
  | TError
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

let int_kind_neg_limit = function
  | I8 -> 128L
  | I16 -> 32768L
  | I32 -> 2147483648L
  | I64 | Isize -> Int64.min_int
  | U8 | U16 | U32 | U64 | Usize -> 0L

(* single source for builtin type names used by both parsing and printing *)
let builtin_tys =
  List.map
    (fun k -> (String.lowercase_ascii (show_int_kind k), TInt k))
    int_kinds
  @ List.map
      (fun k -> (String.lowercase_ascii (show_float_kind k), TFloat k))
      float_kinds
  @ [ ("bool", TBool); ("cstr", TCStr) ]

let int_kind_of_string s =
  List.find_opt
    (fun k -> String.lowercase_ascii (show_int_kind k) = s)
    int_kinds

let rec show_ty = function
  | TInt k -> String.lowercase_ascii (show_int_kind k)
  | TFloat k -> String.lowercase_ascii (show_float_kind k)
  | TBool -> "bool"
  | TCStr -> "cstr"
  | TVoid -> "void"
  | TNull -> "null"
  | TPointer t -> "*" ^ show_ty t
  | TStruct (name, []) -> name
  | TStruct (name, args) ->
      Printf.sprintf "%s[%s]" name (String.concat ", " (List.map show_ty args))
  | TArray (t, n) -> Printf.sprintf "[%d]%s" n (show_ty t)
  | TSlice t -> "[]" ^ show_ty t
  | TNewtype (name, _) -> name
  | TAlias (name, _) -> name
  | TFunc (ps, r) ->
      let p_str = String.concat ", " (List.map show_ty ps) in
      let r_str = match r with TVoid -> "" | t -> " " ^ show_ty t in
      Printf.sprintf "(%s)%s" p_str r_str
  | TError -> "<error>"

(* sees through a newtype or alias to the concrete representation codegen must use *)
let rec resolve_ty = function
  | TNewtype (_, base) | TAlias (_, base) -> resolve_ty base
  | t -> t

let is_float t = match resolve_ty t with TFloat _ -> true | _ -> false

let is_unsigned t =
  match resolve_ty t with
  | TInt (U8 | U16 | U32 | U64 | Usize) -> true
  | _ -> false

(* byte size of each integer kind: bit width / 8 *)
let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let float_kind_size = function F32 -> 4 | F64 -> 8

let int_kind_of (t : ty) : int_kind =
  match resolve_ty t with
  | TInt k -> k
  | _ -> Error.ice "expected an integer type"

(* the value fits unless the target range can't hold every source value *)
let cast_int_needs_check (src : int_kind) (tgt : int_kind) : bool =
  let bits k = 8 * int_kind_size k in
  let src_unsigned = is_unsigned (TInt src) in
  let tgt_unsigned = is_unsigned (TInt tgt) in
  match (src_unsigned, tgt_unsigned) with
  | false, true -> true
  | true, false -> bits tgt <= bits src
  | _ -> bits tgt < bits src

(* C ABI alignment and padding rules *)
(* TODO(4287): Reordering struct fields by alignment to minimize padding  *)
(* TODO(8969): Add a packed attr to strip padding for exact memory layout *)

let rec ty_align (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid -> Error.ice "TVoid has no alignment"
  | TError -> Error.ice "TError has no alignment"
  | TStruct (name, _) -> (
      match Hashtbl.find_opt structs name with
      | Some fields ->
          List.fold_left
            (fun acc (_, ft) -> max acc (ty_align structs ft))
            1 fields
      | None ->
          Error.ice (Printf.sprintf "no layout recorded for struct %s" name))
  | TArray (e, _) -> ty_align structs e
  | TSlice _ -> 8
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* n and a must be non-negative *)
let align_to n a = (n + a - 1) / a * a

let rec ty_size (structs : (string, (string * ty) list) Hashtbl.t) (t : ty) :
    int =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TPointer _ | TNull | TCStr | TFunc _ -> 8
  | TVoid -> Error.ice "TVoid has no size"
  | TError -> Error.ice "TError has no size"
  | TStruct (name, _) -> (
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
      | None ->
          Error.ice (Printf.sprintf "no layout recorded for struct %s" name))
  | TArray (e, n) -> n * align_to (ty_size structs e) (ty_align structs e)
  (* fat pointer: { ptr, len } *)
  | TSlice _ -> 16
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* aggregates are addressed by pointer: an ident of this type is its base address *)
let is_aggregate t =
  match resolve_ty t with TArray _ | TSlice _ | TStruct _ -> true | _ -> false

(* a const is a compile-time value so only types the folder can compute are allowed *)
let is_scalar t =
  match resolve_ty t with
  | TInt _ | TFloat _ | TBool | TError -> true
  | _ -> false

(* the 8 byte value classes, so constant folding picks a 64 bit result *)
let is_wide_ty t =
  match resolve_ty t with
  | TInt (I64 | U64 | Isize | Usize) | TPointer _ | TNull | TCStr | TFunc _ ->
      true
  | _ -> false

let rec strip_alias = function TAlias (_, base) -> strip_alias base | t -> t

(* an alias is just another name for its base type so it never makes two types different *)
let rec erase_aliases = function
  | TAlias (_, base) -> erase_aliases base
  | TPointer t -> TPointer (erase_aliases t)
  | TStruct (name, args) -> TStruct (name, List.map erase_aliases args)
  | TFunc (ps, r) -> TFunc (List.map erase_aliases ps, erase_aliases r)
  | TArray (t, n) -> TArray (erase_aliases t, n)
  | TSlice t -> TSlice (erase_aliases t)
  | TNewtype (name, base) -> TNewtype (name, erase_aliases base)
  | t -> t

let ty_equal a b =
  match (erase_aliases a, erase_aliases b) with
  | TError, _ | _, TError -> true
  | x, y -> x = y
