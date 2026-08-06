(* SPDX-License-Identifier: GPL-2.0-only *)

type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Isize | Usize
[@@deriving show { with_path = false }]

type float_kind = F32 | F64 [@@deriving show { with_path = false }]

type ty =
  | TInt of int_kind
  | TFloat of float_kind
  | TBool
  | TChar
  | TCStr
  | TVoid
  | TNever
  | TNull
  | TPointer of ty
  | TOpaquePtr
  (* TODO(39ca): the ty list stays empty until generics land *)
  | TStruct of Qname.t * ty list
  | TFunc of ty list * ty
  | TArray of ty * int
  | TSlice of ty
  | TNewtype of Qname.t * ty
  | TAlias of Qname.t * ty
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

type builtin = BTy of ty | BOpaque

let builtins : (string * builtin) list =
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
      ("never", BTy TNever);
      ("opaque", BOpaque);
    ]

let int_kind_of_string s =
  List.find_opt
    (fun k -> String.lowercase_ascii (show_int_kind k) = s)
    int_kinds

let rec show_ty_with (show_name : Qname.t -> string) (t : ty) : string =
  let show_ty = show_ty_with show_name in
  match t with
  | TInt k -> String.lowercase_ascii (show_int_kind k)
  | TFloat k -> String.lowercase_ascii (show_float_kind k)
  | TBool -> "bool"
  | TChar -> "char"
  | TCStr -> "cstr"
  | TVoid -> "void"
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
  | TNewtype (name, _) -> show_name name
  | TAlias (name, _) -> show_name name
  | TFunc (ps, r) ->
      let p_str = String.concat ", " (List.map show_ty ps) in
      let r_str = match r with TVoid -> "" | t -> " " ^ show_ty t in
      Printf.sprintf "(%s)%s" p_str r_str
  | TError -> "<error>"

let show_ty (t : ty) : string = show_ty_with Qname.show t

(* A reader inside the module a name belongs to doesn't need its path *)
let show_ty_in (current : string list) (t : ty) : string =
  show_ty_with (Qname.show_in current) t

(* Sees through a newtype or alias to a representation tat the codegen can use *)
let rec resolve_ty = function
  | TNewtype (_, base) | TAlias (_, base) -> resolve_ty base
  | t -> t

let is_float t =
  match resolve_ty t with
  | TFloat _ -> true
  | TInt _ | TBool | TChar | TCStr | TVoid | TNever | TNull | TPointer _
  | TOpaquePtr | TStruct _ | TFunc _ | TArray _ | TSlice _ | TNewtype _
  | TAlias _ | TError ->
      false

let is_unsigned t =
  match resolve_ty t with
  | TInt (U8 | U16 | U32 | U64 | Usize) -> true
  | TInt (I8 | I16 | I32 | I64 | Isize)
  | TFloat _ | TBool | TChar | TCStr | TVoid | TNever | TNull | TPointer _
  | TOpaquePtr | TStruct _ | TFunc _ | TArray _ | TSlice _ | TNewtype _
  | TAlias _ | TError ->
      false

(* A byte size of each integer kind: bit width / 8 *)
let int_kind_size = function
  | I8 | U8 -> 1
  | I16 | U16 -> 2
  | I32 | U32 -> 4
  | I64 | U64 | Isize | Usize -> 8

let float_kind_size = function F32 -> 4 | F64 -> 8

let int_kind_of (t : ty) : int_kind =
  match resolve_ty t with
  | TInt k -> k
  | TFloat _ | TBool | TChar | TCStr | TVoid | TNever | TNull | TPointer _
  | TOpaquePtr | TStruct _ | TFunc _ | TArray _ | TSlice _ | TNewtype _
  | TAlias _ | TError ->
      Diagnostic.ice "expected an integer type"

(* The value fits UNLESS the target range can't hold every source value *)
let cast_int_needs_check (src : int_kind) (tgt : int_kind) : bool =
  let bits k = 8 * int_kind_size k in
  let src_unsigned = is_unsigned (TInt src) in
  let tgt_unsigned = is_unsigned (TInt tgt) in
  match (src_unsigned, tgt_unsigned) with
  | false, true -> true
  | true, false -> bits tgt <= bits src
  | _ -> bits tgt < bits src

let rec ty_align (structs : (Symbol.key, ty list) Hashtbl.t) (t : ty) : int =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TChar -> 4
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ -> 8
  | TVoid -> Diagnostic.ice "TVoid has no alignment"
  | TNever -> Diagnostic.ice "TNever has no alignment"
  | TError -> Diagnostic.ice "TError has no alignment"
  | TStruct (name, _) -> (
      match Hashtbl.find_opt structs (Qname.key name) with
      | Some fields ->
          List.fold_left (fun acc ft -> max acc (ty_align structs ft)) 1 fields
      | None ->
          Diagnostic.ice
            (Printf.sprintf "no layout recorded for struct %s" (Qname.show name))
      )
  | TArray (e, _) -> ty_align structs e
  | TSlice _ -> 8
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* `n` and `a` MUST be non-negative *)
let align_to n a = (n + a - 1) / a * a

let rec ty_size (structs : (Symbol.key, ty list) Hashtbl.t) (t : ty) : int =
  match resolve_ty t with
  | TInt k -> int_kind_size k
  | TFloat k -> float_kind_size k
  | TBool -> 1
  | TChar -> 4
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ -> 8
  | TVoid -> Diagnostic.ice "TVoid has no size"
  | TNever -> Diagnostic.ice "TNever has no size"
  | TError -> Diagnostic.ice "TError has no size"
  | TStruct (name, _) -> (
      match Hashtbl.find_opt structs (Qname.key name) with
      | Some fields ->
          let struct_align = ty_align structs t in
          let offset =
            List.fold_left
              (fun off ft ->
                let a = ty_align structs ft in
                let off = align_to off a in
                off + ty_size structs ft)
              0 fields
          in
          align_to offset struct_align
      | None ->
          Diagnostic.ice
            (Printf.sprintf "no layout recorded for struct %s" (Qname.show name))
      )
  | TArray (e, n) -> n * align_to (ty_size structs e) (ty_align structs e)
  (* Fat pointer: { ptr, len } *)
  | TSlice _ -> 16
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* Aggregates are addressed by pointer: an ident of this type is its base address *)
let is_aggregate t =
  match resolve_ty t with
  | TArray _ | TSlice _ | TStruct _ -> true
  | TInt _ | TFloat _ | TBool | TChar | TCStr | TVoid | TNever | TNull
  | TPointer _ | TOpaquePtr | TFunc _ | TNewtype _ | TAlias _ | TError ->
      false

(* A const can only use types the folder knows how to compute *)
let is_scalar t =
  match resolve_ty t with
  | TInt _ | TFloat _ | TBool | TChar | TError -> true
  | TCStr | TVoid | TNever | TNull | TPointer _ | TOpaquePtr | TStruct _
  | TFunc _ | TArray _ | TSlice _ | TNewtype _ | TAlias _ ->
      false

(* Wide values use 8 bytes so constant folding uses a 64 bit result *)
let is_wide_ty t =
  match resolve_ty t with
  | TInt (I64 | U64 | Isize | Usize)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ ->
      true
  | TInt (I8 | I16 | I32 | U8 | U16 | U32)
  | TFloat _ | TBool | TChar | TVoid | TNever | TStruct _ | TArray _ | TSlice _
  | TNewtype _ | TAlias _ | TError ->
      false

let rec strip_alias = function TAlias (_, base) -> strip_alias base | t -> t

(* An alias is just another name for its base type so it doesn't make two types *)
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
