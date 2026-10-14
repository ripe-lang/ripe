(* SPDX-License-Identifier: Apache-2.0 *)

type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64 | Isize | Usize
type float_kind = F32 | F64
type func_abi = Ripe | C | AbiError

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
  | TStruct of Keyname.t * ty list
  | TFunc of ty list * ty * func_abi
  | TArray of ty * int
  | TSlice of ty
  | TAlias of Keyname.t * ty
  | TError
  | TEnum of Keyname.t
  | TUnit

val show_int_kind : int_kind -> string
val int_kinds : int_kind list
val int_kind_of_string : string -> int_kind option
val int_kind_is_unsigned : int_kind -> bool
val int_kind_size : int_kind -> int
val int_kind_pos_limit : int_kind -> int64
val int_kind_neg_limit : int_kind -> int64
val int_kind_fits : int_kind -> neg:bool -> int64 -> bool
val show_float_kind : float_kind -> string
val float_kinds : float_kind list
val float_kind_of_string : string -> float_kind option
val float_kind_size : float_kind -> int
val float_kind_exact_limit : float_kind -> int64
val pp_func_abi : Format.formatter -> func_abi -> unit
val show_func_abi : func_abi -> string
val func_abi_of_string : string -> func_abi option
val builtins : (string * ty) list
val pp_ty : Format.formatter -> ty -> unit
val show_ty : ty -> string
val resolve_ty : ty -> ty
val has_error : ty -> bool
val ty_equal : ty -> ty -> bool
val is_float : ty -> bool
val is_unsigned : ty -> bool
val is_aggregate : ty -> bool
val is_wide : ty -> bool
val is_never : ty -> bool
val is_unit : ty -> bool
val div_int_needs_check : ty -> bool
