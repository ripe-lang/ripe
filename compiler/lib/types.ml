type int_kind = I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64
[@@deriving show { with_path = false }]

type ty =
  | TInt of int_kind
  | TBool
  | TString
  | TVoid
  | TNull
  | TPointer of ty
  | TStruct of string
[@@deriving show { with_path = false }]
