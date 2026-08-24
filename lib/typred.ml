(* SPDX-License-Identifier: Apache-2.0 *)

open Ast
open Types

(* Types need exact equality but NULL works with any pointer *)
(* TODO(b8e1): Is **i32 compatible with **null? TInt I8 with a TInt I32 (without cast)? *)
let rec compatible want got =
  match (resolve_ty want, resolve_ty got) with
  | TError, _ | _, TError -> true
  | _, TNever -> true
  | TOpaquePtr, (TPointer _ | TCStr | TNull | TOpaquePtr) -> true
  | TPointer _, TNull -> true
  | TCStr, TPointer (TInt I8) | TPointer (TInt I8), TCStr -> true
  | TPointer a, TPointer b -> compatible_under_pointer a b
  | TSlice a, TArray (b, _) -> compatible a b
  | TSlice a, TSlice b -> compatible a b
  | TFunc (p1, r1, abi1), TFunc (p2, r2, abi2) ->
      (abi1 = abi2 || abi1 = Types.AbiError || abi2 = Types.AbiError)
      && List.compare_lengths p1 p2 = 0
      && List.for_all2 compatible p1 p2
      && compatible r1 r2
  | TStruct (n1, a1), TStruct (n2, a2) ->
      n1 = n2 && List.compare_lengths a1 a2 = 0 && List.for_all2 ty_equal a1 a2
  | _, _ -> ty_equal want got

and compatible_under_pointer want got =
  match (resolve_ty want, resolve_ty got) with
  | TPointer _, TNull -> true
  | TCStr, TPointer (TInt I8) | TPointer (TInt I8), TCStr -> true
  | TPointer a, TPointer b -> compatible_under_pointer a b
  | _, _ -> ty_equal want got

let is_lvalue te =
  match te.Tast.desc with
  | Tast.TIdent _ | Tast.TFieldAccess _ | Tast.TIndex _ -> true
  | Tast.TUnOp (Deref, _) -> true
  | _ -> false

(* A deref stops the walk since the pointee isn't owned by this binding *)
let rec root_lvalue te =
  match te.Tast.desc with
  | Tast.TIdent _ -> Some te
  | Tast.TFieldAccess (base, _) -> root_through base
  | Tast.TIndex (base, _) -> root_through base
  | _ -> None

(* Going through a pointer or slice lands on memory this binding doesn't own *)
and root_through base =
  match resolve_ty base.Tast.ty with
  | TPointer _ | TSlice _ -> None
  | _ -> root_lvalue base

let root_binding te =
  match root_lvalue te with
  | Some { Tast.desc = Tast.TIdent s; _ } -> Some s
  | Some _ | None -> None

let is_numeric t =
  match resolve_ty t with TInt _ | TFloat _ | TError -> true | _ -> false

(* A pointer is just an address so p < q asks which one sits earlier in memory *)
let is_ordered t =
  match resolve_ty t with TPointer _ | TChar -> true | _ -> is_numeric t

let is_integer t =
  match resolve_ty t with TInt _ | TError -> true | _ -> false

let rec is_comparable = function
  | TInt _ | TFloat _ | TBool | TChar | TCStr | TPointer _ | TOpaquePtr | TNull
  | TError | TEnum _ ->
      true
  | TAlias (_, base) -> is_comparable base
  | TStr | TNever | TStruct _ | TFunc _ | TArray _ | TSlice _ | TUnit -> false

let binop_accepts op =
  match op with
  | Add | Sub | Mul | Div -> is_numeric
  | Mod | BitAnd | BitOr | BitXor | Lshift | Rshift -> is_integer
  | Eq | Neq -> is_comparable
  | Lt | Gt | Lte | Gte -> is_ordered
  | And | Or -> fun _ -> true

let unop_accepts op =
  match op with
  | Pos | Neg -> is_numeric
  | BitNot -> is_integer
  | Not | Deref | AddressOf -> fun _ -> true

let rec is_num_literal e =
  match e.desc with
  | Int _ | Float _ -> true
  | UnOp ((Pos | Neg), operand) -> is_num_literal operand
  | _ -> false

let widens_to src tgt =
  match (resolve_ty src, resolve_ty tgt) with
  | TInt src_kind, TInt tgt_kind ->
      int_kind_size src_kind < int_kind_size tgt_kind
      && (is_unsigned src || not (is_unsigned tgt))
  | TFloat F32, TFloat F64 -> true
  | _ -> false

let signed_ty_above = function
  | I8 | U8 -> Some (TInt I16)
  | I16 | U16 -> Some (TInt I32)
  | I32 | U32 -> Some (TInt I64)
  | I64 | U64 | Isize | Usize -> None

let common_numeric_ty left right =
  if ty_equal left right then Some left
  else if widens_to left right then Some right
  else if widens_to right left then Some left
  else
    match (resolve_ty left, resolve_ty right) with
    | TInt left_kind, TInt right_kind when is_unsigned left <> is_unsigned right
      ->
        let unsigned_kind =
          if is_unsigned left then left_kind else right_kind
        in
        signed_ty_above unsigned_kind
    | _ -> None

let suffix_kind s = Option.value (int_kind_of_string s) ~default:I32
let float_suffix_kind s = Option.value (float_kind_of_string s) ~default:F64

type cast_class = Numeric | Ptr | Aggregate

let cast_class t =
  match resolve_ty t with
  | TInt _ | TFloat _ | TBool | TChar -> Numeric
  | TPointer _ | TOpaquePtr | TCStr | TNull | TFunc _ -> Ptr
  | TStr | TNever | TStruct _ | TArray _ | TSlice _ | TError | TEnum _ | TUnit
    ->
      Aggregate
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"

(* Both sides need the same width so not floats *)
let bitcast_ok src tgt =
  match (cast_class src, cast_class tgt) with
  | Aggregate, _ | _, Aggregate -> false
  | _ ->
      (not (is_float src))
      && (not (is_float tgt))
      && resolve_ty src <> TBool
      && resolve_ty tgt <> TBool
      && is_wide_ty src = is_wide_ty tgt

(* A pointer bit pattern is not a float and an aggregate only casts to itself *)
let cast_ok src tgt =
  let cast_classes_ok () =
    match (cast_class src, cast_class tgt) with
    | Aggregate, _ | _, Aggregate -> ty_equal src tgt
    | Numeric, Numeric -> true
    | (Numeric | Ptr), (Numeric | Ptr) -> false
  in
  match (resolve_ty src, resolve_ty tgt) with
  | TError, _ | _, TError -> true
  | s, TBool -> s = TBool
  | TChar, TChar -> true
  | TChar, TInt _ | TInt _, TChar -> true
  | TChar, _ | _, TChar -> false
  | _ -> cast_classes_ok ()
