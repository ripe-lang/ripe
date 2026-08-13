(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast
open Types
module T = Typed_ast

(* Types need exact equality but NULL works with any pointer *)
(* TODO(b8e1): Is **i32 compatible with **null? TInt I8 with a TInt I32 (without cast)? *)
let rec compatible (want : ty) (got : ty) : bool =
  match (strip_alias want, strip_alias got) with
  | TError, _ | _, TError -> true
  | _, TNever -> true
  | TOpaquePtr, (TPointer _ | TCStr | TNull | TOpaquePtr) -> true
  | TPointer _, TNull -> true
  | TCStr, TPointer (TInt I8) | TPointer (TInt I8), TCStr -> true
  | TPointer a, TPointer b -> compatible_under_pointer a b
  (* A fixed array coerces to a slice of the same element type *)
  | TSlice a, TArray (b, _) -> compatible a b
  | TSlice a, TSlice b -> compatible a b
  | TFunc (p1, r1, abi1), TFunc (p2, r2, abi2) ->
      (abi1 = abi2 || abi1 = Types.AbiError || abi2 = Types.AbiError)
      && List.length p1 = List.length p2
      && List.for_all2 compatible p1 p2
      && compatible r1 r2
  (* A struct matches nominally by name and its type arguments must match exactly *)
  | TStruct (n1, a1), TStruct (n2, a2) ->
      n1 = n2 && List.length a1 = List.length a2 && List.for_all2 ty_equal a1 a2
  (* A newtype is its own type and never matches its base *)
  | TNewtype (n1, _), TNewtype (n2, _) -> n1 = n2
  | s_want, s_got -> ty_equal s_want s_got

and compatible_under_pointer (want : ty) (got : ty) : bool =
  match (strip_alias want, strip_alias got) with
  | TPointer _, TNull -> true
  | TCStr, TPointer (TInt I8) | TPointer (TInt I8), TCStr -> true
  | TPointer a, TPointer b -> compatible_under_pointer a b
  | s_want, s_got -> ty_equal s_want s_got

let is_lvalue (te : T.texpr) : bool =
  match te.T.desc with
  | T.TIdent _ | T.TFieldAccess _ | T.TIndex _ -> true
  | T.TUnOp (Deref, _) -> true
  | T.TUnOp _ -> false
  | T.TErrorExpr | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TCStr _
  | T.TStr _ | T.TChar _ | T.TCall _ | T.TBinOp _ | T.TCast _ | T.TSizeOf _
  | T.TRange _ | T.TRangeInclusive _ | T.TArrayLit _ | T.TLen _ | T.TSliceExpr _
  | T.TDataPtr _ | T.TZero | T.TUndef | T.TStructLit _ | T.TBlock _ | T.TIf _
  | T.TWhile _ | T.TFor _ | T.TBinding _ | T.TReturn _ | T.TBreak _
  | T.TContinue _ | T.TUnit ->
      false
  | T.TPairAssign _ | T.TLocalDecl | T.TLoop _ | T.TMatch _ | T.TVariant _ ->
      false

(* A deref stops the walk since the pointee isn't owned by this binding *)
let rec root_lvalue (te : T.texpr) : T.texpr option =
  match te.T.desc with
  | T.TIdent _ -> Some te
  | T.TFieldAccess (base, _) -> root_through base
  | T.TIndex (base, _) -> root_through base
  | T.TErrorExpr | T.TInt _ | T.TFloat _ | T.TBool _ | T.TNull | T.TCStr _
  | T.TStr _ | T.TChar _ | T.TCall _ | T.TBinOp _ | T.TUnOp _ | T.TCast _
  | T.TSizeOf _ | T.TRange _ | T.TRangeInclusive _ | T.TArrayLit _ | T.TLen _
  | T.TSliceExpr _ | T.TDataPtr _ | T.TZero | T.TUndef | T.TStructLit _
  | T.TBlock _ | T.TIf _ | T.TWhile _ | T.TFor _ | T.TBinding _ | T.TReturn _
  | T.TBreak _ | T.TContinue _ | T.TUnit ->
      None
  | T.TPairAssign _ | T.TLocalDecl | T.TLoop _ | T.TMatch _ | T.TVariant _ ->
      None

(* Going through a pointer or slice lands on memory this binding doesn't own *)
and root_through (base : T.texpr) : T.texpr option =
  match strip_alias base.T.ty with
  | TPointer _ | TSlice _ -> None
  | _ -> root_lvalue base

let root_binding (te : T.texpr) : Symbol.t option =
  match root_lvalue te with
  | Some { T.desc = T.TIdent s; _ } -> Some s
  | Some _ | None -> None

let is_numeric t =
  match strip_alias t with TInt _ | TFloat _ | TError -> true | _ -> false

(* A pointer is just an address so p < q asks which one sits earlier in memory *)
let is_ordered t =
  match strip_alias t with TPointer _ | TChar -> true | _ -> is_numeric t

let is_integer t =
  match strip_alias t with TInt _ | TError -> true | _ -> false

(* A newtype hides every operation of its base *)
let rec is_comparable = function
  | TInt _ | TFloat _ | TBool | TChar | TCStr | TPointer _ | TOpaquePtr | TNull
  | TError | TEnum _ ->
      true
  | TAlias (_, base) -> is_comparable base
  | TStr | TNever | TStruct _ | TFunc _ | TArray _ | TSlice _ | TNewtype _
  | TUnit ->
      false

let rec is_num_literal (e : expr) =
  match e.desc with
  | Int _ | Float _ -> true
  | UnOp ((Pos | Neg), operand) -> is_num_literal operand
  | _ -> false

let widens_to (src : ty) (tgt : ty) : bool =
  match (strip_alias src, strip_alias tgt) with
  | TInt src_kind, TInt tgt_kind ->
      int_kind_size src_kind < int_kind_size tgt_kind
      && (is_unsigned src || not (is_unsigned tgt))
  | TFloat F32, TFloat F64 -> true
  | _ -> false

let signed_ty_above (kind : int_kind) : ty option =
  match int_kind_size kind with
  | 1 -> Some (TInt I16)
  | 2 -> Some (TInt I32)
  | 4 -> Some (TInt I64)
  | 8 -> None
  | _ -> Diagnostic.ice "invalid integer size"

let common_numeric_ty (left : ty) (right : ty) : ty option =
  if ty_equal left right then Some left
  else if widens_to left right then Some right
  else if widens_to right left then Some left
  else
    match (strip_alias left, strip_alias right) with
    | TInt left_kind, TInt right_kind when is_unsigned left <> is_unsigned right
      ->
        let unsigned_kind =
          if is_unsigned left then left_kind else right_kind
        in
        signed_ty_above unsigned_kind
    | _ -> None

let suffix_kind s = match int_kind_of_string s with Some k -> k | None -> I32

let float_suffix_kind s =
  match float_kind_of_string s with Some k -> k | None -> F64

type cast_class = Numeric | Ptr | Aggregate

let cast_class t =
  match resolve_ty t with
  | TInt _ | TFloat _ | TBool | TChar -> Numeric
  | TPointer _ | TOpaquePtr | TCStr | TNull | TFunc _ -> Ptr
  (* TODO: no enum to integer cast until the boundary is settled *)
  | TStr | TNever | TStruct _ | TArray _ | TSlice _ | TError | TEnum _ | TUnit
    ->
      Aggregate
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* A pointer bit pattern is not a float and an aggregate only casts to itself *)
let cast_ok src tgt =
  match (resolve_ty src, resolve_ty tgt) with
  | TError, _ | _, TError -> true
  | s, TBool -> s = TBool
  (* Char is a distinct scalar so it only converts to and from integers *)
  | TChar, TChar -> true
  | TChar, TInt _ | TInt _, TChar -> true
  | TChar, _ | _, TChar -> false
  | _ -> (
      match (cast_class src, cast_class tgt) with
      | Aggregate, _ | _, Aggregate ->
          ty_equal (resolve_ty src) (resolve_ty tgt)
      | Numeric, Numeric | Ptr, Ptr -> true
      | (Numeric | Ptr), (Numeric | Ptr) ->
          (not (is_float src)) && not (is_float tgt))
