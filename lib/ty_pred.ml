(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast
open Types
module T = Typed_ast

(* Exact equality but NULL is compatible with any pointer *)
(* TODO(b8e1): Is **i32 compatible with **null? TInt I8 with a TInt I32 (without cast)? *)
let rec compatible (want : ty) (got : ty) : bool =
  match (strip_alias want, strip_alias got) with
  | _, TNever -> true
  | TOpaquePtr, (TPointer _ | TCStr | TNull | TOpaquePtr) -> true
  | TPointer _, TNull -> true
  | TCStr, TPointer (TInt I8) | TPointer (TInt I8), TCStr -> true
  | TPointer a, TPointer b -> compatible_under_pointer a b
  (* a fixed array coerces to a slice of the same element type *)
  | TSlice a, TArray (b, _) -> compatible a b
  | TSlice a, TSlice b -> compatible a b
  | TFunc (p1, r1), TFunc (p2, r2) ->
      List.length p1 = List.length p2
      && List.for_all2 compatible p1 p2
      && compatible r1 r2
  (* a struct matches nominally by name and its type arguments must match exactly *)
  | TStruct (n1, a1), TStruct (n2, a2) ->
      n1 = n2 && List.length a1 = List.length a2 && List.for_all2 ty_equal a1 a2
  (* a newtype is its own type and never matches its base *)
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
  | TIdent _ | TFieldAccess _ | TIndex _ -> true
  | TUnOp (Deref, _) -> true
  | TUnOp _ -> false
  | TInt _ | TFloat _ | TBool _ | TNull | TCStr _ | TChar _ | TCall _ | TBinOp _
  | TCast _ | TSizeOf _ | TRange _ | TRangeInclusive _ | TArrayLit _ | TLen _
  | TToSlice _ | TSliceExpr _ | TDataPtr _ | TZero | TUndef | TStructLit _
  | TBlockExpr _ ->
      false

(* a deref stops the walk since the pointee isn't owned by this binding *)
let rec root_binding (te : T.texpr) : Symbol.t option =
  match te.T.desc with
  | TIdent s -> Some s
  | TFieldAccess (base, _) -> root_through base
  | TIndex (base, _) -> root_through base
  | TInt _ | TFloat _ | TBool _ | TNull | TCStr _ | TChar _ | TCall _ | TBinOp _
  | TUnOp _ | TCast _ | TSizeOf _ | TRange _ | TRangeInclusive _ | TArrayLit _
  | TLen _ | TToSlice _ | TSliceExpr _ | TDataPtr _ | TZero | TUndef
  | TStructLit _ | TBlockExpr _ ->
      None

(* going through a pointer or slice lands on memory this binding doesn't own *)
and root_through (base : T.texpr) : Symbol.t option =
  match strip_alias base.T.ty with
  | TPointer _ | TSlice _ -> None
  | _ -> root_binding base

let is_numeric t =
  match strip_alias t with TInt _ | TFloat _ | TError -> true | _ -> false

(* a pointer is just an address so p < q asks which one sits earlier in memory *)
let is_ordered t =
  match strip_alias t with TPointer _ -> true | _ -> is_numeric t

let is_integer t =
  match strip_alias t with TInt _ | TError -> true | _ -> false

(* a newtype hides every operation of its base *)
(* TODO(70f0): let a newtype opt back into operators like haskell deriving *)
let rec is_comparable = function
  | TInt _ | TFloat _ | TBool | TCStr | TPointer _ | TOpaquePtr | TNull | TError
    ->
      true
  | TAlias (_, base) -> is_comparable base
  | TVoid | TNever | TStruct _ | TFunc _ | TArray _ | TSlice _ | TNewtype _ ->
      false

let is_int_literal (e : expr) = match e.desc with Int _ -> true | _ -> false
let suffix_kind s = match int_kind_of_string s with Some k -> k | None -> I32

type cast_class = Numeric | Ptr | Aggregate

let cast_class t =
  match resolve_ty t with
  | TInt _ | TFloat _ | TBool -> Numeric
  | TPointer _ | TOpaquePtr | TCStr | TNull | TFunc _ -> Ptr
  | TVoid | TNever | TStruct _ | TArray _ | TSlice _ | TError -> Aggregate
  | TNewtype _ | TAlias _ -> assert false (* resolve_ty strips these *)

(* a pointer bit pattern is not a float and an aggregate only casts to itself *)
let cast_ok src tgt =
  match (resolve_ty src, resolve_ty tgt) with
  | s, TBool -> s = TBool
  | _ -> (
      match (cast_class src, cast_class tgt) with
      | Aggregate, _ | _, Aggregate ->
          ty_equal (resolve_ty src) (resolve_ty tgt)
      | Numeric, Numeric | Ptr, Ptr -> true
      | (Numeric | Ptr), (Numeric | Ptr) ->
          (not (is_float src)) && not (is_float tgt))
