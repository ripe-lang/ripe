(* SPDX-License-Identifier: GPL-2.0-only *)

open Types
module T = Typed_ast

(* a param array is copied into this frame so a slice of it dangles too *)
let rec array_storage_escapes (te : T.texpr) : bool =
  match te.T.desc with
  | T.TIdent s -> (
      match s.Symbol.kind with
      | Symbol.Local _ | Symbol.Param | Symbol.ForVar -> true
      | _ -> false)
  | T.TArrayLit _ -> true
  | T.TIndex (base, _) -> array_storage_escapes base
  | T.TFieldAccess (base, _) -> array_storage_escapes base
  | _ -> false

(* only these two forms build a slice out of storage that could be local *)
let rec slice_return_escapes (te : T.texpr) : bool =
  match te.T.desc with
  | T.TToSlice arr -> array_storage_escapes arr
  | T.TSliceExpr (base, _, _) -> (
      match strip_alias base.T.ty with
      | TSlice _ -> slice_return_escapes base
      | _ -> array_storage_escapes base)
  | _ -> false
