(* SPDX-License-Identifier: GPL-2.0-only *)

open Types
module T = Typed_ast

type escape = Slice | Address

let inline_aggregate t =
  match strip_alias t with TArray _ | TStruct _ -> true | _ -> false

(* a param array is copied into this frame so a reference into it dangles too *)
let rec storage_is_local (te : T.texpr) : bool =
  match te.T.desc with
  | T.TIdent s -> (
      match s.Symbol.kind with
      | Symbol.Local _ | Symbol.Param | Symbol.ForVar -> true
      | _ -> false)
  | T.TArrayLit _ -> true
  | T.TCall _ -> inline_aggregate te.T.ty
  (* a pointer or slice hop lands in memory the frame doesn't own *)
  | T.TIndex (base, _) | T.TFieldAccess (base, _) ->
      inline_aggregate base.T.ty && storage_is_local base
  | _ -> false

(* only these two forms build a slice out of storage that could be local *)
let rec slice_escapes (te : T.texpr) : bool =
  match te.T.desc with
  | T.TToSlice arr -> storage_is_local arr
  | T.TSliceExpr (base, _, _) -> (
      match strip_alias base.T.ty with
      | TSlice _ -> slice_escapes base
      | _ -> storage_is_local base)
  | _ -> false

(* the frame dies at return so a slice or an address into it is left dangling *)
let return_escapes (te : T.texpr) : escape option =
  match te.T.desc with
  | T.TUnOp (Ast.AddressOf, target) when storage_is_local target -> Some Address
  | _ -> if slice_escapes te then Some Slice else None
