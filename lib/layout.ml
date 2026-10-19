(* SPDX-License-Identifier: Apache-2.0 *)

open Types
open Option.Syntax

type layout = { size : int; align : int; offsets : int iarray }

type entry = {
  variants : ty iarray iarray;
  shape : shape;
  mutable cached_layout : (int * layout) option;
}

type t = { types : entry Symbol.Table.t; mutable revision : int }

let tag_ty = TInt I32
let create () = { types = Symbol.Table.create 16; revision = 0 }

let set_struct_fields t key fields =
  Symbol.Table.replace t.structs key
    { field_tys = Iarray.of_list fields; cached_layout = None };
  t.revision <- t.revision + 1

let shape_of t key =
  Option.map (fun entry -> entry.shape) (Symbol.Table.find_opt t.types key)

let struct_fields t key =
  match entry_of t key with Some e -> e.field_tys | None -> no_fields

let struct_field_ty t name index =
  match entry_of t (Keyname.key name) with
  | Some e when index < Iarray.length e.field_tys ->
      Iarray.get e.field_tys index
  | _ ->
      Diagnostic.ice
        (Printf.sprintf "unknown field %d on struct %s" index
           (Keyname.show name))

let align_to n a = Int.cdiv n a * a

(* A variant starts right after the tag so they all overlap like a C union *)
let rec layout_of t name =
  let entry =
    match Symbol.Table.find_opt t.types (Keyname.key name) with
    | Some entry -> entry
    | None ->
        Diagnostic.ice
          (Printf.sprintf "no layout recorded for %s" (Keyname.show name))
  in
  match entry.cached_layout with
  | Some (revision, layout) when revision = t.revision -> layout
  | _ ->
      let place (align, used) ft =
        let field_align = ty_align t ft in
        let at = align_to used field_align in
        ((max align field_align, at + ty_size t ft), at)
      in
      let start =
        if entry.shape = Tagged then (ty_align t tag_ty, ty_size t tag_ty)
        else (1, 0)
      in
      let lay_out (align, size) fields =
        let (field_align, used), offsets =
          Iarray.fold_left_map place start fields
        in
        ((max align field_align, max size used), offsets)
      in
      let (align, used), offsets =
        Iarray.fold_left_map lay_out start entry.variants
      in
      let layout = { size = align_to used align; align; offsets } in
      entry.cached_layout <- Some (t.revision, layout);
      layout

(* TODO(2e26): every enum tag is an i32 until a backing type can be written down *)
and ty_measure t ty =
  match resolve_ty ty with
  | TInt k ->
      let n = int_kind_size k in
      (n, n)
  | TFloat k ->
      let n = float_kind_size k in
      (n, n)
  | TBool -> (1, 1)
  | TChar | TEnum _ -> (4, 4)
  | TPointer _ | TPtr | TNull | TCStr | TFunc _ -> (8, 8)
  | TSlice _ | TStr -> (16, 8)
  | TUnit | TNever | TError -> (0, 1)
  | TStruct (name, _) ->
      let layout = layout_of t name in
      (layout.size, layout.align)
  | TArray (e, n) -> (n * stride t e, ty_align t e)
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"

and ty_size t ty = fst (ty_measure t ty)
and ty_align t ty = snd (ty_measure t ty)
and stride t elem = align_to (ty_size t elem) (ty_align t elem)

let field_offset t name variant index =
  let layout = layout_of t name in
  let offsets =
    if variant < Iarray.length layout.offsets then
      Iarray.get layout.offsets variant
    else Iarray.of_list []
  in
  if index < Iarray.length offsets then Iarray.get offsets index
  else
    Diagnostic.ice
      (Printf.sprintf "unknown field %d.%d on %s" variant index
         (Keyname.show name))
