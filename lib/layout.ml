(* SPDX-License-Identifier: Apache-2.0 *)

open Types
open Option.Syntax

type layout = { size : int; align : int; offsets : int iarray iarray }
type shape = Untagged | Tagged | TagOnly

(* A struct is one untagged variant so both share the same placement *)
type entry = {
  variants : ty iarray iarray;
  shape : shape;
  mutable cached_layout : (int * layout) option;
}

type t = { types : entry Symbol.Table.t; mutable revision : int }

let tag_ty = TInt I32
let create () = { types = Symbol.Table.create 16; revision = 0 }

let set_variants t key kind variants =
  let shape =
    match kind with
    | Struct -> Untagged
    | Enum when List.exists (Fun.negate List.is_empty) variants -> Tagged
    | Enum -> TagOnly
  in
  let variants = Iarray.of_list (List.map Iarray.of_list variants) in
  Symbol.Table.replace t.types key { variants; shape; cached_layout = None };
  t.revision <- t.revision + 1

let shape_of t key =
  Option.map (fun entry -> entry.shape) (Symbol.Table.find_opt t.types key)

let tagged t key = shape_of t key = Some Tagged

let has_payload t ty =
  match resolve_ty ty with
  | TAdt (name, _) -> tagged t (Keyname.key name)
  | _ -> false

(* A unit only enum is a plain int so it has no fields to lay out *)
let named_aggregate t ty =
  match resolve_ty ty with
  | TAdt (name, _) ->
      let* shape = shape_of t (Keyname.key name) in
      if shape = TagOnly then None else Some name
  | _ -> None

(* An aggregate is addressed by pointer so an ident of this type is its base address *)
let is_aggregate t ty =
  match resolve_ty ty with
  | TArray _ | TSlice _ | TStr -> true
  | _ -> Option.is_some (named_aggregate t ty)

let variants t key =
  match Symbol.Table.find_opt t.types key with
  | Some entry -> entry.variants
  | None -> Iarray.of_list []

let field_ty t name ~variant ~field =
  let unknown () =
    Diagnostic.ice
      (Printf.sprintf "unknown field %d.%d on %s" variant field
         (Keyname.show name))
  in
  let variants = variants t (Keyname.key name) in
  if variant >= Iarray.length variants then unknown ()
  else
    let tys = Iarray.get variants variant in
    if field < Iarray.length tys then Iarray.get tys field else unknown ()

let align_to n ~align = Int.cdiv n align * align

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
        let at = align_to used ~align:field_align in
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
      let layout = { size = align_to used ~align; align; offsets } in
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
  | TAdt (name, _) when Option.is_some (named_aggregate t ty) ->
      let layout = layout_of t name in
      (layout.size, layout.align)
  | TAdt _ ->
      (* TODO(f1ac): unit only enums are still fixed ints and flags don't exist *)
      ty_measure t tag_ty
  | TChar -> (4, 4)
  | TPointer _ | TPtr | TNull | TCStr | TFunc _ -> (8, 8)
  | TSlice _ | TStr -> (16, 8)
  | TUnit | TNever | TError | TType -> (0, 1)
  | TArray (e, n) -> (n * stride t e, ty_align t e)
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"

and ty_size t ty = fst (ty_measure t ty)
and ty_align t ty = snd (ty_measure t ty)
and stride t elem = align_to (ty_size t elem) ~align:(ty_align t elem)

let field_offset t name ~variant ~field =
  let layout = layout_of t name in
  let offsets =
    if variant < Iarray.length layout.offsets then
      Iarray.get layout.offsets variant
    else Iarray.of_list []
  in
  if field < Iarray.length offsets then Iarray.get offsets field
  else
    Diagnostic.ice
      (Printf.sprintf "unknown field %d.%d on %s" variant field
         (Keyname.show name))
