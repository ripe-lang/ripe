(* SPDX-License-Identifier: Apache-2.0 *)

open Types

let align_to n a = Int.cdiv n a * a

type layout = { size : int; align : int; offsets : int iarray }

(* The fields and what they measure sit together so one lookup answers both *)
type entry = { field_tys : ty iarray; mutable layout : (int * layout) option }

type structs = {
  entries : entry Symbol.Table.t;
  (* A `sizeof` in a constant can measure a struct before its turn comes *)
  mutable generation : int;
}

let no_fields = Iarray.of_list []
let make_structs () = { entries = Symbol.Table.create 16; generation = 0 }

let set_struct_fields structs key fields =
  Symbol.Table.replace structs.entries key
    { field_tys = Iarray.of_list fields; layout = None };
  structs.generation <- structs.generation + 1

let entry_of structs key = Symbol.Table.find_opt structs.entries key

let struct_fields structs key =
  match entry_of structs key with Some e -> e.field_tys | None -> no_fields

let struct_field_ty structs name index =
  match entry_of structs (Qname.key name) with
  | Some e when index < Iarray.length e.field_tys ->
      Iarray.get e.field_tys index
  | _ ->
      Diagnostic.ice
        (Printf.sprintf "unknown field %d on struct %s" index (Qname.show name))

let rec layout_of structs name =
  let entry =
    match entry_of structs (Qname.key name) with
    | Some entry -> entry
    | None ->
        Diagnostic.ice
          (Printf.sprintf "no layout recorded for struct %s" (Qname.show name))
  in
  match entry.layout with
  | Some (generation, layout) when generation = structs.generation -> layout
  | _ ->
      let place (align, used) ft =
        let field_align = ty_align structs ft in
        let at = align_to used field_align in
        ((max align field_align, at + ty_size structs ft), at)
      in
      let (align, used), offsets =
        Iarray.fold_left_map place (1, 0) entry.field_tys
      in
      let layout = { size = align_to used align; align; offsets } in
      entry.layout <- Some (structs.generation, layout);
      layout

(* A scalar is as wide as it is aligned and only aggregates differ *)
and ty_measure structs t =
  match resolve_ty t with
  | TInt k ->
      let n = int_kind_size k in
      (n, n)
  | TFloat k ->
      let n = float_kind_size k in
      (n, n)
  | TBool -> (1, 1)
  | TChar -> (4, 4)
  | TPointer _ | TOpaquePtr | TNull | TCStr | TFunc _ -> (8, 8)
  | TNever -> Diagnostic.ice "TNever has no size"
  (* A field that already errored still needs a size so checking can go on *)
  | TError -> (0, 1)
  | TStruct (name, _) ->
      let layout = layout_of structs name in
      (layout.size, layout.align)
  | TArray (e, n) -> (n * stride structs e, ty_align structs e)
  | TSlice _ | TStr -> (16, 8)
  | TEnum _ -> (4, 4)
  | TUnit -> (0, 1)
  | TAlias _ -> Diagnostic.ice "resolve_ty left an alias"

and ty_size structs t = fst (ty_measure structs t)
and ty_align structs t = snd (ty_measure structs t)

and stride structs elem =
  align_to (ty_size structs elem) (ty_align structs elem)

let field_offset structs name index =
  let layout = layout_of structs name in
  if index >= Iarray.length layout.offsets then
    Diagnostic.ice (Printf.sprintf "unknown field index %d" index)
  else Iarray.get layout.offsets index
