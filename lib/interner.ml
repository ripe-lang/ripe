(* SPDX-License-Identifier: Apache-2.0 *)

type id = int

module Ids = Hashtbl.Make (String)

(* One table for the whole run since a process only ever works on one program and threading it through every phase buys nothing *)
let ids = Ids.create 4096
let texts = Dynarray.create ()

let intern text =
  match Ids.find_opt ids text with
  | Some id -> id
  | None ->
      let id = Dynarray.length texts in
      Dynarray.add_last texts text;
      Ids.replace ids text id;
      id

let text id = Dynarray.get texts id

let pp fmt id = Format.pp_print_string fmt (text id)
