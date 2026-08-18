(* SPDX-License-Identifier: Apache-2.0 *)

type id = int

module Ids = Hashtbl.Make (String)

(* One table for the whole run since a process only ever works on one program
   and threading it through every phase buys nothing *)
let ids : id Ids.t = Ids.create 4096
let texts : string Dynarray.t = Dynarray.create ()

let intern (text : string) : id =
  match Ids.find_opt ids text with
  | Some id -> id
  | None ->
      let id = Dynarray.length texts in
      Dynarray.add_last texts text;
      Ids.replace ids text id;
      id

let text (id : id) : string = Dynarray.get texts id

let pp (fmt : Format.formatter) (id : id) : unit =
  Format.pp_print_string fmt (text id)
