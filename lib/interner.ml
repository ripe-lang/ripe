(* SPDX-License-Identifier: GPL-2.0-only *)

type id = int

(* One table for the whole run since a process only ever works on one program
   and threading it through every phase buys nothing *)
let ids : (string, id) Hashtbl.t = Hashtbl.create 4096
let texts : string Dynarray.t = Dynarray.create ()

let intern (text : string) : id =
  match Hashtbl.find_opt ids text with
  | Some id -> id
  | None ->
      let id = Dynarray.length texts in
      Dynarray.add_last texts text;
      Hashtbl.replace ids text id;
      id

let text (id : id) : string = Dynarray.get texts id

let pp (fmt : Format.formatter) (id : id) : unit =
  Format.pp_print_string fmt (text id)
