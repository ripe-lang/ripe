(* SPDX-License-Identifier: GPL-2.0-only *)

(* A panic prints where it fired from a table the backend emits so the generated
   call carries only a site index *)

type site = { file : int; line : int; col : int; func : int }

type t = {
  (* A span carries a file id so a check inside an import points at that file
     and not at whatever the root happened to be *)
  source_of : Span.file_id -> string * Source_map.t;
  mutable cur_func : string;
  (* Both lists stay reversed until they're emitted *)
  mutable strings : string list;
  mutable strings_len : int;
  offsets : (string, int) Hashtbl.t;
  mutable sites : site list;
  mutable count : int;
  (* Two checks on one expression report the same thing so they share an entry *)
  ids : (site, int) Hashtbl.t;
}

let create ~(source_of : Span.file_id -> string * Source_map.t) : t =
  {
    source_of;
    cur_func = "";
    strings = [];
    strings_len = 0;
    offsets = Hashtbl.create 8;
    sites = [];
    count = 0;
    ids = Hashtbl.create 16;
  }

(* A site stores offsets rather than pointers so the table needs no relocations *)
let intern (t : t) (s : string) : int =
  match Hashtbl.find_opt t.offsets s with
  | Some off -> off
  | None ->
      let off = t.strings_len in
      t.strings <- s :: t.strings;
      (* The zero byte means the runtime doesn't need a length *)
      t.strings_len <- off + String.length s + 1;
      Hashtbl.replace t.offsets s off;
      off

let enter_func (t : t) (name : string) : unit = t.cur_func <- name

let record (t : t) (span : Ast.span) : int =
  if span.Span.file < 0 then
    Diagnostic.ice "runtime check has no source location";
  if t.cur_func = "" then Diagnostic.ice "runtime check outside a function";
  let filename, sm = t.source_of span.Span.file in
  let line, col = Source_map.lookup sm span.Span.lo in
  let site =
    { file = intern t filename; line; col; func = intern t t.cur_func }
  in
  match Hashtbl.find_opt t.ids site with
  | Some id -> id
  | None ->
      let id = t.count in
      t.count <- id + 1;
      t.sites <- site :: t.sites;
      Hashtbl.replace t.ids site id;
      id

(* Each backend spells a table its own way so the shape of one stays out of here *)
let strings (t : t) : string list = List.rev t.strings
let sites (t : t) : site list = List.rev t.sites
