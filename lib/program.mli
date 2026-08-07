(* SPDX-License-Identifier: GPL-2.0-only *)

exception Invalid_utf8 of string

type source = {
  file_id : Span.file_id;
  filename : string;
  source_map : Source_map.t;
}

type unit_ = { source : source; ast : Ast.module_ }
type dependency = { import : Ast.import; target : Symbol.module_id }

type module_ = {
  module_id : Symbol.module_id;
  path : string list;
  units : unit_ list;
  dependencies : dependency list;
  failed : bool;
}

type t = { root : module_; root_source : source; modules : module_ array }

val module_decls : module_ -> Ast.decl list

val load :
  diags:Diagnostic.sink ->
  read_file:(string -> string) ->
  list_dir:(string -> string list) ->
  ?search_roots:string list ->
  root_filename:string ->
  unit ->
  t
