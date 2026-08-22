(* SPDX-License-Identifier: Apache-2.0 *)

exception Invalid_utf8 of string
exception Source_too_large of string

type source = {
  base : int; (* Where this file starts in the global offset space *)
  filename : string;
  source_map : Sourcemap.t;
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

(* Picks the file a global offset landed in *)
val source_at : t -> int -> source

val load :
  diags:Diagnostic.sink ->
  read_file:(string -> string) ->
  list_dir:(string -> string list) ->
  ?search_roots:string list ->
  root_filename:string ->
  unit ->
  t
