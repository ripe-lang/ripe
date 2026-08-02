(* SPDX-License-Identifier: GPL-2.0-only *)

type source = {
  file_id : Span.file_id;
  filename : string;
  source_map : Source_map.t;
}

type unit_ = { source : source; ast : Ast.module_ }

type module_ = {
  module_id : Symbol.module_id;
  path : string list;
  units : unit_ list;
}

type t = {
  root : module_;
  (* A dummy span has no file of its own so rendering it needs somewhere to
     point *)
  root_source : source;
  modules : module_ array;
}

let empty_ast : Ast.module_ = { Ast.header = None; imports = []; decls = [] }

let parse_source ~(diags : Diagnostic.sink) (file_id : Span.file_id)
    (filename : string) (src : string) : source * Ast.module_ =
  let source = { file_id; filename; source_map = Source_map.create src } in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf filename;
  let read = Lexer.read (Lexer.make_state file_id) in
  (* The bracket error is already in the sink so reporting the payload too would
     say it twice *)
  let ast =
    try Parser.parse ~diags read lexbuf with Diagnostic.Errors _ -> empty_ast
  in
  (source, ast)

let load ~(diags : Diagnostic.sink) ~(read_file : string -> string)
    ~(root_filename : string) : t =
  let source, ast =
    parse_source ~diags 0 root_filename (read_file root_filename)
  in
  let path =
    [ root_filename |> Filename.basename |> Filename.remove_extension ]
  in
  let root = { module_id = 0; path; units = [ { source; ast } ] } in
  { root; root_source = source; modules = [| root |] }
