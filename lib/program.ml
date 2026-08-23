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

type t = {
  root : module_;
  (* A dummy span has no file so rendering needs somewhere to point *)
  root_source : source;
  modules : module_ array;
}

type load_state = Loading of Symbol.module_id | Loaded of module_
type hop = { from_path : string list; from_file : string }
type located = Single of string | Merged of string list | Clash | Not_found
type load_origin = Root | Imported of Ast.import

type loader = {
  diags : Diagnostic.sink;
  read_file : string -> string;
  list_dir : string -> string list;
  roots : string list;
  source_root : string;
  root_filename : string;
  states : (string list, load_state) Hashtbl.t;
  mutable next_base : int;
  mutable next_module_id : int;
  mutable modules : module_ list;
}

let module_decls (module_ : module_) =
  List.concat_map (fun (unit_ : unit_) -> unit_.ast.Ast.decls) module_.units

let source_at (t : t) =
  let sources =
    t.modules |> Array.to_list
    |> List.concat_map (fun (m : module_) -> m.units)
    |> List.map (fun (u : unit_) -> u.source)
    |> List.sort (fun (a : source) (b : source) -> compare a.base b.base)
    |> Array.of_list
  in
  let rec search pos lo hi =
    if lo > hi then t.root_source
    else
      let mid = (lo + hi) / 2 in
      if sources.(mid).base > pos then search pos lo (mid - 1)
      else if mid + 1 < Array.length sources && sources.(mid + 1).base <= pos
      then search pos (mid + 1) hi
      else sources.(mid)
  in
  fun pos ->
    if Array.length sources = 0 || pos < 0 then t.root_source
    else search pos 0 (Array.length sources - 1)

let empty_ast = { Ast.header = None; imports = []; decls = [] }

let parse_source ~(diags : Diagnostic.sink) ~(base : int) (filename : string)
    (src : string) =
  let source = { base; filename; source_map = Sourcemap.create ~base src } in
  let lexbuf = Lexer.lexbuf_of_string src in
  let read = Lexer.read (Lexer.make_state base) in
  (* The bracket error is already in the sink so the payload would double it *)
  let ast =
    try Parser.parse ~diags read lexbuf with Diagnostic.Errors _ -> empty_ast
  in
  (source, ast)

let file_of_path (source_root : string) (path : string list) =
  List.fold_left Filename.concat source_root path ^ ".rp"

let dir_of_path (source_root : string) (path : string list) =
  List.fold_left Filename.concat source_root path

let show_module_path (path : string list) = String.concat "." path

let module_name_of_path (path : string list) =
  match List.rev path with name :: _ -> name | [] -> ""

let parent_path (path : string list) =
  match List.rev path with _ :: rest -> List.rev rest | [] -> []

(* The cycle is the tail of the stack starting where the path shows up again *)
let import_cycle (stack : hop list) (path : string list) =
  let rec starting_at = function
    | [] -> []
    | hop :: _ as hops when hop.from_path = path -> hops
    | _ :: hops -> starting_at hops
  in
  starting_at stack

let show_import_cycle hops back =
  let line hop target =
    "    imports " ^ show_module_path target ^ " from " ^ hop.from_file ^ "\n"
  in
  let rec hops_from = function
    | [] -> []
    | [ hop ] -> [ line hop back ]
    | hop :: (next :: _ as rest) -> line hop next.from_path :: hops_from rest
  in
  match hops with
  | [] -> ""
  | first :: _ ->
      "  module "
      ^ show_module_path first.from_path
      ^ "\n"
      ^ String.concat "" (hops_from hops)

(* Only the first item matters so a full parse would double every error *)
let probe_header (src : string) =
  let lexbuf = Lexer.lexbuf_of_string src in
  let read = Lexer.read (Lexer.make_state 0) in
  let rec first_item () =
    match read lexbuf with
    | (Tokens.AUTOSEMI | Tokens.SEMI), _, _ -> first_item ()
    | tok, _, _ -> tok
  in
  let module_name () =
    match read lexbuf with Tokens.IDENT name, _, _ -> Some name | _ -> None
  in
  match first_item () with Tokens.MODULE -> module_name () | _ -> None

let ripe_files (list_dir : string -> string list) (dir : string) =
  match list_dir dir with
  | exception Sys_error _ -> []
  | entries ->
      entries
      |> List.filter (fun entry -> Filename.extension entry = ".rp")
      |> List.sort compare
      |> List.map (Filename.concat dir)

let locate_module ~(read_file : string -> string)
    ~(list_dir : string -> string list) (source_root : string)
    (path : string list) =
  let file = file_of_path source_root path in
  let readable filename =
    match read_file filename with exception Sys_error _ -> false | _ -> true
  in
  let declares filename =
    match read_file filename with
    | exception Sys_error _ -> false
    | src -> probe_header src <> None
  in
  let candidates = ripe_files list_dir (dir_of_path source_root path) in
  match (readable file, List.exists declares candidates) with
  | true, true -> Clash
  | true, false -> Single file
  | false, true -> Merged candidates
  | false, false -> Not_found

(* Every file of a merged module needs the name importers actually write *)
let check_header ~(diags : Diagnostic.sink) (path : string list) (merged : bool)
    (unit_ : unit_) =
  let expected = module_name_of_path path in
  match unit_.ast.Ast.header with
  | Some header when Interner.text header.Ast.name <> expected ->
      (* A header naming its own directory means the import went too deep *)
      let parent = parent_path path in
      let parent_import =
        if
          (not (List.is_empty parent))
          && Interner.text header.Ast.name = module_name_of_path parent
        then Some (show_module_path parent)
        else None
      in
      let diagnostic =
        Diagnostic.error_at header.Ast.span "module name mismatch"
        |> Diagnostic.label ("expected " ^ expected)
      in
      let diagnostic =
        match parent_import with
        | Some path ->
            Diagnostic.help ("import `" ^ path ^ "` instead") diagnostic
        | None -> diagnostic
      in
      Diagnostic.emit diags diagnostic
  | Some _ -> ()
  | None when merged ->
      Diagnostic.emit diags
        (Diagnostic.error "missing module header"
        |> Diagnostic.at (Span.make unit_.source.base unit_.source.base)
        |> Diagnostic.label ("expected `module " ^ expected ^ "`")
        |> Diagnostic.help
             "every file beside a module header needs the same header")
  | None -> ()

let rec locate_in loader path = function
  | [] -> (Not_found : located)
  | root :: rest -> (
      match
        locate_module ~read_file:loader.read_file ~list_dir:loader.list_dir root
          path
      with
      | Not_found -> locate_in loader path rest
      | found -> found)

let fresh_base loader filename length =
  let base = loader.next_base in
  if base + length > Span.max_offset then raise (Source_too_large filename);
  loader.next_base <- base + length;
  base

let fresh_module_id loader =
  let id = loader.next_module_id in
  loader.next_module_id <- id + 1;
  id

let record_module loader module_id path ?(failed = false) units dependencies =
  let module_ = { module_id; path; units; dependencies; failed } in
  Hashtbl.replace loader.states path (Loaded module_);
  loader.modules <- module_ :: loader.modules;
  module_id

(* A file that won't read becomes a module so lookups never fail *)
let record_failed_module loader module_id path diagnostic =
  Diagnostic.emit loader.diags diagnostic;
  let filename = file_of_path loader.source_root path in
  let base = fresh_base loader filename 0 in
  let source = { base; filename; source_map = Sourcemap.create ~base "" } in
  record_module loader module_id path ~failed:true
    [ { source; ast = empty_ast } ]
    []

let read_unit loader filename =
  let src = loader.read_file filename in
  (* The lexer walks bytes so it would split a character in half *)
  if not (String.is_valid_utf_8 src) then raise (Invalid_utf8 filename);
  let source, ast =
    parse_source ~diags:loader.diags
      ~base:(fresh_base loader filename (String.length src))
      filename src
  in
  { source; ast }

let tried_paths loader path =
  let show index root =
    let lead = if index = 0 then "  tried " else "        " in
    lead ^ file_of_path root path ^ "\n"
  in
  loader.roots |> List.mapi show |> String.concat ""

let rec load_module loader stack origin path =
  match Hashtbl.find_opt loader.states path with
  | Some (Loading module_id) ->
      begin match origin with
      | Root -> ()
      | Imported import ->
          let detail =
            show_import_cycle (import_cycle (List.rev stack) path) path
          in
          Diagnostic.emit loader.diags
            (Diagnostic.error "import cycle"
            |> Diagnostic.at import.Ast.span
            |> Diagnostic.detail detail)
      end;
      module_id
  | Some (Loaded module_) -> module_.module_id
  | None ->
      let module_id = fresh_module_id loader in
      (* The ID comes first because an import can lead right back here *)
      Hashtbl.add loader.states path (Loading module_id);
      load_new_module loader stack origin module_id path

and load_new_module loader stack origin module_id path =
  let located =
    match origin with
    | Root -> Single loader.root_filename
    | Imported _ -> locate_in loader path loader.roots
  in
  try
    match (origin, located) with
    | Imported import, Not_found ->
        record_failed_module loader module_id path
          (Diagnostic.error "module not found"
          |> Diagnostic.at import.Ast.span
          |> Diagnostic.detail (tried_paths loader path))
    | Imported import, Clash ->
        record_failed_module loader module_id path
          (Diagnostic.error "module is both a file and a directory"
          |> Diagnostic.at import.Ast.span)
    | _, Single filename ->
        load_units loader stack module_id path false [ filename ]
    | _, Merged filenames ->
        load_units loader stack module_id path true filenames
    | Root, (Not_found | Clash) ->
        raise (Sys_error (file_of_path loader.source_root path))
  with Invalid_utf8 filename -> (
    match origin with
    | Root -> raise (Invalid_utf8 filename)
    | Imported import ->
        record_failed_module loader module_id path
          (Diagnostic.error ("not valid UTF-8: " ^ filename)
          |> Diagnostic.at import.Ast.span))

and load_units loader stack module_id path merged filenames =
  let units = List.map (read_unit loader) filenames in
  List.iter (check_header ~diags:loader.diags path merged) units;
  let dependencies =
    units |> List.concat_map (load_imports loader stack path)
  in
  record_module loader module_id path units dependencies

and load_imports loader stack path unit_ =
  let load_import import =
    let hop = { from_path = path; from_file = unit_.source.filename } in
    let target_path = List.map Interner.text import.Ast.path in
    let target =
      load_module loader (hop :: stack) (Imported import) target_path
    in
    { import; target }
  in
  List.map load_import unit_.ast.Ast.imports

let load ~(diags : Diagnostic.sink) ~(read_file : string -> string)
    ~(list_dir : string -> string list) ?(search_roots : string list = [])
    ~(root_filename : string) () =
  (* A bare filename has no directory so every import would start with "./" *)
  let source_root =
    match Filename.dirname root_filename with "." -> "" | dir -> dir
  in
  (* An import tries each root in turn and the first hit wins
     1. beside the root file so a local copy shadows an installed one
     2. every -I on the command line in the order they were given
     3. every dir in RIPE_PATH *)
  let roots = source_root :: search_roots in
  let loader =
    {
      diags;
      read_file;
      list_dir;
      roots;
      source_root;
      root_filename;
      states = Hashtbl.create 16;
      next_base = 0;
      next_module_id = 0;
      modules = [];
    }
  in
  let root_path =
    [ root_filename |> Filename.basename |> Filename.remove_extension ]
  in
  let root_id = load_module loader [] Root root_path in
  (* Sorting lines the array index up with the module ID *)
  let modules =
    loader.modules
    |> List.sort (fun (a : module_) b -> compare a.module_id b.module_id)
    |> Array.of_list
  in
  let root = modules.(root_id) in
  let root_source =
    match root.units with
    | unit_ :: _ -> unit_.source
    | [] -> Diagnostic.ice ("module has no units: " ^ show_module_path root.path)
  in
  { root; root_source; modules }
