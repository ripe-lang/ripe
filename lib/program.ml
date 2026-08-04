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

let empty_ast : Ast.module_ = { Ast.header = None; imports = []; decls = [] }

let parse_source ~(diags : Diagnostic.sink) (file_id : Span.file_id)
    (filename : string) (src : string) : source * Ast.module_ =
  let source = { file_id; filename; source_map = Source_map.create src } in
  let lexbuf = Lexing.from_string src in
  Lexing.set_filename lexbuf filename;
  let read = Lexer.read (Lexer.make_state file_id) in
  (* The bracket error is already in the sink so the payload would double it *)
  let ast =
    try Parser.parse ~diags read lexbuf with Diagnostic.Errors _ -> empty_ast
  in
  (source, ast)

let file_of_path (source_root : string) (path : string list) : string =
  List.fold_left Filename.concat source_root path ^ ".rp"

let dir_of_path (source_root : string) (path : string list) : string =
  List.fold_left Filename.concat source_root path

let show_module_path (path : string list) : string = String.concat "." path

let module_name_of_path (path : string list) : string =
  match List.rev path with name :: _ -> name | [] -> ""

let parent_path (path : string list) : string list =
  match List.rev path with _ :: rest -> List.rev rest | [] -> []

(* Every file of a module shares one namespace *)
let module_decls (module_ : module_) : Ast.decl list =
  List.concat_map (fun (unit_ : unit_) -> unit_.ast.Ast.decls) module_.units

let import_error ?(detail : string option) ~(diags : Diagnostic.sink)
    (import : Ast.import) (headline : string) : unit =
  let d = Diagnostic.error headline |> Diagnostic.at import.Ast.span in
  Diagnostic.emit diags
    (match detail with Some s -> Diagnostic.detail s d | None -> d)

(* The cycle is the tail of the stack starting where the path shows up again *)
let import_cycle (stack : hop list) (path : string list) : hop list =
  let rec starting_at = function
    | [] -> []
    | hop :: _ as hops when hop.from_path = path -> hops
    | _ :: hops -> starting_at hops
  in
  starting_at stack

let show_import_cycle (hops : hop list) (back : string list) : string =
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
let probe_header (src : string) : string option =
  let lexbuf = Lexing.from_string src in
  let read = Lexer.read (Lexer.make_state 0) in
  let rec first_item () =
    match read lexbuf with Tokens.SEMI, _ -> first_item () | tok, _ -> tok
  in
  match first_item () with
  | Tokens.MODULE -> (
      match read lexbuf with Tokens.IDENT name, _ -> Some name | _ -> None)
  | _ -> None

let ripe_files (list_dir : string -> string list) (dir : string) : string list =
  match list_dir dir with
  | exception Sys_error _ -> []
  (* A module's units shouldn't ride on filesystem order *)
  | entries ->
      entries
      |> List.filter (fun entry -> Filename.extension entry = ".rp")
      |> List.sort compare
      |> List.map (Filename.concat dir)

let locate_module ~(read_file : string -> string)
    ~(list_dir : string -> string list) (source_root : string)
    (path : string list) : located =
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
    (unit_ : unit_) : unit =
  let expected = module_name_of_path path in
  match unit_.ast.Ast.header with
  | Some header when header.Ast.name <> expected ->
      let wrong =
        Error.named header.Ast.span "module name mismatch" header.Ast.name
        |> Diagnostic.label ("expected " ^ expected)
      in
      (* A header naming its own directory means the import went too deep *)
      let parent = parent_path path in
      Diagnostic.emit diags
        (if parent <> [] && header.Ast.name = module_name_of_path parent then
           Diagnostic.help
             ("import `" ^ show_module_path parent ^ "` instead")
             wrong
         else wrong)
  | Some _ -> ()
  | None when merged ->
      Diagnostic.emit diags
        (Diagnostic.error ("missing `module " ^ expected ^ "` header")
        |> Diagnostic.at (Span.make unit_.source.file_id 0 0)
        |> Diagnostic.help
             "every file beside a module header needs the same header")
  | None -> ()

let load ~(diags : Diagnostic.sink) ~(read_file : string -> string)
    ~(list_dir : string -> string list) ~(root_filename : string) : t =
  (* A bare filename has no directory so every import would start with "./" *)
  let source_root =
    match Filename.dirname root_filename with "." -> "" | dir -> dir
  in
  let next_file_id = ref 0 in
  let next_module_id = ref 0 in
  let states = Hashtbl.create 16 in
  let modules = ref [] in
  let fresh_file_id () =
    let id = !next_file_id in
    incr next_file_id;
    id
  in
  let fresh_module_id () =
    let id = !next_module_id in
    incr next_module_id;
    id
  in
  let rec load_module stack imported_by path =
    match Hashtbl.find_opt states path with
    | Some (Loading module_id) ->
        (* The root imports itself back with no import of its own to blame *)
        (match imported_by with
        | None -> ()
        | Some import ->
            let detail = show_import_cycle (import_cycle stack path) path in
            import_error ~detail ~diags import "import cycle");
        module_id
    | Some (Loaded module_) -> module_.module_id
    | None -> (
        let module_id = fresh_module_id () in
        (* The ID comes first because an import can lead right back here *)
        Hashtbl.add states path (Loading module_id);
        let record units dependencies =
          let module_ = { module_id; path; units; dependencies } in
          Hashtbl.replace states path (Loaded module_);
          modules := module_ :: !modules;
          module_id
        in
        (* A file that won't read becomes a module so lookups never fail *)
        let unreadable import headline =
          import_error ~diags import headline;
          let source =
            {
              file_id = fresh_file_id ();
              filename = file_of_path source_root path;
              source_map = Source_map.create "";
            }
          in
          record [ { source; ast = empty_ast } ] []
        in
        let read_unit filename =
          let src = read_file filename in
          (* The lexer walks bytes so it would split a character in half *)
          if not (String.is_valid_utf_8 src) then raise (Invalid_utf8 filename);
          let source, ast =
            parse_source ~diags (fresh_file_id ()) filename src
          in
          { source; ast }
        in
        let loaded merged filenames =
          let units = List.map read_unit filenames in
          List.iter (check_header ~diags path merged) units;
          let follow (unit_ : unit_) (import : Ast.import) =
            let hop = { from_path = path; from_file = unit_.source.filename } in
            let target =
              load_module (stack @ [ hop ]) (Some import) import.Ast.path
            in
            { import; target }
          in
          let dependencies =
            units
            |> List.concat_map (fun (unit_ : unit_) ->
                List.map (follow unit_) unit_.ast.Ast.imports)
          in
          record units dependencies
        in
        (* The root has no import to blame so it throws instead *)
        let missing headline =
          match imported_by with
          | None -> raise (Sys_error (file_of_path source_root path))
          | Some import -> unreadable import headline
        in
        let load_units merged filenames =
          match loaded merged filenames with
          | module_id -> module_id
          | exception Invalid_utf8 filename -> (
              match imported_by with
              | None -> raise (Invalid_utf8 filename)
              | Some import ->
                  unreadable import
                    ("module is not valid UTF-8: " ^ show_module_path path))
        in
        (* The root came off the command line so nothing beside it competes *)
        let located =
          match imported_by with
          | None -> Single root_filename
          | Some _ -> locate_module ~read_file ~list_dir source_root path
        in
        match located with
        | Not_found -> missing ("module not found: " ^ show_module_path path)
        | Clash ->
            missing
              ("module is both a file and a directory: " ^ show_module_path path)
        | Single filename -> load_units false [ filename ]
        | Merged filenames -> load_units true filenames)
  in
  let root_path =
    [ root_filename |> Filename.basename |> Filename.remove_extension ]
  in
  let root_id = load_module [] None root_path in
  (* Sorting lines the array index up with the module ID *)
  let modules =
    !modules
    |> List.sort (fun (a : module_) b -> compare a.module_id b.module_id)
    |> Array.of_list
  in
  let root = modules.(root_id) in
  let root_source =
    match root.units with
    | unit_ :: _ -> unit_.source
    | [] -> Error.ice ("module has no units: " ^ show_module_path root.path)
  in
  { root; root_source; modules }
