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
  (* A dummy span has no file of its own so rendering it needs somewhere to
     point *)
  root_source : source;
  modules : module_ array;
}

type load_state = Loading of Symbol.module_id | Loaded of module_

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

let file_of_path (source_root : string) (path : string list) : string =
  List.fold_left Filename.concat source_root path ^ ".rp"

let show_module_path (path : string list) : string = String.concat "." path

let import_error ?(detail : string option) ~(diags : Diagnostic.sink)
    (import : Ast.import) (headline : string) : unit =
  let d = Diagnostic.error headline |> Diagnostic.at import.Ast.span in
  Diagnostic.emit diags
    (match detail with Some s -> Diagnostic.detail s d | None -> d)

(* The stack holds everything still loading so the cycle is the tail of it
   starting where the path shows up again *)
let import_cycle (stack : string list list) (path : string list) :
    string list list =
  let rec from_path = function
    | [] -> []
    | current :: _ as paths when current = path -> paths
    | _ :: paths -> from_path paths
  in
  from_path stack @ [ path ]

let show_import_cycle (paths : string list list) : string =
  match paths with
  | [] -> ""
  | first :: rest ->
      let hop path = "    imports " ^ show_module_path path ^ "\n" in
      "  module " ^ show_module_path first ^ "\n"
      ^ String.concat "" (List.map hop rest)

let locate_module ~(read_file : string -> string) (source_root : string)
    (path : string list) : string option =
  let file = file_of_path source_root path in
  match read_file file with exception Sys_error _ -> None | _ -> Some file

let load ~(diags : Diagnostic.sink) ~(read_file : string -> string)
    ~(root_filename : string) : t =
  (* Dirname of a bare filename is "." which would stick "./" on the front of
     every import *)
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
        (* The root is still loading when it imports itself back but no import
           of its own started that *)
        (match imported_by with
        | None -> ()
        | Some import ->
            let detail = show_import_cycle (import_cycle stack path) in
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
        (* A file that won't read still becomes a module so looking it up by ID
           later never fails *)
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
        let loaded filename src =
          let source, ast =
            parse_source ~diags (fresh_file_id ()) filename src
          in
          let stack = stack @ [ path ] in
          let dependencies =
            ast.Ast.imports
            |> List.map (fun import ->
                let target = load_module stack (Some import) import.Ast.path in
                { import; target })
          in
          record [ { source; ast } ] dependencies
        in
        (* The root has no import to blame so it throws instead *)
        let missing () =
          match imported_by with
          | None -> raise (Sys_error (file_of_path source_root path))
          | Some import ->
              unreadable import ("module not found: " ^ show_module_path path)
        in
        let not_utf8 filename =
          match imported_by with
          | None -> raise (Invalid_utf8 filename)
          | Some import ->
              unreadable import
                ("module is not valid UTF-8: " ^ show_module_path path)
        in
        (* The root came straight off the command line so there is nothing to
           look up *)
        let located =
          match imported_by with
          | None -> Some root_filename
          | Some _ -> locate_module ~read_file source_root path
        in
        match located with
        | None -> missing ()
        | Some filename ->
            let src = read_file filename in
            (* The lexer walks bytes so it would split a character in half *)
            if String.is_valid_utf_8 src then loaded filename src
            else not_utf8 filename)
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
