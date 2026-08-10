(* SPDX-License-Identifier: GPL-2.0-only *)

let executable_dir () =
  try Filename.dirname (Unix.realpath Sys.executable_name)
  with _ -> Filename.dirname Sys.executable_name

let toolchain_roots () =
  let exe_dir = executable_dir () in
  let source_root = Filename.concat exe_dir "../../../" in
  let installed_root = Filename.concat exe_dir "../lib/ripe/" in
  let shared_root = Filename.concat exe_dir "../share/ripe/" in
  match Sys.getenv_opt "RIPE_TOOLCHAIN" with
  | Some root when String.trim root <> "" -> [ root ]
  | _ ->
      [
        Filename.concat (Sys.getcwd ()) "vendor/toolchain/linux-x86_64";
        Filename.concat source_root "vendor/toolchain/linux-x86_64";
        Filename.concat exe_dir "..";
        Filename.concat installed_root "toolchain/linux-x86_64";
        Filename.concat shared_root "toolchain/linux-x86_64";
      ]

let bundled_tool name =
  toolchain_roots ()
  |> List.map (fun root -> Filename.concat root (Filename.concat "bin" name))
  |> List.find_opt Sys.file_exists

let bundled_library_dir () =
  toolchain_roots ()
  |> List.map (fun root -> Filename.concat root "lib")
  |> List.find_opt Sys.file_exists

let is_executable_file path =
  Sys.file_exists path
  &&
  match Unix.access path [ Unix.X_OK ] with
  | () -> true
  | exception Unix.Unix_error _ -> false

let find_on_path name =
  match Sys.getenv_opt "PATH" with
  | None -> None
  | Some path ->
      path
      |> String.split_on_char (if Sys.win32 then ';' else ':')
      |> List.find_map (fun dir ->
          let candidate = Filename.concat dir name in
          if is_executable_file candidate then Some candidate else None)

let nonempty_environment name =
  match Sys.getenv_opt name with
  | Some path when String.trim path <> "" -> Some path
  | _ -> None

let configured_tool name =
  Option.map (fun path -> (name, path)) (nonempty_environment name)

let resolve_configured_tool (name, path) =
  let resolved =
    if Sys.file_exists path then Some path else find_on_path path
  in
  match resolved with
  | Some path -> path
  | None -> failwith (name ^ " points to a missing tool: " ^ path)

let realpath_or_original path = try Unix.realpath path with _ -> path

let default_tool bundled fallback =
  match bundled_tool bundled with
  | Some path -> realpath_or_original path
  | None -> Option.value (find_on_path fallback) ~default:fallback

let resolve_tool ~environment ~bundled ~fallback () =
  match configured_tool environment with
  | Some configured -> realpath_or_original (resolve_configured_tool configured)
  | None -> default_tool bundled fallback

let qbe : string =
  resolve_tool ~environment:"QBE" ~bundled:"qbe" ~fallback:"qbe" ()

let assembler : string =
  resolve_tool ~environment:"RIPE_AS" ~bundled:"as" ~fallback:"as" ()

let lld : string =
  resolve_tool ~environment:"RIPE_LLD" ~bundled:"ld.lld" ~fallback:"ld.lld" ()

let lld_library_path =
  match configured_tool "RIPE_LLD" with
  | Some _ -> None
  | None -> bundled_library_dir ()

(* An installed compiler finds the object through its dune install site *)
let runtime_in_sites () : string list =
  List.map (fun dir -> Filename.concat dir "panic.o") Ripe_sites.Sites.runtime

(* A fresh build has no install site so look beside the binary *)
let runtime_near_exe () : string list =
  let exe =
    try Unix.realpath Sys.executable_name with _ -> Sys.executable_name
  in
  let bin_dir = Filename.dirname exe in
  [
    Filename.concat bin_dir
      (Filename.concat Filename.parent_dir_name "runtime/panic.o");
    Filename.concat bin_dir "../lib/ripe/runtime/panic.o";
  ]

let runtime_object () : string =
  (* An explicit RIPE_RUNTIME wins so a user can force a path *)
  let override =
    match Sys.getenv_opt "RIPE_RUNTIME" with
    | Some p when String.trim p <> "" -> [ p ]
    | _ -> []
  in
  let candidates = override @ runtime_in_sites () @ runtime_near_exe () in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None ->
      failwith
        "cannot find the ripe runtime object (set RIPE_RUNTIME to its path)"

let standard_library_roots ?root_filename () =
  let exe_dir = executable_dir () in
  let source_root =
    realpath_or_original (Filename.concat exe_dir "../../../")
  in
  let shared_root = Filename.concat exe_dir "../share/ripe/" in
  let is_source_root_parent filename =
    let filename = realpath_or_original filename in
    source_root <> Filename.dir_sep
    && (filename = source_root
       || String.starts_with ~prefix:(source_root ^ Filename.dir_sep) filename)
  in
  let source_root_is_parent =
    Option.value (Option.map is_source_root_parent root_filename) ~default:false
  in
  let source_roots =
    if source_root = Filename.dir_sep || source_root_is_parent then []
    else [ source_root ]
  in
  source_roots @ [ shared_root ]
  |> List.filter (fun root ->
      let standard = Filename.concat root "std" in
      Sys.file_exists standard && Sys.is_directory standard)

let path_separator = if Sys.win32 then ';' else ':'

let split_paths ?(sep = path_separator) (v : string) : string list =
  String.split_on_char sep v |> List.filter (fun p -> String.trim p <> "")

let search_roots ?root_filename () : string list =
  let standard = standard_library_roots ?root_filename () in
  match Sys.getenv_opt "RIPE_PATH" with
  | Some v -> split_paths v @ standard
  | None -> standard
