(* SPDX-License-Identifier: Apache-2.0 *)

let executable_dir () =
  try Filename.dirname (Unix.realpath Sys.executable_name)
  with Unix.Unix_error _ -> Filename.dirname Sys.executable_name

let nonempty_environment name =
  match Sys.getenv_opt name with
  | Some path when not (String.is_empty (String.trim path)) -> Some path
  | _ -> None

let toolchain_roots () =
  let exe_dir = executable_dir () in
  let source_root = Filename.concat exe_dir "../../../" in
  let installed_root = Filename.concat exe_dir "../lib/ripe/" in
  let shared_root = Filename.concat exe_dir "../share/ripe/" in
  let toolchain = Filename.concat "toolchain" (Platform.host ()) in
  match nonempty_environment "RIPE_TOOLCHAIN" with
  | Some root -> [ root ]
  | None ->
      [
        Filename.concat (Sys.getcwd ()) (Filename.concat "vendor" toolchain);
        Filename.concat source_root (Filename.concat "vendor" toolchain);
        Filename.concat exe_dir "..";
        Filename.concat installed_root toolchain;
        Filename.concat shared_root toolchain;
      ]

let bundled_tool name =
  toolchain_roots ()
  |> List.map (fun root -> Filename.concat root (Filename.concat "bin" name))
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

let configured_tool name =
  Option.map (fun path -> (name, path)) (nonempty_environment name)

let resolve_configured_tool (name, path) =
  let resolved =
    if Sys.file_exists path then Some path else find_on_path path
  in
  match resolved with
  | Some path -> path
  | None -> failwith (name ^ " points to a missing tool: " ^ path)

let realpath_or_original path =
  try Unix.realpath path with Unix.Unix_error _ -> path

let default_tool bundled fallback =
  match bundled_tool bundled with
  | Some path -> realpath_or_original path
  | None -> Option.value (find_on_path fallback) ~default:fallback

let resolve_tool ~environment ~bundled ~fallback () =
  match configured_tool environment with
  | Some configured -> realpath_or_original (resolve_configured_tool configured)
  | None -> default_tool bundled fallback

let qbe () = resolve_tool ~environment:"QBE" ~bundled:"qbe" ~fallback:"qbe" ()

let assembler () =
  resolve_tool ~environment:"RIPE_AS" ~bundled:"as" ~fallback:"as" ()

let linker () =
  resolve_tool ~environment:"RIPE_CC" ~bundled:"cc" ~fallback:"cc" ()

(* An installed compiler finds the object through its dune install site *)
let runtime_in_sites () =
  List.map (fun dir -> Filename.concat dir "panic.o") Ripesites.Sites.runtime

(* A fresh build has no install site so look beside the binary *)
let runtime_near_exe () =
  let exe =
    try Unix.realpath Sys.executable_name
    with Unix.Unix_error _ -> Sys.executable_name
  in
  let bin_dir = Filename.dirname exe in
  [
    Filename.concat bin_dir
      (Filename.concat Filename.parent_dir_name "runtime/panic.o");
    Filename.concat bin_dir "../lib/ripe/runtime/panic.o";
  ]

let runtime_object () =
  (* An explicit RIPE_RUNTIME wins so a user can force a path *)
  let override = Option.to_list (nonempty_environment "RIPE_RUNTIME") in
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
    Option.exists is_source_root_parent root_filename
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

let split_paths ?(sep = path_separator) v =
  String.split_all ~sep:(String.of_char sep)
    ~drop:(fun p -> String.is_empty (String.trim p))
    v

let search_roots ?root_filename () =
  let standard = standard_library_roots ?root_filename () in
  match Sys.getenv_opt "RIPE_PATH" with
  | Some v -> split_paths v @ standard
  | None -> standard
