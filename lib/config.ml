(* SPDX-License-Identifier: Apache-2.0 *)

open Option.Syntax

let realpath_or_original path =
  try Unix.realpath path with Unix.Unix_error _ -> path

let executable_dir () =
  Filename.dirname (realpath_or_original Sys.executable_name)

let nonempty_environment name =
  match Sys.getenv_opt name with
  | Some path when not (String.is_empty (String.trim path)) -> Some path
  | _ -> None

let is_executable_file path =
  Sys.file_exists path
  &&
  match Unix.access path [ Unix.X_OK ] with
  | () -> true
  | exception Unix.Unix_error _ -> false

let find_on_path name =
  let* path = Sys.getenv_opt "PATH" in
  path
  |> String.split_on_char (if Sys.win32 then ';' else ':')
  |> List.find_map (fun dir ->
      let candidate = Filename.concat dir name in
      if is_executable_file candidate then Some candidate else None)

(* The env var beats the bundled tool and PATH *)
let find_tool environment name =
  match nonempty_environment environment with
  | Some path -> (
      let found =
        if Sys.file_exists path then Some path else find_on_path path
      in
      match found with
      | Some path -> realpath_or_original path
      | None -> failwith (environment ^ " not found: " ^ path))
  | None -> (
      let dir = executable_dir () in
      let toolchain = Filename.concat "toolchain" (Platform.host ()) in
      let roots =
        match nonempty_environment "RIPE_TOOLCHAIN" with
        | Some root -> [ root ]
        | None ->
            [
              Filename.concat (Sys.getcwd ())
                (Filename.concat "vendor" toolchain);
              Filename.concat dir (Filename.concat "../../../vendor" toolchain);
              Filename.concat dir "..";
              Filename.concat dir (Filename.concat "../lib/ripe" toolchain);
              Filename.concat dir (Filename.concat "../share/ripe" toolchain);
            ]
      in
      roots
      |> List.map (fun root ->
          Filename.concat root (Filename.concat "bin" name))
      |> List.find_opt Sys.file_exists
      |> function
      | Some path -> realpath_or_original path
      | None -> Option.value (find_on_path name) ~default:name)

let qbe () = find_tool "QBE" "qbe"
let assembler () = find_tool "RIPE_AS" "as"
let linker () = find_tool "RIPE_CC" "cc"

type toolchain = { assembler : string list; linker : string list }

let toolchain () =
  match Platform.host () with
  | "linux-x86_64" ->
      { assembler = [ assembler (); "--64" ]; linker = [ linker () ] }
  | _ -> failwith "no toolchain for this host"

(* A fresh build has no install site so we also look beside the binary *)
let runtime_object () =
  let dir = executable_dir () in
  let candidates =
    Option.to_list (nonempty_environment "RIPE_RUNTIME")
    @ List.map
        (fun site -> Filename.concat site "panic.o")
        Ripesites.Sites.runtime
    @ [
        Filename.concat dir
          (Filename.concat Filename.parent_dir_name "runtime/panic.o");
        Filename.concat dir "../lib/ripe/runtime/panic.o";
      ]
  in
  match List.find_opt Sys.file_exists candidates with
  | Some path -> path
  | None -> failwith "runtime not found (set RIPE_RUNTIME)"
