(* SPDX-License-Identifier: Apache-2.0 *)

open Option.Syntax

let realpath_or_original path =
  try Unix.realpath path with Unix.Unix_error _ -> path

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

(* The env var beats PATH *)
let find_tool environment name =
  match nonempty_environment environment with
  | Some path -> (
      let found =
        if Sys.file_exists path then Some path else find_on_path path
      in
      match found with
      | Some path -> realpath_or_original path
      | None -> failwith (environment ^ " not found: " ^ path))
  | None -> Option.value (find_on_path name) ~default:name

let qbe () = find_tool "QBE" "qbe"
let assembler () = find_tool "RIPE_AS" "as"
let linker () = find_tool "RIPE_CC" "cc"

type toolchain = { assembler : string list; linker : string list }

let toolchain () =
  match Platform.host () with
  | "linux-x86_64" ->
      { assembler = [ assembler (); "--64" ]; linker = [ linker () ] }
  | _ -> failwith "no toolchain for this host"
