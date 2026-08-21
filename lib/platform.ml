(* SPDX-License-Identifier: Apache-2.0 *)

let normalize_architecture architecture =
  match String.lowercase_ascii architecture with
  | "amd64" -> "x86_64"
  | "aarch64" -> "arm64"
  | architecture -> architecture

let uname () =
  let input = Unix.open_process_args_in "uname" [| "uname"; "-sm" |] in
  let value = In_channel.input_all input |> String.trim in
  match Unix.close_process_in input with
  | Unix.WEXITED 0 -> value
  | _ -> failwith "cannot determine host platform"

let unix_host () =
  match String.split_all ~sep:" " ~drop:String.is_empty (uname ()) with
  | [ system; architecture ] -> (String.lowercase_ascii system, architecture)
  | _ -> failwith "cannot determine host platform"

let host () =
  let system, architecture =
    match Sys.os_type with
    | "Win32" | "Cygwin" ->
        let architecture =
          Option.value
            (Sys.getenv_opt "PROCESSOR_ARCHITECTURE")
            ~default:"x86_64"
        in
        ("windows", architecture)
    | _ -> unix_host ()
  in
  system ^ "-" ^ normalize_architecture architecture
