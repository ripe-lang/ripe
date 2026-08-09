(* SPDX-License-Identifier: GPL-2.0-only *)

type t = Linux_x86_64

let host () : t =
  let input = Unix.open_process_args_in "uname" [| "uname"; "-sm" |] in
  let value = In_channel.input_all input |> String.trim in
  match Unix.close_process_in input with
  | Unix.WEXITED 0 when value = "Linux x86_64" -> Linux_x86_64
  | _ -> failwith "no toolchain for this host"

let assembler_args (_target : t) ~output ~input : string list =
  [ Config.assembler; "--64"; "-o"; output; input ]

let linker_args (_target : t) ~output ~object_file ~runtime ~libraries :
    string list =
  [ Config.linker; "-o"; output; object_file; runtime ]
  @ List.map (fun library -> "-l" ^ library) libraries
