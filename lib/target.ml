(* SPDX-License-Identifier: GPL-2.0-only *)

type t = Linux_x86_64

let host () : t =
  match Platform.host () with
  | "linux-x86_64" -> Linux_x86_64
  | _ -> failwith "no toolchain for this host"

let assembler_args (_target : t) ~output ~input : string list =
  [ Config.assembler; "--64"; "-o"; output; input ]

let linker_args (_target : t) ~output ~object_file ~runtime ~libraries :
    string list =
  [ Config.linker; "-o"; output; object_file; runtime ]
  @ List.map (fun library -> "-l" ^ library) libraries
