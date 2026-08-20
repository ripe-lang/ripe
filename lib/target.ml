(* SPDX-License-Identifier: Apache-2.0 *)

type t = Linux_x86_64

let host () =
  match Platform.host () with
  | "linux-x86_64" -> Linux_x86_64
  | _ -> failwith "no toolchain for this host"

let assembler_args _target ~output ~input =
  [ Config.assembler; "--64"; "-o"; output; input ]

let linker_args _target ~output ~object_file ~runtime ~libraries =
  [ Config.linker; "-o"; output; object_file; runtime ]
  @ List.map (fun library -> "-l" ^ library) libraries
