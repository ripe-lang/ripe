(* SPDX-License-Identifier: Apache-2.0 *)

type t = { assembler : string; linker : string }

let host () =
  match Platform.host () with
  | "linux-x86_64" ->
      { assembler = Config.assembler (); linker = Config.linker () }
  | _ -> failwith "no toolchain for this host"

let assembler_args target ~output ~input =
  [ target.assembler; "--64"; "-o"; output; input ]

let linker_args target ~output ~object_file ~runtime ~libraries =
  [ target.linker; "-o"; output; object_file; runtime ]
  @ List.map (fun library -> "-l" ^ library) libraries
