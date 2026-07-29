(* SPDX-License-Identifier: GPL-2.0-only *)

let qbe : string = match Sys.getenv_opt "QBE" with Some p -> p | None -> "qbe"

(* Test *)
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
