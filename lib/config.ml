(* SPDX-License-Identifier: GPL-2.0-only *)

let qbe = match Sys.getenv_opt "QBE" with Some p -> p | None -> "qbe"

let find_runtime_in_sites () =
  let object_in dir = Filename.concat dir "panic.o" in
  List.find_opt Sys.file_exists (List.map object_in Ripe_sites.Sites.runtime)

let runtime_object () =
  match (Sys.getenv_opt "RIPE_RUNTIME", find_runtime_in_sites ()) with
  | Some path, _ -> path
  | None, Some path -> path
  | None, None ->
      failwith
        "cannot find the ripe runtime object (set RIPE_RUNTIME to its path)"
