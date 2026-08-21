(* SPDX-License-Identifier: Apache-2.0 *)

let encode_component component =
  string_of_int (String.length component) ^ component

let declaration module_path name =
  "_R" ^ String.concat "" (List.map encode_component (module_path @ [ name ]))
