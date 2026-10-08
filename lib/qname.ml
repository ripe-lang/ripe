(* SPDX-License-Identifier: Apache-2.0 *)

(* The key is what gets compared and the name is only for printing *)
type t = { key : Symbol.key; base : string }
[@@deriving show { with_path = false }]

let make key base = { key; base }
let unresolved base = { key = Symbol.unresolved_key; base }
let show q = q.base
let key q = q.key
