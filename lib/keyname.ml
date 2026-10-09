(* SPDX-License-Identifier: Apache-2.0 *)

(* This keeps declaration identity and readable text together for later passes *)
type t = { key : Symbol.key; base : string }
[@@deriving show { with_path = false }]

let make key base = { key; base }
let unresolved base = { key = Symbol.unresolved_key; base }
let show q = q.base
let key q = q.key
