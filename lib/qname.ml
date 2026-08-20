(* SPDX-License-Identifier: Apache-2.0 *)

(* The key is what gets compared and the path is only for printing *)
type t = { key : Symbol.key; path : string list; base : string }
[@@deriving show { with_path = false }]

(* Nothing got resolved here so this can be printed but never looked up *)
let unresolved_key : Symbol.key = Symbol.make_key (-1) (-1)

let make (key : Symbol.key) (path : string list) (base : string) : t =
  { key; path; base }

let unresolved (base : string) : t = { key = unresolved_key; path = []; base }
let show (q : t) : string = String.concat "." (q.path @ [ q.base ])
let key (q : t) : Symbol.key = q.key

(* Reading your own module's path back in every message is noise *)
let show_in current q = if q.path = current then q.base else show q
