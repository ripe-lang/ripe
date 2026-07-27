(* SPDX-License-Identifier: GPL-2.0-only *)

let substring_offset src sub =
  try Str.search_forward (Str.regexp_string sub) src 0
  with Not_found -> failwith (Printf.sprintf "substring not found: %S" sub)

let span src sub =
  let lo = substring_offset src sub in
  Ripe.Span.make 0 lo (lo + String.length sub)

let point src sub =
  let offset = substring_offset src sub in
  Ripe.Span.make 0 offset offset

let replace s old rep = Str.global_replace (Str.regexp_string old) rep s
