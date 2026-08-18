(* SPDX-License-Identifier: Apache-2.0 *)

let substring_offset src sub =
  match String.find_first ~sub src with
  | Some offset -> offset
  | None -> failwith (Printf.sprintf "substring not found: %S" sub)

let span src sub =
  let lo = substring_offset src sub in
  Ripe.Span.make lo (lo + String.length sub)

let point src sub =
  let offset = substring_offset src sub in
  Ripe.Span.make offset offset

let replace s old rep = String.replace_all ~sub:old ~by:rep s
