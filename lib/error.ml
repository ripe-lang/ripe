(* SPDX-License-Identifier: GPL-2.0-only *)

(* shortcuts for the common shapes and drop to the raw pipeline for one offs *)

open Diagnostic

(* keep the headline short and let the expected/found detail ride the caret *)
let type_mismatch span ~expected ~found =
  error "type mismatch" |> at span
  |> label (Printf.sprintf "expected %s, found %s" expected found)

let undefined_name span kind name =
  error (Printf.sprintf "undefined %s: %s" kind name) |> at span

(* thing: name, terse fragment, lowercase, no trailing period *)
let named span msg name = error (Printf.sprintf "%s: %s" msg name) |> at span

(* The prev span marks the original binder so both ends are on screen. *)
let redefinition span ~prev name =
  named span "already defined" name |> secondary prev "previous definition here"

let arity span ~expected ~found =
  error (Printf.sprintf "%s, found %d" expected found) |> at span

let int_out_of_range span ~ty =
  error "integer literal out of range"
  |> at span
  |> label ("does not fit in " ^ ty)

let unsupported span msg = error (msg ^ " is not yet supported") |> at span

let internal ?span msg =
  let d =
    error "internal compiler error"
    |> detail (msg ^ "\n")
    |> help
         "this is a bug in ripec, please report it at \
          https://github.com/ripe-lang/ripe/issues"
  in
  match span with Some sp -> at sp d | None -> d
