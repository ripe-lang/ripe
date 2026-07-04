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

let arity span ~expected ~found =
  error (Printf.sprintf "%s, found %d" expected found) |> at span
