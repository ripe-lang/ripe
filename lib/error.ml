(* SPDX-License-Identifier: GPL-2.0-only *)

(* Shortcuts for the common shapes and drop to the raw pipeline for one offs *)

open Diagnostic

(* The goal is to try to keep the headline short and let the expected/found
   detail ride the caret *)
let type_mismatch (span : Ast.span) ~(expected : string) ~(found : string) : t =
  error "type mismatch" |> at span
  |> label (Printf.sprintf "expected %s, found %s" expected found)

let undefined_name (span : Ast.span) (kind : string) (name : string) : t =
  error (Printf.sprintf "undefined %s: %s" kind name) |> at span

(* This is a name then a terse fragment then lowercase with no trailing
   period *)
let named (span : Ast.span) (msg : string) (name : string) : t =
  error (Printf.sprintf "%s: %s" msg name) |> at span

(* The prev span marks the original binder so both ends show up *)
let redefinition (span : Ast.span) ~(prev : Ast.span) (name : string) : t =
  named span "already defined" name |> secondary prev "previous definition here"

let arity (span : Ast.span) ~(expected : string) ~(found : int) : t =
  error (Printf.sprintf "%s, found %d" expected found) |> at span

let int_out_of_range (span : Ast.span) ~(ty : string) : t =
  error "integer literal out of range"
  |> at span
  |> label ("does not fit in " ^ ty)

let bad_operand (span : Ast.span) ~(op : string) ~(ty : string) : t =
  error (Printf.sprintf "cannot apply `%s` to %s" op ty) |> at span

let unsupported (span : Ast.span) (msg : string) : t =
  error (msg ^ " is not yet supported") |> at span

let internal ?(span : Ast.span option) (msg : string) : t =
  let d =
    error "internal compiler error"
    |> detail (msg ^ "\n")
    |> help
         "this is a bug in ripec, please report it at \
          https://github.com/ripe-lang/ripe/issues"
  in
  match span with Some sp -> at sp d | None -> d

let ice ?(span : Ast.span option) (msg : string) : 'a =
  raise (Errors [ internal ?span msg ])
