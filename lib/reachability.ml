(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast

(* a break inside an inner loop stops that loop and not this one *)
let rec loop_has_break (body : block) : bool = List.exists expr_has_break body

and expr_has_break (e : expr) : bool =
  match e.desc with
  | Break -> true
  | Block body -> loop_has_break body
  | If (branches, else_body) ->
      Option.fold ~none:false ~some:loop_has_break else_body
      || List.exists (fun (_, b) -> loop_has_break b) branches
  (* a break can hide in a value if that a binding or return holds *)
  | Binding (_, _, _, _, init) ->
      Option.fold ~none:false ~some:expr_has_break init
  | Return e -> Option.fold ~none:false ~some:expr_has_break e
  (* a nested loop owns its own breaks *)
  | While _ | For _ -> false
  | _ -> false
