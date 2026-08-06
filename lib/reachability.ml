(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast

(* A break inside an inner loop stops that loop and not this one *)
let rec loop_has_break (body : block) : bool =
  List.exists block_item_has_break body

and block_item_has_break = function
  | Expr e -> expr_has_break e
  | Decl _ -> false

and expr_has_break (e : expr) : bool =
  match e.desc with
  | ErrorExpr -> false
  | Break -> true
  | Block body -> loop_has_break body
  | If (branches, else_body) ->
      Option.fold ~none:false
        ~some:(fun { Ast.value = b; _ } -> loop_has_break b)
        else_body
      || List.exists
           (fun (_, { Ast.value = b; _ }) -> loop_has_break b)
           branches
  (* A break can hide in a value if that a binding or return holds *)
  | Binding (_, _, _, _, init) ->
      Option.fold ~none:false ~some:expr_has_break init
  | Return e -> Option.fold ~none:false ~some:expr_has_break e
  (* A nested loop owns its own breaks *)
  | While _ | For _ -> false
  | PairAssign _ -> false
  | _ -> false
