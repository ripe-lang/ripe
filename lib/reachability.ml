(* SPDX-License-Identifier: Apache-2.0 *)

open Ast

let label_name = Option.map (fun l -> l.Ast.value)

(* A bare break belongs to the nearest loop and a labeled one to the name *)
let rec loop_has_break ?label body =
  block_has_break ~own:true (label_name label) body

and block_has_break ~own target body =
  List.exists (block_item_has_break ~own target) body

and block_item_has_break ~own target = function
  | Expr e -> expr_has_break ~own target e
  | Decl _ -> false

and expr_has_break ~own target e =
  match e.desc with
  | ErrorExpr -> false
  | Break (None, _) -> own
  | Break (Some l, _) -> target <> None && target = Some l.Ast.value
  | Block body -> block_has_break ~own target body
  | If (branches, else_body) ->
      Option.exists
        (fun { Ast.value = b; _ } -> block_has_break ~own target b)
        else_body
      || List.exists
           (fun (_, { Ast.value = b; _ }) -> block_has_break ~own target b)
           branches
  | Binding (_, _, _, _, init) ->
      Option.exists (expr_has_break ~own target) init
  | Return e -> Option.exists (expr_has_break ~own target) e
  | While (label, _, body) | Loop (label, body) ->
      nested_has_break target label body
  | For (label, _, _, _, body) -> nested_has_break target label body
  | PairAssign _ -> false
  | _ -> false

and nested_has_break target label body =
  target <> None
  && label_name label <> target
  && block_has_break ~own:false target body
