(* SPDX-License-Identifier: Apache-2.0 *)

open Ast

let label_name : loop_label option -> Ast.name option =
  Option.map (fun (l : loop_label) -> l.Ast.value)

(* A bare break belongs to the nearest loop and a labeled one to the name *)
let rec loop_has_break ?label (body : block) : bool =
  block_has_break ~own:true (label_name label) body

and block_has_break ~own (target : Ast.name option) (body : block) : bool =
  List.exists (block_item_has_break ~own target) body

and block_item_has_break ~own target = function
  | Expr e -> expr_has_break ~own target e
  | Decl _ -> false

and expr_has_break ~own (target : Ast.name option) (e : expr) : bool =
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

and nested_has_break (target : Ast.name option) (label : loop_label option)
    (body : block) : bool =
  target <> None
  && label_name label <> target
  && block_has_break ~own:false target body
