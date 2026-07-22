(* SPDX-License-Identifier: GPL-2.0-only *)

open Ast

(* a break inside an inner loop stops that loop and not this one *)
let rec loop_has_break stmts = List.exists stmt_has_break stmts

and stmt_has_break s =
  match s.sdesc with
  | Break -> true
  | Block body -> loop_has_break body
  | If (branches, else_body) ->
      loop_has_break else_body
      || List.exists (fun (_, b) -> loop_has_break b) branches
  | _ -> false

(* every path through the stmts ends in a return or another terminator *)
let rec stmts_return (diverges : expr -> bool) (stmts : stmt list) : bool =
  List.exists (stmt_returns diverges) stmts

and stmt_returns (diverges : expr -> bool) (s : stmt) : bool =
  match s.sdesc with
  | Return _ -> true
  (* a call to a never function doesn't come back so it ends the path like a return *)
  | Expr e -> diverges e
  | Block body -> stmts_return diverges body
  | If (branches, else_body) ->
      else_body <> []
      && stmts_return diverges else_body
      && List.for_all (fun (_, body) -> stmts_return diverges body) branches
  (* a while true with no break loops forever so the code after it never runs *)
  | While ({ desc = Bool true; _ }, body) -> not (loop_has_break body)
  | _ -> false
