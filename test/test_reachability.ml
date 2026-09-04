(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

(* The first loop in the source is the one under test *)
let rec find_loop block = List.find_map find_in_item block
and find_in_item = function Ast.Expr e -> find_in_expr e | Ast.Decl _ -> None

and find_in_expr e =
  match e.Ast.desc with
  | Ast.While (label, _, body) | Ast.Loop (label, body) -> Some (label, body)
  | Ast.For (label, _, _, _, body) -> Some (label, body)
  | Ast.Block body -> find_loop body
  | Ast.If (branches, else_body) ->
      let in_branch (_, { Ast.value = b; _ }) = find_loop b in
      List.find_map in_branch branches
      |> Option.fold
           ~none:(Option.bind else_body (fun b -> find_loop b.Ast.value))
           ~some:Option.some
  | _ -> None

let breaks src =
  let body =
    match Pipeline.parse ("func f() { " ^ src ^ " }") with
    | [ Ast.Func { body; _ } ] -> body
    | _ -> failwith "expected one function"
  in
  match find_loop body with
  | Some (label, body) ->
      Printf.printf "%b\n" (Reachability.loop_has_break ?label body)
  | None -> failwith "no loop in the source"

let%expect_test "reachability: a bare break belongs to the nearest loop" =
  breaks "while true { break }";
  breaks "while true { }";
  [%expect {|
    true
    false
    |}]

let%expect_test "reachability: a break in a nested block still counts" =
  breaks "while true { { break } }";
  breaks "while true { if x { break } }";
  breaks "while true { if x { } else { break } }";
  breaks "while true { if x { } else if y { break } }";
  [%expect {|
    true
    true
    true
    true
    |}]

let%expect_test "reachability: a break inside an inner loop is not this one's" =
  breaks "while true { while y { break } }";
  breaks "while true { loop { break } }";
  breaks "while true { for i in r { break } }";
  [%expect {|
    false
    false
    false
    |}]

let%expect_test "reachability: a labeled break reaches out of an inner loop" =
  breaks "outer: while true { while y { break :outer } }";
  breaks "outer: while true { while y { break :inner } }";
  breaks "outer: while true { while y { break } }";
  breaks "outer: while true { break :outer }";
  [%expect {|
    true
    false
    false
    true
    |}]

let%expect_test "reachability: an unlabeled loop ignores a labeled break" =
  breaks "while true { while y { break :outer } }";
  [%expect {| false |}]

let%expect_test "reachability: an inner loop with the same label shadows it" =
  breaks "outer: while true { outer: while y { break :outer } }";
  [%expect {| false |}]

let%expect_test "reachability: a break hides in an initializer or a return" =
  breaks "while true { var x = if y { break } else { 1 } }";
  breaks "while true { return }";
  [%expect {|
    true
    false
    |}]

(* FIXME(f4a6): A match arm never gets walked so this answers false *)
let%expect_test "reachability: a break in a match arm is missed" =
  breaks {|while true { match n {
 1 => break
 _ => x
 } }|};
  [%expect {| false |}]
