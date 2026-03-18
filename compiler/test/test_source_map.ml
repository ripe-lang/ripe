(* SPDX-License-Identifier: GPL-2.0-only *)

(* "hello"  offset 0 = (1,1) *)
let%expect_test "single line" =
  let sm = Ripe.Source_map.create "hello" in
  let l, c = Ripe.Source_map.lookup sm 0 in
  Printf.printf "(%d,%d)" l c;
  [%expect {| (1,1) |}]

(* "aaa\nbbb\nccc"  offset 0=(1,1)  4=(2,1)  9=(3,2) *)
let%expect_test "multi line" =
  let sm = Ripe.Source_map.create "aaa\nbbb\nccc" in
  let l, c = Ripe.Source_map.lookup sm 0 in
  Printf.printf "(%d,%d)" l c;
  let l, c = Ripe.Source_map.lookup sm 4 in
  Printf.printf " (%d,%d)" l c;
  let l, c = Ripe.Source_map.lookup sm 9 in
  Printf.printf " (%d,%d)" l c;
  [%expect {| (1,1) (2,1) (3,2) |}]

(* "let x = 5\nreturn x"  span {4,15} = (1,5)-(2,6) *)
let%expect_test "span across lines" =
  let sm = Ripe.Source_map.create "let x = 5\nreturn x" in
  let span = { Ripe.Ast.lo = 4; hi = 15 } in
  let sl, sc, el, ec = Ripe.Source_map.span_to_locs sm span in
  Printf.printf "(%d,%d)-(%d,%d)" sl sc el ec;
  [%expect {| (1,5)-(2,6) |}]
