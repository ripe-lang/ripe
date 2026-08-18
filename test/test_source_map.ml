(* SPDX-License-Identifier: Apache-2.0 *)

(* "hello" offset 0 = (1,1) *)
let%expect_test "single line" =
  let sm = Ripe.Source_map.create ~base:0 "hello" in
  let l, c = Ripe.Source_map.lookup sm 0 in
  Printf.printf "(%d,%d)" l c;
  [%expect {| (1,1) |}]

(* "aaa\nbbb\nccc" offset 0=(1,1) 4=(2,1) 9=(3,2) *)
let%expect_test "multi line" =
  let sm = Ripe.Source_map.create ~base:0 "aaa\nbbb\nccc" in
  let l, c = Ripe.Source_map.lookup sm 0 in
  Printf.printf "(%d,%d)" l c;
  let l, c = Ripe.Source_map.lookup sm 4 in
  Printf.printf " (%d,%d)" l c;
  let l, c = Ripe.Source_map.lookup sm 9 in
  Printf.printf " (%d,%d)" l c;
  [%expect {| (1,1) (2,1) (3,2) |}]

(* prints line_bounds as a tuple so the cases below can check start and end *)
let bounds sm pos =
  let s, e = Ripe.Source_map.line_bounds sm pos in
  Printf.printf "(%d,%d)" s e

(* "aaa\nbbb\nccc" first, middle, and last line (no trailing newline) *)
let%expect_test "line_bounds across lines" =
  let sm = Ripe.Source_map.create ~base:0 "aaa\nbbb\nccc" in
  bounds sm 1;
  bounds sm 5;
  bounds sm 9;
  [%expect {| (0,3)(4,7)(8,11) |}]

(* trailing newline is excluded from the last line *)
let%expect_test "line_bounds trailing newline" =
  let sm = Ripe.Source_map.create ~base:0 "abc\n" in
  bounds sm 1;
  [%expect {| (0,3) |}]

(* "a\n\nb" the empty middle line is a zero-width range *)
let%expect_test "line_bounds empty line" =
  let sm = Ripe.Source_map.create ~base:0 "a\n\nb" in
  bounds sm 0;
  bounds sm 2;
  bounds sm 3;
  [%expect {| (0,1)(2,2)(3,4) |}]

(* an empty source has one empty line *)
let%expect_test "line_bounds empty source" =
  let sm = Ripe.Source_map.create ~base:0 "" in
  bounds sm 0;
  [%expect {| (0,0) |}]

(* a source of only newlines is all empty lines *)
let%expect_test "line_bounds only newlines" =
  let sm = Ripe.Source_map.create ~base:0 "\n\n\n" in
  bounds sm 0;
  bounds sm 1;
  bounds sm 2;
  [%expect {| (0,0)(1,1)(2,2) |}]

(* first byte and one past the last byte both resolve to a line *)
let%expect_test "line_bounds absolute boundaries" =
  let sm = Ripe.Source_map.create ~base:0 "aaa\nbbb\nccc" in
  bounds sm 0;
  bounds sm (String.length "aaa\nbbb\nccc");
  [%expect {| (0,3)(8,11) |}]
