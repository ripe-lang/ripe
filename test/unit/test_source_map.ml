(* SPDX-License-Identifier: Apache-2.0 *)

let bounds sm pos =
  let s, e = Ripe.Sourcemap.line_bounds sm pos in
  Printf.printf "(%d,%d)" s e

let%expect_test "single line" =
  let sm = Ripe.Sourcemap.create "hello" in
  let l, c = Ripe.Sourcemap.find sm 0 in
  Printf.printf "(%d,%d)" l c;
  [%expect {| (1,1) |}]

let%expect_test "multi line" =
  let sm = Ripe.Sourcemap.create "aaa\nbbb\nccc" in
  let l, c = Ripe.Sourcemap.find sm 0 in
  Printf.printf "(%d,%d)" l c;
  let l, c = Ripe.Sourcemap.find sm 4 in
  Printf.printf " (%d,%d)" l c;
  let l, c = Ripe.Sourcemap.find sm 9 in
  Printf.printf " (%d,%d)" l c;
  [%expect {| (1,1) (2,1) (3,2) |}]

let%expect_test "line_bounds across lines" =
  let sm = Ripe.Sourcemap.create "aaa\nbbb\nccc" in
  bounds sm 1;
  bounds sm 5;
  bounds sm 9;
  [%expect {| (0,3)(4,7)(8,11) |}]

let%expect_test "line_bounds trailing newline" =
  let sm = Ripe.Sourcemap.create "abc\n" in
  bounds sm 1;
  [%expect {| (0,3) |}]

let%expect_test "line_bounds empty line" =
  let sm = Ripe.Sourcemap.create "a\n\nb" in
  bounds sm 0;
  bounds sm 2;
  bounds sm 3;
  [%expect {| (0,1)(2,2)(3,4) |}]

let%expect_test "line_bounds empty source" =
  let sm = Ripe.Sourcemap.create "" in
  bounds sm 0;
  [%expect {| (0,0) |}]

let%expect_test "line_bounds only newlines" =
  let sm = Ripe.Sourcemap.create "\n\n\n" in
  bounds sm 0;
  bounds sm 1;
  bounds sm 2;
  [%expect {| (0,0)(1,1)(2,2) |}]

let%expect_test "line_bounds absolute boundaries" =
  let sm = Ripe.Sourcemap.create "aaa\nbbb\nccc" in
  bounds sm 0;
  bounds sm (String.length "aaa\nbbb\nccc");
  [%expect {| (0,3)(8,11) |}]

let%expect_test "the map hands back the text it was built from" =
  let sm = Ripe.Sourcemap.create "one\ntwo\n" in
  Printf.printf "%S\n" (Ripe.Sourcemap.src sm);
  [%expect {| "one\ntwo\n" |}]
