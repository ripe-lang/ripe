(* SPDX-License-Identifier: Apache-2.0 *)

(* prints line_bounds as a tuple so the cases below can check start and end *)
let bounds sm pos =
  let s, e = Ripe.Sourcemap.line_bounds sm pos in
  Printf.printf "(%d,%d)" s e

(* "hello" offset 0 = (1,1) *)
let%expect_test "single line" =
  let sm = Ripe.Sourcemap.create ~base:0 "hello" in
  let l, c = Ripe.Sourcemap.lookup sm 0 in
  Printf.printf "(%d,%d)" l c;
  [%expect {| (1,1) |}]

(* "aaa\nbbb\nccc" offset 0=(1,1) 4=(2,1) 9=(3,2) *)
let%expect_test "multi line" =
  let sm = Ripe.Sourcemap.create ~base:0 "aaa\nbbb\nccc" in
  let l, c = Ripe.Sourcemap.lookup sm 0 in
  Printf.printf "(%d,%d)" l c;
  let l, c = Ripe.Sourcemap.lookup sm 4 in
  Printf.printf " (%d,%d)" l c;
  let l, c = Ripe.Sourcemap.lookup sm 9 in
  Printf.printf " (%d,%d)" l c;
  [%expect {| (1,1) (2,1) (3,2) |}]

(* "aaa\nbbb\nccc" first, middle, and last line (no trailing newline) *)
let%expect_test "line_bounds across lines" =
  let sm = Ripe.Sourcemap.create ~base:0 "aaa\nbbb\nccc" in
  bounds sm 1;
  bounds sm 5;
  bounds sm 9;
  [%expect {| (0,3)(4,7)(8,11) |}]

(* trailing newline is excluded from the last line *)
let%expect_test "line_bounds trailing newline" =
  let sm = Ripe.Sourcemap.create ~base:0 "abc\n" in
  bounds sm 1;
  [%expect {| (0,3) |}]

(* "a\n\nb" the empty middle line is a zero-width range *)
let%expect_test "line_bounds empty line" =
  let sm = Ripe.Sourcemap.create ~base:0 "a\n\nb" in
  bounds sm 0;
  bounds sm 2;
  bounds sm 3;
  [%expect {| (0,1)(2,2)(3,4) |}]

(* an empty source has one empty line *)
let%expect_test "line_bounds empty source" =
  let sm = Ripe.Sourcemap.create ~base:0 "" in
  bounds sm 0;
  [%expect {| (0,0) |}]

(* a source of only newlines is all empty lines *)
let%expect_test "line_bounds only newlines" =
  let sm = Ripe.Sourcemap.create ~base:0 "\n\n\n" in
  bounds sm 0;
  bounds sm 1;
  bounds sm 2;
  [%expect {| (0,0)(1,1)(2,2) |}]

(* first byte and one past the last byte both resolve to a line *)
let%expect_test "line_bounds absolute boundaries" =
  let sm = Ripe.Sourcemap.create ~base:0 "aaa\nbbb\nccc" in
  bounds sm 0;
  bounds sm (String.length "aaa\nbbb\nccc");
  [%expect {| (0,3)(8,11) |}]

let%expect_test "the map hands back the text it was built from" =
  let sm = Ripe.Sourcemap.create ~base:0 "one\ntwo\n" in
  Printf.printf "%S\n" (Ripe.Sourcemap.src sm);
  [%expect {| "one\ntwo\n" |}]

let%expect_test "a trailing newline counts the empty line after it" =
  let count src =
    Ripe.Sourcemap.line_count (Ripe.Sourcemap.create ~base:0 src)
  in
  Printf.printf "%d %d %d %d %d\n" (count "") (count "a") (count "a\n")
    (count "a\nb") (count "a\nb\n");
  [%expect {| 1 1 2 2 3 |}]

let%expect_test "a global offset comes back relative to the file" =
  let sm = Ripe.Sourcemap.create ~base:100 "one\ntwo\n" in
  Printf.printf "%d %d %d\n"
    (Ripe.Sourcemap.rel sm 100)
    (Ripe.Sourcemap.rel sm 104)
    (Ripe.Sourcemap.rel sm 107);
  [%expect {| 0 4 7 |}]

let%expect_test "a based map still finds the right line and column" =
  let sm = Ripe.Sourcemap.create ~base:100 "one\ntwo\nthree\n" in
  let show pos =
    let line, col = Ripe.Sourcemap.lookup sm pos in
    Printf.printf "%d -> %d:%d\n" pos line col
  in
  show 100;
  show 104;
  show 108;
  [%expect {|
    100 -> 1:1
    104 -> 2:1
    108 -> 3:1
    |}]
