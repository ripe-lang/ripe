(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let show lo hi =
  let s = Span.make lo hi in
  Printf.printf "make %d %d -> lo %d hi %d\n" lo hi (Span.lo s) (Span.hi s)

let%expect_test "span: a span gives back the range it was made from" =
  show 0 0;
  show 0 1;
  show 5 9;
  show 100 100;
  [%expect
    {|
    make 0 0 -> lo 0 hi 0
    make 0 1 -> lo 0 hi 1
    make 5 9 -> lo 5 hi 9
    make 100 100 -> lo 100 hi 100
    |}]

let%expect_test "span: a span at the end of the offset space still round trips"
    =
  show (Span.max_offset - 1) Span.max_offset;
  show Span.max_offset Span.max_offset;
  [%expect
    {|
    make 2147483645 2147483646 -> lo 2147483645 hi 2147483646
    make 2147483646 2147483646 -> lo 2147483646 hi 2147483646
    |}]

let%expect_test "span: the dummy span sits below every real offset" =
  Printf.printf "lo %d hi %d\n" (Span.lo Span.dummy) (Span.hi Span.dummy);
  Printf.printf "before zero %b\n" (Span.lo Span.dummy < Span.lo (Span.make 0 0));
  [%expect {|
    lo -1 hi -1
    before zero true
    |}]

let%expect_test "span: two spans compare on where they start" =
  let ordered = compare (Span.make 3 9) (Span.make 5 6) < 0 in
  let same_start = compare (Span.make 3 4) (Span.make 3 9) < 0 in
  Printf.printf "earlier first %b shorter first %b\n" ordered same_start;
  [%expect {| earlier first true shorter first true |}]

let%expect_test "span: printing shows the pair" =
  print_endline (Span.show (Span.make 2 7));
  print_endline (Span.show Span.dummy);
  [%expect {|
    (2,7)
    (-1,-1)
    |}]

let%expect_test "span: the table hashes on the start offset" =
  let table = Span.Table.create 8 in
  Span.Table.replace table (Span.make 4 8) "first";
  Span.Table.replace table (Span.make 4 9) "second";
  Span.Table.replace table (Span.make 5 8) "third";
  let get lo hi =
    Option.value (Span.Table.find_opt table (Span.make lo hi)) ~default:"none"
  in
  Printf.printf "%s %s %s %d\n" (get 4 8) (get 4 9) (get 5 8)
    (Span.Table.length table);
  [%expect {| first second third 3 |}]

let%expect_test "span: a zero width span is its own start and end" =
  let s = Span.make 12 12 in
  Printf.printf "lo %d hi %d width %d\n" (Span.lo s) (Span.hi s)
    (Span.hi s - Span.lo s);
  [%expect {| lo 12 hi 12 width 0 |}]

let%expect_test "span: a span may run the whole width of the offset space" =
  show 0 Span.max_offset;
  [%expect {| make 0 2147483646 -> lo 0 hi 2147483646 |}]
