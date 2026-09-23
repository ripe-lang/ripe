(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let src = "one\ntwo\nthree\nfour\n"
let second_base = String.length src

(* The two files share one offset space so a span picks out its own *)
let source_of pos =
  if pos < second_base then ("main.rp", Sourcemap.create ~base:0 src)
  else ("other.rp", Sourcemap.create ~base:second_base src)

let line_starts = [| 0; 4; 8; 14 |]
let offset line col = line_starts.(line - 1) + col - 1
let at line col = Span.make (offset line col) (offset line col + 1)

let in_other line col =
  let lo = second_base + offset line col in
  Span.make lo (lo + 1)

let fires t func span =
  Panictable.enter_func t func;
  ignore (Panictable.record t span)

let dump t =
  let strings = Panictable.strings t in
  List.iteri (fun i s -> Printf.printf "string %d %S\n" i s) strings;
  List.iteri
    (fun i (s : Panictable.site) ->
      Printf.printf "site %d file %d line %d col %d func %d\n" i s.file s.line
        s.col s.func)
    (Panictable.sites t)

let%expect_test "panictable: one site per place a check fires" =
  let t = Panictable.create ~source_of in
  Panictable.enter_func t "main";
  let first = Panictable.record t (at 1 1) in
  let second = Panictable.record t (at 2 3) in
  Printf.printf "%d %d\n" first second;
  dump t;
  [%expect
    {|
    0 1
    string 0 "main"
    string 1 "main.rp"
    site 0 file 5 line 1 col 1 func 0
    site 1 file 5 line 2 col 3 func 0
    |}]

let%expect_test "panictable: two checks on one spot share an entry" =
  let t = Panictable.create ~source_of in
  Panictable.enter_func t "main";
  let first = Panictable.record t (at 2 1) in
  let again = Panictable.record t (at 2 1) in
  let other = Panictable.record t (at 2 2) in
  Printf.printf "%d %d %d\n" first again other;
  dump t;
  [%expect
    {|
    0 0 1
    string 0 "main"
    string 1 "main.rp"
    site 0 file 5 line 2 col 1 func 0
    site 1 file 5 line 2 col 2 func 0
    |}]

let%expect_test "panictable: the same spot in two functions is two entries" =
  let t = Panictable.create ~source_of in
  Panictable.enter_func t "first";
  Printf.printf "%d " (Panictable.record t (at 3 1));
  Panictable.enter_func t "second";
  Printf.printf "%d\n" (Panictable.record t (at 3 1));
  dump t;
  [%expect
    {|
    0 1
    string 0 "first"
    string 1 "main.rp"
    string 2 "second"
    site 0 file 6 line 3 col 1 func 0
    site 1 file 6 line 3 col 1 func 14
    |}]

let%expect_test "panictable: a repeated name is stored once" =
  let t = Panictable.create ~source_of in
  fires t "main" (at 1 1);
  fires t "helper" (at 2 1);
  fires t "main" (at 3 1);
  dump t;
  [%expect
    {|
    string 0 "main"
    string 1 "main.rp"
    string 2 "helper"
    site 0 file 5 line 1 col 1 func 0
    site 1 file 5 line 2 col 1 func 13
    site 2 file 5 line 3 col 1 func 0
    |}]

let%expect_test "panictable: an offset in an import lands on that file" =
  let t = Panictable.create ~source_of in
  fires t "main" (at 2 1);
  fires t "imported" (in_other 2 1);
  dump t;
  [%expect
    {|
    string 0 "main"
    string 1 "main.rp"
    string 2 "imported"
    string 3 "other.rp"
    site 0 file 5 line 2 col 1 func 0
    site 1 file 22 line 2 col 1 func 13
    |}]

let%expect_test "panictable: a check with no location is a compiler bug" =
  let t = Panictable.create ~source_of in
  Panictable.enter_func t "main";
  (try ignore (Panictable.record t Span.dummy) with
  | Diagnostic.Errors _ -> print_endline "rejected"
  | Failure msg -> print_endline msg);
  [%expect {| rejected |}]

let%expect_test "panictable: a check outside a function is a compiler bug" =
  let t = Panictable.create ~source_of in
  (try ignore (Panictable.record t (at 1 1)) with
  | Diagnostic.Errors _ -> print_endline "rejected"
  | Failure msg -> print_endline msg);
  [%expect {| rejected |}]
