(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe
open Span_utils
open Diag

let%expect_test "single caret from a zero-width span" =
  let src = "main() {\n    break\n}\n" in
  render src
    Diagnostic.(error "invalid break statement" |> at (point src "break"));
  [%expect
    {|
    error: invalid break statement
      at <test>:2:5
            break
            ^
    |}]

let%expect_test "wide caret over a span" =
  let src = "record 12345\n" in
  render src Diagnostic.(error "expected identifier" |> at (span src "12345"));
  [%expect
    {|
    error: expected identifier
      at <test>:1:8
        record 12345
               ^~~~~
    |}]

let%expect_test "inline label after the caret" =
  let src = "wrap() = needsInt(oops)\n" in
  render src
    Diagnostic.(
      error "type mismatch"
      |> at (span src "oops")
      |> label "expected Int, found Str");
  [%expect
    {|
    error: type mismatch
      at <test>:1:19
        wrap() = needsInt(oops)
                          ^~~~ expected Int, found Str
    |}]

let%expect_test "note with a secondary location" =
  let src = "use()\ndecl()\n" in
  render src
    Diagnostic.(
      error "undefined name: foo"
      |> at (span src "use")
      |> add_note (note "declared here" |> at (span src "decl")));
  [%expect
    {|
    error: undefined name: foo
      at <test>:1:1
        use()
        ^~~
    note: declared here
      at <test>:2:1
        decl()
        ^~~~
    |}]

let%expect_test "help suggestion line" =
  let src = "var x = 1 hi\n" in
  render src
    Diagnostic.(
      error "expected `;`" |> at (point src "hi") |> help "add `;` here");
  [%expect
    {|
    error: expected `;`
      at <test>:1:11
        var x = 1 hi
                  ^
    help: add `;` here
    |}]

let%expect_test "caret aligns past a leading tab" =
  let src = "main() {\n\tvar x = 1\n}\n" in
  render src Diagnostic.(error "bad" |> at (span src "x"));
  [%expect
    {|
    error: bad
      at <test>:2:13
        var x = 1
                    ^
    |}]
