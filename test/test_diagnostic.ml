(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe

let off src sub =
  let n = String.length sub and m = String.length src in
  let rec go i =
    if i + n > m then -1 else if String.sub src i n = sub then i else go (i + 1)
  in
  go 0

let ctx src =
  { Diagnostic.sm = Source_map.create src; filename = "t.rp"; color = false }

let span src sub =
  { Ast.lo = off src sub; hi = off src sub + String.length sub }

let point src sub = { Ast.lo = off src sub; hi = off src sub }
let render src d = print_string (Diagnostic.render (ctx src) d)

let%expect_test "single caret from a zero-width span" =
  let src = "main() {\n    break\n}\n" in
  render src
    Diagnostic.(error "invalid break statement" |> at (point src "break"));
  [%expect
    {|
    error: invalid break statement
      at t.rp:2:5
            break
            ^
    |}]

let%expect_test "wide caret over a span" =
  let src = "record 12345\n" in
  render src Diagnostic.(error "expected identifier" |> at (span src "12345"));
  [%expect
    {|
    error: expected identifier
      at t.rp:1:8
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
      at t.rp:1:19
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
      at t.rp:1:1
        use()
        ^~~
    note: declared here
      at t.rp:2:1
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
      at t.rp:1:11
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
      at t.rp:2:13
        var x = 1
                    ^
    |}]
