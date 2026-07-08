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

let render src d = print_string (Diagnostic.render (ctx src) d)

let%expect_test "error: type mismatch" =
  let src = "var x: i32 = true\n" in
  render src
    (Error.type_mismatch (span src "true") ~expected:"i32" ~found:"bool");
  [%expect
    {|
    error: type mismatch
      at t.rp:1:14
        var x: i32 = true
                     ^~~~ expected i32, found bool
    |}]

let%expect_test "error: undefined name" =
  let src = "return foo\n" in
  render src (Error.undefined_name (span src "foo") "variable" "foo");
  [%expect
    {|
    error: undefined variable: foo
      at t.rp:1:8
        return foo
               ^~~
    |}]

let%expect_test "error: redefinition points at the previous binder" =
  let src = "var x = 1\nvar x = 2\n" in
  let prev = { Ast.lo = off src "x"; hi = off src "x" + 1 } in
  let second = off src "x" + String.length "var x = 1\nvar " in
  render src (Error.redefinition { Ast.lo = second; hi = second + 1 } ~prev "x");
  [%expect
    {|
    error: already defined: x
      at t.rp:2:9
        var x = 2
                ^
      at t.rp:1:5
        var x = 1
            ^ previous definition here
    |}]

let%expect_test "error: arity" =
  let src = "f(1, 2)\n" in
  render src
    (Error.arity (span src "f(1, 2)") ~expected:"expected 1 argument" ~found:2);
  [%expect
    {|
    error: expected 1 argument, found 2
      at t.rp:1:1
        f(1, 2)
        ^~~~~~~
    |}]

let%expect_test "error: integer literal out of range" =
  let src = "var x: i8 = 300\n" in
  render src (Error.int_out_of_range (span src "300") ~ty:"i8");
  [%expect
    {|
    error: integer literal out of range
      at t.rp:1:13
        var x: i8 = 300
                    ^~~ does not fit in i8
    |}]

let%expect_test "error: unsupported feature" =
  let src = "return 0..5\n" in
  render src (Error.unsupported (span src "0..5") "range expressions");
  [%expect
    {|
    error: range expressions is not yet supported
      at t.rp:1:8
        return 0..5
               ^~~~
    |}]

let%expect_test "error: internal compiler error" =
  let src = "func main() {}\n" in
  render src (Error.internal ~span:(span src "main") "TVoid has no size");
  [%expect
    {|
    error: internal compiler error
      at t.rp:1:6
        func main() {}
             ^~~~
    TVoid has no size
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]
