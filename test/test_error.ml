(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Spanutils
open Diag

let%expect_test "error: type mismatch" =
  let src = "var x: i32 = true\n" in
  render src
    (Diagnostic.type_mismatch (span src "true") ~expected:"i32" ~found:"bool");
  [%expect
    {|
    error: type mismatch
      at <test>:1:14
        var x: i32 = true
                     ^~~~ expected i32, found bool
    |}]

let%expect_test "error: undefined name" =
  let src = "return foo\n" in
  render src (Diagnostic.undefined_name (span src "foo") "variable");
  [%expect
    {|
    error: undefined variable
      at <test>:1:8
        return foo
               ^~~
    |}]

let%expect_test "error: redefinition points at the previous binder" =
  let src = "var x = 1\nvar x = 2\n" in
  let first = substring_offset src "x" in
  let prev = Span.make first (first + 1) in
  let second = first + String.length "var x = 1\nvar " in
  render src (Diagnostic.redefinition (Span.make second (second + 1)) ~prev);
  [%expect
    {|
    error: already defined
      at <test>:2:9
        var x = 2
                ^
      at <test>:1:5
        var x = 1
            ^ previous definition here
    |}]

let%expect_test "error: arity" =
  let src = "f(1, 2)\n" in
  render src
    (Diagnostic.arity (span src "f(1, 2)") ~expected:"expected 1 argument"
       ~found:2);
  [%expect
    {|
    error: wrong number of arguments
      at <test>:1:1
        f(1, 2)
        ^~~~~~~ expected 1 argument, found 2
    |}]

let%expect_test "error: integer literal out of range" =
  let src = "var x: i8 = 300\n" in
  render src (Diagnostic.int_out_of_range (span src "300") ~ty:"i8");
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:13
        var x: i8 = 300
                    ^~~ does not fit in i8
    |}]

let%expect_test "error: internal compiler error" =
  let src = "func main() {}\n" in
  render src
    (Diagnostic.internal ~span:(span src "main") "test invariant failed");
  [%expect
    {|
    error: internal compiler error
      at <test>:1:6
        func main() {}
             ^~~~
    test invariant failed
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "error: expected expression after operator" =
  let src = "return +\n" in
  render src (Diagnostic.expected_expression (span src "+"));
  [%expect
    {|
    error: expected expression
      at <test>:1:8
        return +
               ^
    |}]

let%expect_test "error: expected type after operator" =
  let src = "return x as\n" in
  render src (Diagnostic.expected_type (span src "as"));
  [%expect
    {|
    error: expected type
      at <test>:1:10
        return x as
                 ^~
    |}]

let%expect_test "error: message with type" =
  let src = "return value[0]\n" in
  render src
    (Diagnostic.with_type (span src "value[0]") "cannot index type" "i32");
  [%expect
    {|
    error: cannot index type
      at <test>:1:8
        return value[0]
               ^~~~~~~~ on i32
    |}]
