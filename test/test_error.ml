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

let%expect_test "error: invalid operand names the op and the type" =
  let src = "a + b\n" in
  render src (Diagnostic.bad_operand (span src "+") ~op:"+" ~ty:"bool");
  [%expect
    {|
    error: invalid operand
      at <test>:1:3
        a + b
          ^ cannot apply `+` to bool
    |}]

let%expect_test "error: cannot infer suggests writing the type" =
  let src = "var x\n" in
  render src (Diagnostic.cannot_infer (span src "x"));
  [%expect
    {|
    error: cannot infer type
      at <test>:1:5
        var x
            ^
    help: write the type or give it a value
    |}]

let%expect_test "error: message with what was found instead" =
  let src = "type T = 1\n" in
  render src (Diagnostic.with_found (span src "1") "expected a type" "a number");
  [%expect
    {|
    error: expected a type
      at <test>:1:10
        type T = 1
                 ^ found a number
    |}]

let%expect_test "error: an unsupported abi" =
  let src = {|extern "Rust" func f()|} in
  render src (Diagnostic.unsupported_abi (span src {|"Rust"|}));
  [%expect
    {|
    error: unsupported ABI
      at <test>:1:8
        extern "Rust" func f()
               ^~~~~~ this ABI is not supported here
    |}]

let%expect_test "error: an operation on an opaque pointer" =
  let src = "var y = *p\n" in
  render src (Diagnostic.opaque_operation (span src "*p") "dereference");
  [%expect
    {|
    error: cannot dereference *opaque
      at <test>:1:9
        var y = *p
                ^~
    help: cast to a typed pointer first
    |}]

let%expect_test "error: a constant that depends on itself" =
  let src = "const C = C + 1\n" in
  render src (Diagnostic.cyclic_constant (span src "C + 1"));
  [%expect
    {|
    error: cyclic constant
      at <test>:1:11
        const C = C + 1
                  ^~~~~
    |}]

let%expect_test "error: two break values that disagree" =
  let src = "break 1\nbreak true\n" in
  render src
    (Diagnostic.break_disagree (span src "break true") "this one is bool"
       ~other:(span src "break 1") ~other_message:"this one is i32");
  [%expect
    {|
    error: `break` values disagree
      at <test>:2:1
        break true
        ^~~~~~~~~~ this one is bool
      at <test>:1:1
        break 1
        ^~~~~~~ this one is i32
    |}]
