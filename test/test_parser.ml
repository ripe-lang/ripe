(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

(* watch https://youtu.be/0c8b7YfsBKs?si=euXz4FdXHS4UXAE8 *)

let%expect_test "parse: missing rparen" =
  run_src "func f() { g( }";
  [%expect {| ParseError: expected expression |}]

let%expect_test "parse: stray token" =
  run_src "func f() { @ }";
  [%expect {| ParseError: unexpected character: @ |}]

let%expect_test "parse: hex/binary literals" =
  run_src "func f() i32 { return 0xff + 0b1010 }";
  [%expect
    {|
    <test>:1:24: warning: unreachable code
    TypeError: <test>:1:24: undefined variable 'xff'
    TypeError: <test>:1:31: undefined variable 'b1010'
    |}]

let%expect_test "parse: line comments stripped" =
  run_src "func f() i32 {\n  # comment\n  return 1\n}";
  [%expect {| ok |}]

let%expect_test "parse: semicolon-free newline-terminated" =
  run_src "func f() i32 {\n  const x: i32 = 1\n  return x\n}";
  [%expect {| ok |}]

let%expect_test "parse: recover, two broken decls" =
  run_src "func f() { @ } func g() { $ }";
  [%expect
    {|
    ParseError: unexpected character: @
    ParseError: unexpected character: $
    |}]

let%expect_test "parse: recover, broken then good" =
  run_src "func f() { return + } func g() i32 { return 1 }";
  [%expect {| ParseError: expected expression |}]

let%expect_test "parse: recover, lex error then grammar error" =
  run_src "func f() { @ } func g() { return + }";
  [%expect
    {|
    ParseError: unexpected character: @
    ParseError: expected expression
    |}]

let%expect_test "parse: precedence + vs *" =
  parse_expr "1 + 2 * 3";
  [%expect {| (+ 1 (* 2 3)) |}]

let%expect_test "parse: precedence * vs +" =
  parse_expr "1 * 2 + 3";
  [%expect {| (+ (* 1 2) 3) |}]

let%expect_test "parse: associativity, - is left" =
  parse_expr "a - b - c";
  [%expect {| (- (- a b) c) |}]

let%expect_test "parse: cast binds tighter than +" =
  parse_expr "1 + 2 as i64";
  [%expect {| (+ 1 (as 2 i64)) |}]

let%expect_test "parse: comparison non-associative" =
  parse_expr "a < b < c";
  [%expect {| ParseError: cannot chain non-associative operator '<' |}]

let%expect_test "parse: unary minus and not" =
  parse_expr "!flag && -x > 0";
  [%expect {| (&& (! flag) (> (neg x) 0)) |}]

let%expect_test "parse: address-of and deref chain" =
  parse_expr "&*p";
  [%expect {| (addr (deref p)) |}]

let%expect_test "parse: call with args" =
  parse_expr "add(1, 2 * 3)";
  [%expect {| (call add 1 (* 2 3)) |}]

let%expect_test "parse: field access" =
  parse_expr "p.x + 1";
  [%expect {| (+ (. p x) 1) |}]

let%expect_test "parse: sizeof" =
  parse_expr "sizeof(*i32)";
  [%expect {| (sizeof *i32) |}]

let%expect_test "parse: range" =
  parse_expr "0..n";
  [%expect {| (.. 0 n) |}]

let%expect_test "parse: range inclusive" =
  parse_expr "0..=n";
  [%expect {| (..= 0 n) |}]

let%expect_test "parse: range non-associative" =
  parse_expr "0..5..10";
  [%expect {| ParseError: cannot chain non-associative operator '..' |}]
