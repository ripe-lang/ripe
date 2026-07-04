(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

(* watch https://youtu.be/0c8b7YfsBKs?si=euXz4FdXHS4UXAE8 *)

let%expect_test "parse: missing rparen" =
  run_src "func f() { g( }";
  [%expect
    {|
    error: expected expression
      at <test>:1:15
        func f() { g( }
                      ^ found }
    |}]

let%expect_test "parse: stray token" =
  run_src "func f() { @ }";
  [%expect
    {|
    error: unexpected character: @
      at <test>:1:12
        func f() { @ }
                   ^
    |}]

let%expect_test "parse: unterminated string" =
  run_src "func f() { const s = \"oops";
  [%expect
    {|
    error: unterminated string
      at <test>:1:27
        func f() { const s = "oops
                                  ^
    error: expected }
      at <test>:1:27
        func f() { const s = "oops
                                  ^ found <eof>
    |}]

let%expect_test "parse: hex/binary literals" =
  run_src "func f() i32 { return 0xff + 0b1010 }";
  [%expect
    {|
    warning: unreachable code
      at <test>:1:24
        func f() i32 { return 0xff + 0b1010 }
                               ^~~~~~~
    error: undefined variable: xff
      at <test>:1:24
        func f() i32 { return 0xff + 0b1010 }
                               ^~~
    error: undefined variable: b1010
      at <test>:1:31
        func f() i32 { return 0xff + 0b1010 }
                                      ^~~~~
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
    error: unexpected character: @
      at <test>:1:12
        func f() { @ } func g() { $ }
                   ^
    error: unexpected character: $
      at <test>:1:27
        func f() { @ } func g() { $ }
                                  ^
    |}]

let%expect_test "parse: recover, broken then good" =
  run_src "func f() { return + } func g() i32 { return 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:19
        func f() { return + } func g() i32 { return 1 }
                          ^ found +
    |}]

let%expect_test "parse: recover, lex error then grammar error" =
  run_src "func f() { @ } func g() { return + }";
  [%expect
    {|
    error: unexpected character: @
      at <test>:1:12
        func f() { @ } func g() { return + }
                   ^
    error: expected expression
      at <test>:1:34
        func f() { @ } func g() { return + }
                                         ^ found +
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
  [%expect
    {|
    error: cannot chain non-associative operator <
      at <test>:1:26
        func _f() { return a < b < c }
                                 ^
    |}]

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
  [%expect
    {|
    error: cannot chain non-associative operator ..
      at <test>:1:24
        func _f() { return 0..5..10 }
                               ^~
    |}]

let%expect_test "parse: array literal" =
  parse_expr "[1, 2, 3]";
  [%expect {| (array 1 2 3) |}]

let%expect_test "parse: empty array literal" =
  parse_expr "[]";
  [%expect {| (array) |}]

let%expect_test "parse: index" =
  parse_expr "a[0]";
  [%expect {| (index a 0) |}]

let%expect_test "parse: index with expression" =
  parse_expr "a[i + 1]";
  [%expect {| (index a (+ i 1)) |}]

let%expect_test "parse: chained index" =
  parse_expr "a[i][j]";
  [%expect {| (index (index a i) j) |}]

let%expect_test "parse: index binds tighter than binop" =
  parse_expr "a[0] + b[1]";
  [%expect {| (+ (index a 0) (index b 1)) |}]

let%expect_test "parse: len field access" =
  parse_expr "a.len";
  [%expect {| (. a len) |}]

let%expect_test "parse: fixed array type" =
  run_src "func f(a: [4]i32) {}";
  [%expect {| ok |}]

let%expect_test "parse: slice type" =
  run_src "func f(a: []i32) {}";
  [%expect {| ok |}]

let%expect_test "parse: slice of pointer type" =
  run_src "func f(a: []*i32) {}";
  [%expect {| ok |}]

let%expect_test "parse: array missing size" =
  run_src "func f(a: [xyz]i32) {}";
  [%expect
    {|
    error: expected array size
      at <test>:1:12
        func f(a: [xyz]i32) {}
                   ^~~ found xyz
    |}]

let%expect_test "parse: array literal trailing comma" =
  parse_expr "[1, 2, 3,]";
  [%expect {| (array 1 2 3) |}]

let%expect_test "parse: call trailing comma" =
  parse_expr "add(1, 2,)";
  [%expect {| (call add 1 2) |}]

let%expect_test "parse: nested array literal" =
  parse_expr "[[1, 2], [3, 4]]";
  [%expect {| (array (array 1 2) (array 3 4)) |}]

let%expect_test "parse: slice index is range" =
  parse_expr "a[1..3]";
  [%expect {| (index a (.. 1 3)) |}]

let%expect_test "parse: ptr field access" =
  parse_expr "s.ptr";
  [%expect {| (. s ptr) |}]

let%expect_test "parse: multiline array literal" =
  run_src "func f() {\n  var a: [2]i32 = [\n    1,\n    2\n  ]\n}";
  [%expect
    {|
    warning: unused variable: a
      at <test>:2:7
          var a: [2]i32 = [
              ^
    help: prefix with an underscore: _a
    ok
    |}]

let%expect_test "parse: line tracking after multiline string" =
  run_src "func f() {\n  let s = \"line one\nline two\"\n  @\n}";
  [%expect
    {|
    error: unexpected character: @
      at <test>:4:3
          @
          ^
    |}]

let%expect_test "parse: modifiers on func" =
  run_src "public inline func f() {}";
  [%expect {| ok |}]

let%expect_test "parse: modifier before non-func decl" =
  run_src "public const X: i32 = 1";
  [%expect
    {|
    error: expected declaration
      at <test>:1:8
        public const X: i32 = 1
               ^~~~~ found const
    |}]

let%expect_test "parse: stray token at top level" =
  run_src "return 1";
  [%expect
    {|
    error: expected declaration
      at <test>:1:1
        return 1
        ^~~~~~ found return
    |}]

let%expect_test "parse: struct literal" =
  parse_expr "pt { x: 3, y: 4 }";
  [%expect {| (struct pt (x 3) (y 4)) |}]

let%expect_test "parse: empty struct literal" =
  parse_expr "pt { }";
  [%expect {| (struct pt) |}]

let%expect_test "parse: struct literal trailing comma" =
  parse_expr "pt { x: 3, }";
  [%expect {| (struct pt (x 3)) |}]

let%expect_test "parse: nested struct literal" =
  parse_expr "wrap { p: pt { x: 1 } }";
  [%expect {| (struct wrap (p (struct pt (x 1)))) |}]

let%expect_test "parse: struct literal as call argument" =
  parse_expr "dist(pt { x: 1, y: 2 })";
  [%expect {| (call dist (struct pt (x 1) (y 2))) |}]

let%expect_test "parse: field access on struct literal" =
  parse_expr "pt { x: 1 }.x";
  [%expect {| (. (struct pt (x 1)) x) |}]

let%expect_test "parse: multiline struct literal" =
  run_src
    "struct pt { x: i32, y: i32 }\n\
     func f() i32 {\n\
    \  const p = pt {\n\
    \    x: 1,\n\
    \    y: 2\n\
    \  }\n\
    \  return p.x\n\
     }";
  [%expect {| ok |}]

let%expect_test "parse: if condition is not a struct literal" =
  run_src "func f(x: bool) i32 { if x { return 1 } return 0 }";
  [%expect {| ok |}]

let%expect_test "parse: else if hints elseif" =
  run_src "func f(x: bool) { if x {} else if x {} }";
  [%expect
    {|
    error: expected block after else
      at <test>:1:32
        func f(x: bool) { if x {} else if x {} }
                                       ^~ found if
    help: the keyword is elseif, one word
    |}]

let%expect_test "parse: while condition is not a struct literal" =
  run_src "func f(x: bool) { while x { return } }";
  [%expect {| ok |}]

let%expect_test "parse: for iterable is not a struct literal" =
  run_src
    "func f(xs: []i32) i32 {\n\
    \  var s: i32 = 0\n\
    \  for x in xs { s += x }\n\
    \  return s\n\
     }";
  [%expect {| ok |}]

let%expect_test "parse: parenthesized struct literal in condition" =
  run_src
    "struct pt { x: i32 }\n\
     func f() i32 { if (pt { x: 1 }).x == 1 { return 1 } return 0 }";
  [%expect {| ok |}]

let%expect_test "parse: stray brace in string" =
  parse_expr "\"a}b\"";
  [%expect {| <interp> |}]
