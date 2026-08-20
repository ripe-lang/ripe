(* SPDX-License-Identifier: Apache-2.0 *)

open Span_utils
open Dump
open Pipeline

let%expect_test "parse: missing rparen" =
  run_src "func f() { g( }";
  [%expect
    {|
    error: mismatched delimiter
      at <test>:1:15
        func f() { g( }
                      ^ expected `)`
      at <test>:1:13
        func f() { g( }
                    ^ unclosed `(`
    |}]

let%expect_test "parse: stray token" =
  run_src "func f() { @ }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:12
        func f() { @ }
                   ^
    |}]

let%expect_test "parse: unterminated string" =
  run_src "func f() { var s = \"oops";
  [%expect
    {|
    error: unclosed delimiter
      at <test>:1:10
        func f() { var s = "oops
                 ^
    error: unterminated string
      at <test>:1:25
        func f() { var s = "oops
                                ^
    |}]

let%expect_test "parse: hex/binary literals" =
  run_src "func f() i32 { return 0xff + 0b1010 }";
  [%expect {| ok |}]

let%expect_test "parse: line comments stripped" =
  run_src "func f() i32 {\n  // comment\n  return 1\n}";
  [%expect {| ok |}]

let%expect_test "parse: semicolon-free newline-terminated" =
  run_src "func f() i32 {\n  var x: i32 = 1\n  return x\n}";
  [%expect {| ok |}]

let%expect_test "parse: same-line statements require a separator" =
  run_src "func f(a: i32, b: i32) { var x = 1 x, b = b, a }";
  [%expect
    {|
    error: expected `;`
      at <test>:1:36
        func f(a: i32, b: i32) { var x = 1 x, b = b, a }
                                           ^ found x
    |}]

let%expect_test "parse: explicit semicolon separates statements" =
  run_src "func f() i32 { var x = 1; return x }";
  [%expect {| ok |}]

let%expect_test "parse: semicolon before an initializer" =
  run_src "func f() { var x; = 1 }";
  [%expect
    {|
    error: cannot infer type
      at <test>:1:16
        func f() { var x; = 1 }
                       ^
    help: write the type or give it a value
    error: expected expression
      at <test>:1:19
        func f() { var x; = 1 }
                          ^ found =
    |}]

let%expect_test "parse: newline before an initializer" =
  run_src "func f() {\n  var x\n  = 1\n}";
  [%expect
    {|
    error: cannot infer type
      at <test>:2:7
          var x
              ^
    help: write the type or give it a value
    error: expected expression
      at <test>:3:3
          = 1
          ^ found =
    |}]

let%expect_test "parse: same-line declarations require a separator" =
  run_src "func f() {} func g() {}";
  [%expect
    {|
    error: expected `;`
      at <test>:1:13
        func f() {} func g() {}
                    ^~~~ found `func`
    |}]

let%expect_test "parse: multiline call requires a trailing comma" =
  run_src "func g(_x: i32) {}\nfunc f() {\n  g(\n    1\n  )\n}";
  [%expect
    {|
    error: missing `,` before newline
      at <test>:4:6
            1
             ^
    |}]

let%expect_test "parse: multiline call with a trailing comma" =
  run_src "func g(_x: i32) {}\nfunc f() {\n  g(\n    1,\n  )\n}";
  [%expect {| ok |}]

let%expect_test "parse: multiline block comment separates statements" =
  run_src "func f() {\n  var x = 1 /* first\n  second */ var y = 2\n}";
  [%expect
    {|
    warning: unused variable: x
      at <test>:2:7
          var x = 1 /* first
              ^
    help: prefix with an underscore: _x
    warning: unused variable: y
      at <test>:3:17
          second */ var y = 2
                        ^
    help: prefix with an underscore: _y
    ok
    |}]

let%expect_test "parse: newline before a block" =
  run_src "func f(x: bool) {\n  if x\n  {}\n}";
  [%expect
    {|
    error: expected `{`
      at <test>:2:7
          if x
              ^ found ;
    |}]

let%expect_test "parse: recover, two broken decls" =
  run_src "func f() { @ }\nfunc g() { $ }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:12
        func f() { @ }
                   ^
    error: unexpected character
      at <test>:2:12
        func g() { $ }
                   ^
    |}]

let%expect_test "parse: recover, broken then good" =
  run_src "func f() { return / }\nfunc g() i32 { return 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:19
        func f() { return / }
                          ^ found /
    |}]

let%expect_test "parse: keep binders and collect later type errors" =
  run_src
    {|func f() i32 {
  var x: = /
  return x
}
func g() i32 { return true }|};
  [%expect
    {|
    error: expected type
      at <test>:2:10
          var x: = /
                 ^ found =
    error: expected expression
      at <test>:2:12
          var x: = /
                   ^ found /
    error: type mismatch
      at <test>:5:23
        func g() i32 { return true }
                              ^~~~ expected i32, found bool
    |}]

let%expect_test "parse: sort diagnostics from every phase by source" =
  run_src {|func g() i32 { return true }
func f() { return / }|};
  [%expect
    {|
    error: type mismatch
      at <test>:1:23
        func g() i32 { return true }
                              ^~~~ expected i32, found bool
    error: expected expression
      at <test>:2:19
        func f() { return / }
                          ^ found /
    |}]

let%expect_test "parse: recover, broken body with local does not cascade" =
  run_src "func f() { return / var x: i32 = 1 }\nfunc g() i32 { return 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:19
        func f() { return / var x: i32 = 1 }
                          ^ found /
    |}]

let%expect_test "parse: recover, lex error then grammar error" =
  run_src "func f() { @ }\nfunc g() { return / }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:12
        func f() { @ }
                   ^
    error: expected expression
      at <test>:2:19
        func g() { return / }
                          ^ found /
    |}]

let%expect_test "parse: recover, repeated return after operators" =
  run_src {|func f() {
  return /
  return /
  return /
}|};
  [%expect
    {|
    error: expected expression
      at <test>:2:10
          return /
                 ^ found /
    error: expected expression
      at <test>:3:10
          return /
                 ^ found /
    error: expected expression
      at <test>:4:10
          return /
                 ^ found /
    |}]

let%expect_test "parse: recover, repeated incomplete unary plus" =
  run_src {|func f() {
  return +
  return +
  return +
}|};
  [%expect
    {|
    error: expected expression
      at <test>:2:10
          return +
                 ^
    error: expected expression
      at <test>:3:10
          return +
                 ^
    error: expected expression
      at <test>:4:10
          return +
                 ^
    |}]

let%expect_test "parse: unary operator keeps a valid operand across newline" =
  run_src {|func f() i32 {
  return +
  1
}|};
  [%expect {| ok |}]

let%expect_test "parse: recover, repeated incomplete binary plus" =
  run_src {|func f() {
  return 1 +
  return 2 +
  return 3 +
}|};
  [%expect
    {|
    error: expected expression
      at <test>:2:12
          return 1 +
                   ^
    error: expected expression
      at <test>:3:12
          return 2 +
                   ^
    error: expected expression
      at <test>:4:12
          return 3 +
                   ^
    |}]

let%expect_test "parse: recover, errors in nested blocks" =
  run_src
    {|func f() {
  if true {
    return /
    return /
  }
  while true {
    return /
    return /
  }
  return /
}|};
  [%expect
    {|
    error: expected expression
      at <test>:3:12
            return /
                   ^ found /
    error: expected expression
      at <test>:4:12
            return /
                   ^ found /
    error: expected expression
      at <test>:7:12
            return /
                   ^ found /
    error: expected expression
      at <test>:8:12
            return /
                   ^ found /
    error: expected expression
      at <test>:10:10
          return /
                 ^ found /
    |}]

let%expect_test "parse: recover, errors across top level declarations" =
  run_src
    {|var a: = 1
comptime b: = 2
var c: = 3
type A = +
struct S { x: }
extern "C" func e(x:)
func f(x:) {}|};
  [%expect
    {|
    error: expected type
      at <test>:1:8
        var a: = 1
               ^ found =
    error: expected type
      at <test>:2:13
        comptime b: = 2
                    ^ found =
    error: expected type
      at <test>:3:8
        var c: = 3
               ^ found =
    error: expected type
      at <test>:4:10
        type A = +
                 ^ found +
    error: expected type
      at <test>:5:15
        struct S { x: }
                      ^ found }
    error: expected type
      at <test>:6:21
        extern "C" func e(x:)
                            ^ found )
    error: expected type
      at <test>:7:10
        func f(x:) {}
                 ^ found )
    |}]

let%expect_test "parse: recover, explicit separators on one line" =
  run_src "func f() { return /; return /; var x =; return / }";
  [%expect
    {|
    error: expected expression
      at <test>:1:19
        func f() { return /; return /; var x =; return / }
                          ^ found /
    error: expected expression
      at <test>:1:29
        func f() { return /; return /; var x =; return / }
                                    ^ found /
    error: expected expression
      at <test>:1:39
        func f() { return /; return /; var x =; return / }
                                              ^ found ;
    error: expected expression
      at <test>:1:48
        func f() { return /; return /; var x =; return / }
                                                       ^ found /
    |}]

let%expect_test "parse: recover, skip nested expression tokens" =
  run_src {|func f() {
  return call(
    /
    return /
  )
  return /
}|};
  [%expect
    {|
    error: expected expression
      at <test>:3:5
            /
            ^ found /
    error: expected expression
      at <test>:6:10
          return /
                 ^ found /
    |}]

let%expect_test "parse: recover, preserve valid multiline expressions" =
  run_src
    {|func f() {
  var x = 1 +
    2
  return x +
    3
  return /
  return /
}|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:10
          return x +
                 ^~~ expected (), found i32
    error: expected expression
      at <test>:6:10
          return /
                 ^ found /
    error: expected expression
      at <test>:7:10
          return /
                 ^ found /
    |}]

let%expect_test "parse: recover, comments preserve physical lines" =
  run_src
    {|func f() {
  return / // first
  return / /* second
  line */
  return /
}|};
  [%expect
    {|
    error: expected expression
      at <test>:2:10
          return / // first
                 ^ found /
    error: expected expression
      at <test>:3:10
          return / /* second
                 ^ found /
    error: expected expression
      at <test>:5:10
          return /
                 ^ found /
    |}]

let%expect_test "parse: recover, restore struct literal parsing" =
  run_src
    {|struct point { x: i32 }
func f() {
  if /
  var p = point { x: 1 }
  return /
}|};
  [%expect
    {|
    error: expected expression
      at <test>:3:6
          if /
             ^ found /
    error: expected expression
      at <test>:5:10
          return /
                 ^ found /
    |}]

let%expect_test "parse: recover incomplete cast operators" =
  let src = {|func f() {
  return 1 as
}|} in
  run_parse src;
  [%expect
    {|
    error: expected `;`
      at <test>:2:12
          return 1 as
                   ^~ found as
    |}]

let%expect_test "parse: recover operators across statement forms" =
  let src =
    {|func f() {
  if 1 *
  var a = +
  comptime b = 1 -
  var c = !
  while 2 /
  for x in 3 %
  return 4 ==
}|}
  in
  run_parse src;
  [%expect
    {|
    error: expected expression
      at <test>:2:8
          if 1 *
               ^
    error: expected expression
      at <test>:3:11
          var a = +
                  ^
    error: expected expression
      at <test>:4:18
          comptime b = 1 -
                         ^
    error: expected expression
      at <test>:5:11
          var c = !
                  ^
    error: expected expression
      at <test>:6:11
          while 2 /
                  ^
    error: expected expression
      at <test>:7:14
          for x in 3 %
                     ^
    error: expected expression
      at <test>:8:12
          return 4 ==
                   ^~
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
  [%expect
    {|
    error: expected `;`
      at <test>:1:26
        func _f() { return 1 + 2 as i64 }
                                 ^~ found as
    |}]

let%expect_test "parse: comparison non-associative" =
  parse_expr "a < b < c";
  [%expect
    {|
    error: comparison operators cannot be chained
      at <test>:1:26
        func _f() { return a < b < c }
                                 ^ second comparison operator
      at <test>:1:22
        func _f() { return a < b < c }
                             ^ first comparison operator
    help: split the chain into separate comparisons joined with `&&`
    |}]

let%expect_test "parse: unary minus and not" =
  parse_expr "!flag && -x > 0";
  [%expect {| (&& (! flag) (> (- x) 0)) |}]

let%expect_test "parse: address-of and deref chain" =
  parse_expr "&*p";
  [%expect {| (& (* p)) |}]

let%expect_test "parse: logical and cannot start an expression" =
  parse_expr "&&x";
  [%expect
    {|
    error: expected expression
      at <test>:1:20
        func _f() { return &&x }
                           ^~ found &&
    |}]

let%expect_test "parse: logical or cannot start an expression" =
  parse_expr "||x";
  [%expect
    {|
    error: expected expression
      at <test>:1:20
        func _f() { return ||x }
                           ^~ found ||
    |}]

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
    error: range operators cannot be chained
      at <test>:1:24
        func _f() { return 0..5..10 }
                               ^~ second range operator
      at <test>:1:21
        func _f() { return 0..5..10 }
                            ^~ first range operator
    help: parenthesize a range if nesting is intended
    |}]

let%expect_test "parse: mixed comparison operators cannot be chained" =
  parse_expr "a < b == c";
  [%expect
    {|
    error: comparison operators cannot be chained
      at <test>:1:26
        func _f() { return a < b == c }
                                 ^~ second comparison operator
      at <test>:1:22
        func _f() { return a < b == c }
                             ^ first comparison operator
    help: split the chain into separate comparisons joined with `&&`
    |}]

let%expect_test "parse: mixed range operators cannot be chained" =
  parse_expr "0..5..=10";
  [%expect
    {|
    error: range operators cannot be chained
      at <test>:1:24
        func _f() { return 0..5..=10 }
                               ^~~ second range operator
      at <test>:1:21
        func _f() { return 0..5..=10 }
                            ^~ first range operator
    help: parenthesize a range if nesting is intended
    |}]

let%expect_test "parse: longer comparison chain" =
  parse_expr "a < b < c < d";
  [%expect
    {|
    error: comparison operators cannot be chained
      at <test>:1:26
        func _f() { return a < b < c < d }
                                 ^ second comparison operator
      at <test>:1:22
        func _f() { return a < b < c < d }
                             ^ first comparison operator
    help: split the chain into separate comparisons joined with `&&`
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
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:8
        func f(a: [4]i32) {}
               ^~~~~~~~~
    help: prefix with an underscore: _a
    ok
    |}]

let%expect_test "parse: slice type" =
  run_src "func f(a: []i32) {}";
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:8
        func f(a: []i32) {}
               ^~~~~~~~
    help: prefix with an underscore: _a
    ok
    |}]

let%expect_test "parse: slice of pointer type" =
  run_src "func f(a: []*i32) {}";
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:8
        func f(a: []*i32) {}
               ^~~~~~~~~
    help: prefix with an underscore: _a
    ok
    |}]

let%expect_test "parse: array missing size" =
  run_src "func f(a: [xyz]i32) {}";
  [%expect
    {|
    error: undefined variable
      at <test>:1:12
        func f(a: [xyz]i32) {}
                   ^~~
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
  run_src "func f() {\n  var a: [2]i32 = [\n    1,\n    2,\n  ]\n}";
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
  run_src "func f() {\n  var s = \"line one\nline two\"\n  @\n}";
  [%expect
    {|
    error: unexpected character
      at <test>:4:3
          @
          ^
    |}]

let%expect_test "parse: modifiers on func" =
  run_src "pub func f() {}";
  [%expect {| ok |}]

let%expect_test "parse: modifiers on a global" =
  run_src "pub var X: i32 = 1";
  [%expect {| ok |}]

let%expect_test "parse: modifiers on a type alias" =
  run_src "pub type binop = func (i32, i32) i32";
  [%expect {| ok |}]

let%expect_test "parse: modifier before extern" =
  run_src {|pub extern "C" func puts(s: cstr) i32|};
  [%expect
    {|
    error: expected declaration
      at <test>:1:5
        pub extern "C" func puts(s: cstr) i32
            ^~~~~~ found `extern`
    |}]

let%expect_test "parse: stray token at top level" =
  run_src "return 1";
  [%expect
    {|
    error: expected declaration
      at <test>:1:1
        return 1
        ^~~~~~ found `return`
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
    {|struct pt { x: i32; y: i32 }
func f() i32 {
  var p = pt {
    x: 1,
    y: 2,
  }
  return p.x
}|};
  [%expect {| ok |}]

let%expect_test "parse: if condition is not a struct literal" =
  run_src "func f(x: bool) i32 {\n  if x { return 1 }\n  return 0\n}";
  [%expect {| ok |}]

let%expect_test "parse: else if parses" =
  run_src "func f(x: bool) { if x {} else if x {} }";
  [%expect {| ok |}]

let%expect_test "parse: dangling else has no matching if" =
  run_src "func f() i32 {\n  { if 1 > 0 { 1 } }\n  else { 2 }\n}";
  [%expect
    {|
    error: `else` without a matching `if`
      at <test>:3:3
          else { 2 }
          ^~~~ found `else`
    help: an `if` used as a value closes at its `}`
    |}]

let%expect_test "parse: while condition is not a struct literal" =
  run_src "func f(x: bool) { while x { return } }";
  [%expect {| ok |}]

let%expect_test "parse: for iterable is not a struct literal" =
  run_src
    {|func f(xs: []i32) i32 {
  var s: i32 = 0
  for x in xs { s += x }
  return s
}|};
  [%expect {| ok |}]

let%expect_test "parse: parenthesized struct literal in condition" =
  run_src
    {|struct pt { x: i32 }
func f() i32 {
  if (pt { x: 1 }).x == 1 { return 1 }
  return 0
}|};
  [%expect {| ok |}]

let%expect_test "parse: positional struct literal" =
  parse_expr "pt { 3, 4 }";
  [%expect {| (struct pt 3 4) |}]

let%expect_test "parse: positional struct literal trailing comma" =
  parse_expr "pt { 3, }";
  [%expect {| (struct pt 3) |}]

let%expect_test "parse: positional struct literal of identifiers" =
  parse_expr "pt { a, b }";
  [%expect {| (struct pt a b) |}]

let%expect_test "parse: positional struct literal of expressions" =
  parse_expr "pt { a.x + 1, -b }";
  [%expect {| (struct pt (+ (. a x) 1) (- b)) |}]

let%expect_test "parse: nested positional struct literal" =
  parse_expr "wrap { pt { 1, 2 } }";
  [%expect {| (struct wrap (struct pt 1 2)) |}]

let%expect_test "parse: named struct literal inside a positional one" =
  parse_expr "wrap { pt { x: 1 } }";
  [%expect {| (struct wrap (struct pt (x 1))) |}]

let%expect_test "parse: positional struct literal inside a named one" =
  parse_expr "wrap { p: pt { 1, 2 } }";
  [%expect {| (struct wrap (p (struct pt 1 2))) |}]

let%expect_test "parse: positional struct literal as call argument" =
  parse_expr "dist(pt { 1, 2 })";
  [%expect {| (call dist (struct pt 1 2)) |}]

let%expect_test "parse: field access on positional struct literal" =
  parse_expr "pt { 1, 2 }.x";
  [%expect {| (. (struct pt 1 2) x) |}]

let%expect_test "parse: qualified positional struct literal" =
  parse_expr "geo.pt { 1, 2 }";
  [%expect {| (struct geo.pt 1 2) |}]

let%expect_test "parse: multiline positional struct literal" =
  run_src
    {|struct pt { x: i32; y: i32 }
func f() i32 {
  var p = pt {
    1,
    2,
  }
  return p.x
}|};
  [%expect {| ok |}]

let%expect_test "parse: named field after a positional one" =
  parse_expr "pt { 1, y: 2 }";
  [%expect
    {|
    error: mixed struct fields
      at <test>:1:28
        func _f() { return pt { 1, y: 2 } }
                                   ^ expected a positional field
    |}]

let%expect_test "parse: positional field after a named one" =
  parse_expr "pt { x: 1, 2 }";
  [%expect
    {|
    error: mixed struct fields
      at <test>:1:31
        func _f() { return pt { x: 1, 2 } }
                                      ^ expected a named field
    |}]

let%expect_test "parse: positional struct literal missing comma" =
  parse_expr "pt { 1 2 }";
  [%expect
    {|
    error: expected `,` between fields
      at <test>:1:27
        func _f() { return pt { 1 2 } }
                                  ^ found 2
    |}]

let%expect_test "parse: named struct literal missing comma" =
  parse_expr "pt { x: 1 y: 2 }";
  [%expect
    {|
    error: expected `,` between fields
      at <test>:1:30
        func _f() { return pt { x: 1 y: 2 } }
                                     ^ found y
    |}]

let%expect_test "parse: positional struct literal double comma" =
  parse_expr "pt { 1, 2,, }";
  [%expect
    {|
    error: expected expression
      at <test>:1:30
        func _f() { return pt { 1, 2,, } }
                                     ^ found ,
    |}]

let%expect_test "parse: named struct literal double comma" =
  parse_expr "pt { x: 1, y: 2,, }";
  [%expect
    {|
    error: expected identifier
      at <test>:1:36
        func _f() { return pt { x: 1, y: 2,, } }
                                           ^ found ,
    |}]

let%expect_test "parse: struct literal leading comma" =
  parse_expr "pt { , 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:25
        func _f() { return pt { , 1 } }
                                ^ found ,
    |}]

let%expect_test "parse: struct literal leading comma before a named field" =
  parse_expr "pt { , x: 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:25
        func _f() { return pt { , x: 1 } }
                                ^ found ,
    |}]

let%expect_test "parse: struct literal leading double comma" =
  parse_expr "pt { ,, x: 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:25
        func _f() { return pt { ,, x: 1 } }
                                ^ found ,
    |}]

let%expect_test "parse: struct literal of only commas" =
  parse_expr "pt { ,, }";
  [%expect
    {|
    error: expected expression
      at <test>:1:25
        func _f() { return pt { ,, } }
                                ^ found ,
    |}]

let%expect_test "parse: if body is not a positional struct literal" =
  run_src
    {|struct pt { x: i32 }
func f(c: bool) i32 {
  if c { 1 }
  return 0
}|};
  [%expect {| ok |}]

let%expect_test "parse: parenthesized positional struct literal in condition" =
  run_src
    {|struct pt { x: i32 }
func f() i32 {
  if (pt { 1 }).x == 1 { return 1 }
  return 0
}|};
  [%expect {| ok |}]

let%expect_test "parse: braces are literal in a string" =
  parse_expr "\"a{x}b\"";
  [%expect {| "a{x}b" |}]

let%expect_test "parse: crlf newline as statement separator" =
  run_src "func f() i32 {\r\n  var x: i32 = 1\r\n  return x\r\n}";
  [%expect {| ok |}]

let%expect_test "parse: stray closing paren" =
  run_src "func f() { ) }";
  [%expect
    {|
    error: mismatched delimiter
      at <test>:1:12
        func f() { ) }
                   ^ expected `}`
      at <test>:1:10
        func f() { ) }
                 ^ unclosed `{`
    |}]

let%expect_test "parse: comment at eof with no trailing newline" =
  run_src "func f() i32 { return 1 }\n// trailing comment";
  [%expect {| ok |}]

let%expect_test "parse: bitand binds tighter than comparison" =
  parse_expr "a & 1 == 0";
  [%expect {| (== (& a 1) 0) |}]

let%expect_test "parse: bitor binds looser than bitand" =
  parse_expr "a | b & c";
  [%expect {| (| a (& b c)) |}]

let%expect_test "parse: bitxor sits between bitor and bitand" =
  parse_expr "a | b ^ c & d";
  [%expect {| (| a (^ b (& c d))) |}]

let%expect_test "parse: shift binds tighter than bitand" =
  parse_expr "a & b << c";
  [%expect {| (& a (<< b c)) |}]

let%expect_test "parse: add binds tighter than shift" =
  parse_expr "a << b + c";
  [%expect {| (<< a (+ b c)) |}]

let%expect_test "parse: logical and binds tighter than or" =
  parse_expr "a || b && c";
  [%expect {| (|| a (&& b c)) |}]

let%expect_test "parse: comparison binds tighter than logical and" =
  parse_expr "a && b < c";
  [%expect {| (&& a (< b c)) |}]

let%expect_test "parse: cast chain" =
  parse_expr "x as i32 as f64";
  [%expect
    {|
    error: expected `;`
      at <test>:1:22
        func _f() { return x as i32 as f64 }
                             ^~ found as
    |}]

let%expect_test "parse: negation binds tighter than multiply" =
  parse_expr "-2 * 3";
  [%expect {| (* (- 2) 3) |}]

let%expect_test "parse: double negation" =
  parse_expr "!!x";
  [%expect {| (! (! x)) |}]

let%expect_test "parse: bitnot" =
  parse_expr "~x";
  [%expect {| (~ x) |}]

let%expect_test "parse: unary plus" =
  parse_expr "+42";
  [%expect {| (+ 42) |}]

let%expect_test "parse: unary plus binds tighter than multiply" =
  parse_expr "+2 * 3";
  [%expect {| (* (+ 2) 3) |}]

let%expect_test "parse: field access after call" =
  parse_expr "f().x";
  [%expect {| (. (call f) x) |}]

let%expect_test "parse: index after call" =
  parse_expr "f()[0]";
  [%expect {| (index (call f) 0) |}]

let%expect_test "parse: negation binds looser than index" =
  parse_expr "-arr[0]";
  [%expect {| (- (index arr 0)) |}]

let%expect_test "parse: address-of binds looser than field" =
  parse_expr "&s.x";
  [%expect {| (& (. s x)) |}]

let%expect_test "parse: deref binds looser than field" =
  parse_expr "*p.x";
  [%expect {| (* (. p x)) |}]

let%expect_test "parse: postfix on a cast is rejected" =
  parse_expr "x as i32[0]";
  [%expect
    {|
    error: expected `;`
      at <test>:1:22
        func _f() { return x as i32[0] }
                             ^~ found as
    |}]

let%expect_test "parse: sizeof array type" =
  parse_expr "sizeof([4]i32)";
  [%expect {| (sizeof [4]i32) |}]

let%expect_test "parse: struct literal nested inside array literal" =
  run_src
    {|
struct pt { x: i32; y: i32 }
func f() {
  var a: [2]pt = [pt { x: 1, y: 2 }, pt { x: 3, y: 4 }]
}
|};
  [%expect
    {|
    warning: unused variable: a
      at <test>:4:7
          var a: [2]pt = [pt { x: 1, y: 2 }, pt { x: 3, y: 4 }]
              ^
    help: prefix with an underscore: _a
    ok
    |}]

let%expect_test "parse: function pointer parameter type" =
  run_src {|
func apply(g: func (i32) i32, v: i32) i32 { return g(v) }
|};
  [%expect {| ok |}]

let%expect_test "parse: C function pointer parameter type" =
  run_src
    {|
func apply(g: extern "C" func (i32) i32, v: i32) i32 { return g(v) }
|};
  [%expect {| ok |}]

let%expect_test "parse: C extern function" =
  run_src {|extern "C" func exit(code: i32) never|};
  [%expect {| ok |}]

let%expect_test "parse: Ripe extern function" =
  run_src {|extern "Ripe" func exit(code: i32) never|};
  [%expect {| ok |}]

let%expect_test "parse: extern requires ABI" =
  run_src {|extern func exit(code: i32) never
func main() i32 { return 0 }
|};
  [%expect
    {|
    error: expected ABI name
      at <test>:1:8
        extern func exit(code: i32) never
               ^~~~ found `func`
    |}]

let%expect_test "parse: unsupported extern ABI" =
  run_src
    {|extern "Rust" func exit(code: i32) never
func main() i32 { return 0 }
|};
  [%expect
    {|
    error: unsupported ABI
      at <test>:1:8
        extern "Rust" func exit(code: i32) never
               ^~~~~~ this ABI is not supported here
    |}]

let%expect_test "parse: multiple parameters" =
  run_src "func f(a: i32, b: i32, c: i32) i32 { return a + b + c }";
  [%expect {| ok |}]

let%expect_test "parse: else if chain with else" =
  run_src
    {|
func f(x: i32) i32 {
  if x < 0 { return 0 } else if x == 0 { return 1 } else if x < 10 { return 2 } else { return 3 }
}
|};
  [%expect {| ok |}]

let%expect_test "parse: function body requires a block" =
  run_src "func f() i32 = 1";
  [%expect
    {|
    error: expected `{`
      at <test>:1:14
        func f() i32 = 1
                     ^ found =
    |}]

let%expect_test "parse: unknown string escape" =
  run_src {|func f() { var s = "a\qb" }|};
  [%expect
    {|
    error: unknown escape
      at <test>:1:23
        func f() { var s = "a\qb" }
                              ^
    |}]

let%expect_test "parse: call result indexed then field accessed" =
  parse_expr "f()[0].x";
  [%expect {| (. (index (call f) 0) x) |}]

let%expect_test "parse: deep field access chain" =
  parse_expr "a.b.c.d";
  [%expect {| (. a b c d) |}]

let%expect_test "parse: slice bounds are expressions" =
  parse_expr "a[i + 1..n]";
  [%expect {| (index a (.. (+ i 1) n)) |}]

let%expect_test "parse: function pointer returning array" =
  run_src "type t = func (i32) [3]i32";
  [%expect {| ok |}]

let%expect_test "parse: function pointer returning slice" =
  run_src "type t = func (i32) []i32";
  [%expect {| ok |}]

let%expect_test "parse: struct fields need a separator" =
  run_src "struct S { x: i32 y: i32 }";
  [%expect
    {|
    error: expected `;` or newline between fields
      at <test>:1:19
        struct S { x: i32 y: i32 }
                          ^ found y
    |}]

let%expect_test "parse: struct literal fields need a separator" =
  run_src "func f() { var s = S { x: 1 y: 2 } }";
  [%expect
    {|
    error: expected `,` between fields
      at <test>:1:29
        func f() { var s = S { x: 1 y: 2 } }
                                    ^ found y
    |}]

let%expect_test "parse: multiline struct literal requires a trailing comma" =
  run_src "struct S { x: i32 }\nfunc f() {\n  var s = S {\n    x: 1\n  }\n}";
  [%expect
    {|
    error: missing `,` before newline
      at <test>:4:9
            x: 1
                ^
    |}]

let%expect_test "parse: never as a return type" =
  run_src {|extern "C" func exit(code: i32) never|};
  [%expect {| ok |}]

let%expect_test "parse: block expression needs a trailing value" =
  run_src "func f() i32 {\n  var x = { var a = 1 }\n  return x\n}";
  [%expect
    {|
    warning: unused variable: a
      at <test>:2:17
          var x = { var a = 1 }
                        ^
    help: prefix with an underscore: _a
    error: type mismatch
      at <test>:3:10
          return x
                 ^ expected i32, found ()
    |}]

let%expect_test "parse: if expression needs an else branch" =
  run_src "func f() i32 {\n  var x = if true { 1 }\n  return x\n}";
  [%expect
    {|
    error: type mismatch
      at <test>:3:10
          return x
                 ^ expected i32, found ()
    |}]

let%expect_test "parse: an early exit block yields a value" =
  parse_expr "{ return 5 }";
  [%expect {| (block (return 5)) |}]

let%expect_test "parse: an if with diverging arms is a value" =
  parse_expr "if c { return 1 } else { break }";
  [%expect {| (if (c (block (return 1))) (block (break))) |}]

(* Expression oriented collapse: parse shapes *)

let%expect_test "parse: value if binding shape" =
  parse_expr "if c { 1 } else { 2 }";
  [%expect {| (if (c (block 1)) (block 2)) |}]

let%expect_test "parse: else if chain shape" =
  parse_expr "if a { 1 } else if b { 2 } else { 3 }";
  [%expect {| (if (a (block 1)) (b (block 2)) (block 3)) |}]

let%expect_test "parse: block with a tail value" =
  parse_expr "{ var a = 1\n a + 2 }";
  [%expect {| (block (var a 1) (+ a 2)) |}]

let%expect_test "parse: nested block" =
  parse_expr "{ { 5 } }";
  [%expect {| (block (block 5)) |}]

let%expect_test "parse: if with no else has no else block" =
  parse_expr "if c { 1 }";
  [%expect {| (if (c (block 1))) |}]

let%expect_test "parse: a bare tail expression is an implicit return" =
  run_src "func sq(x: i32) i32 { x * x }";
  [%expect {| ok |}]

let%expect_test "parse: a bad char literal does not cascade" =
  run_src "func f() i32 { return 'AA'i32() }";
  [%expect
    {|
    error: character literal must be a single character
      at <test>:1:23
        func f() i32 { return 'AA'i32() }
                              ^~~~
    error: type mismatch
      at <test>:1:23
        func f() i32 { return 'AA'i32() }
                              ^~~~ expected i32, found char
    error: expected `;`
      at <test>:1:27
        func f() i32 { return 'AA'i32() }
                                  ^~~ found i32
    |}]

let%expect_test "parse: unclosed paren in a while condition points at the paren"
    =
  run_src "func f() { var j = 0 while (j >= 0 && j < 5 { j = j + 1 } }";
  [%expect
    {|
    error: mismatched delimiter
      at <test>:1:59
        func f() { var j = 0 while (j >= 0 && j < 5 { j = j + 1 } }
                                                                  ^ expected `)`
      at <test>:1:28
        func f() { var j = 0 while (j >= 0 && j < 5 { j = j + 1 } }
                                   ^ unclosed `(`
    |}]

let%expect_test "parse: unclosed bracket in an index points at the bracket" =
  run_src "func f() { var arr = [1, 2, 3] if (arr[0 { 1 } }";
  [%expect
    {|
    error: mismatched delimiter
      at <test>:1:48
        func f() { var arr = [1, 2, 3] if (arr[0 { 1 } }
                                                       ^ expected `]`
      at <test>:1:39
        func f() { var arr = [1, 2, 3] if (arr[0 { 1 } }
                                              ^ unclosed `[`
    |}]

let%expect_test "parse: stray closing paren with nothing open" =
  run_src ")";
  [%expect
    {|
    error: unexpected closing delimiter
      at <test>:1:1
        )
        ^
    error: expected declaration
      at <test>:1:1
        )
        ^ found )
    |}]

let%expect_test "parse: multiple unclosed delimiters at eof" =
  run_src "func f() { ( [";
  [%expect
    {|
    error: unclosed delimiter
      at <test>:1:10
        func f() { ( [
                 ^
    error: unclosed delimiter
      at <test>:1:12
        func f() { ( [
                   ^
    error: unclosed delimiter
      at <test>:1:14
        func f() { ( [
                     ^
    |}]

let%expect_test "parse: spans from different files are distinct" =
  let src = "func f() {}" in
  let first_span file =
    match parse ~file src with
    | Ripe.Ast.Func fd :: _ -> fd.func_span
    | _ -> failwith "expected a function"
  in
  Printf.printf "%b" (first_span 0 = first_span 1);
  [%expect {| false |}]

let%expect_test "parse: module imports" =
  let module_ = parse_module {|
import io
import math.vector
|} in
  List.iter
    (fun import ->
      Printf.printf "import %s\n"
        (String.concat "." (List.map Ripe.Interner.text import.Ripe.Ast.path)))
    module_.imports;
  [%expect {|
    import io
    import math.vector
    |}]

let%expect_test "parse: pair assignment" =
  run_src "func f(a: i32, b: i32) { a, b = b, a }";
  [%expect {| ok |}]

let%expect_test "parse: pair assignment rejects a third target" =
  run_src "func f(a: i32, b: i32, c: i32) { a, b, c = b, c, a }";
  [%expect
    {|
    error: pair assignment requires exactly two targets
      at <test>:1:38
        func f(a: i32, b: i32, c: i32) { a, b, c = b, c, a }
                                             ^
    |}]

let%expect_test "parse: pair assignment rejects a third value" =
  run_src "func f(a: i32, b: i32, c: i32) { a, b = b, c, a }";
  [%expect
    {|
    error: pair assignment requires exactly two values
      at <test>:1:45
        func f(a: i32, b: i32, c: i32) { a, b = b, c, a }
                                                    ^
    |}]

let%expect_test "parse: regular assignment remains accepted" =
  run_src "func f(a: i32, b: i32) { a = b }";
  [%expect {| ok |}]

let%expect_test "parse: a module header" =
  let module_ = parse_module "module math\nfunc f() {}" in
  (match module_.header with
  | Some header -> print_endline (Ripe.Interner.text header.Ripe.Ast.name)
  | None -> print_endline "<no header>");
  [%expect {| math |}]

let%expect_test "parse: a file without a module header" =
  let module_ = parse_module "func f() {}" in
  print_endline (match module_.header with Some _ -> "yes" | None -> "no");
  [%expect {| no |}]

let%expect_test "parse: module must come before anything else" =
  run_parse "import io\nmodule math";
  [%expect
    {|
    error: `module` must be the first item
      at <test>:2:1
        module math
        ^~~~~~
    |}]

let%expect_test "parse: a dotted type path" =
  (match parse "var p: math.vector.point = undefined" with
  | [ Ripe.Ast.Global { typ = Some t; _ } ] -> print_endline (dump_typ t)
  | _ -> print_endline "<expected a global>");
  [%expect {| math.vector.point |}]

let%expect_test "parse: a dotted struct literal" =
  parse_expr "math.Point { x: 1, y: 2 }";
  [%expect {| (struct math.Point (x 1) (y 2)) |}]

let%expect_test "parse: a dotted field read stays a field read" =
  parse_expr "math.origin.x";
  [%expect {| (. math origin x) |}]

let%expect_test "parse: an if condition still reads a field access" =
  (match parse "func f(p: point) { if p.flag { } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (if ((. p flag) (block )))) |}]

let%expect_test "parse: an if is a value in an assignment" =
  parse_expr "x = if c { 1 } else { 2 }";
  [%expect {| (= x (if (c (block 1)) (block 2))) |}]

let%expect_test "parse: an if is a value in a call argument" =
  parse_expr "f(if c { 1 } else { 2 })";
  [%expect {| (call f (if (c (block 1)) (block 2))) |}]

let%expect_test "parse: an if is a value in a binary operand" =
  parse_expr "1 + if c { 1 } else { 2 }";
  [%expect {| (+ 1 (if (c (block 1)) (block 2))) |}]

let%expect_test "parse: a nested if body still reads a struct literal" =
  (match parse "func f() { if 1 == if c { P { x: 1 }.x } else { 0 } { } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect
    {| (block (if ((== 1 (if (c (block (. (struct P (x 1)) x))) (block 0))) (block )))) |}]

let%expect_test "parse: operator cannot continue after an automatic semicolon" =
  run_src
    {|func f(x: i32) i32 { return x }
func main() {
  var x = f(1)
    + f(2)
}|};
  [%expect
    {|
    error: operator starts a new statement after a newline
      at <test>:4:5
            + f(2)
            ^
    help: move the operator to the previous line
    |}]

let%expect_test "parse: operator may follow an explicit semicolon" =
  run_src
    {|func f(x: i32) i32 { return x }
func main() {
  var _x = f(1); +f(2)
}|};
  [%expect
    {|
    warning: discarded operation result
      at <test>:3:18
          var _x = f(1); +f(2)
                         ^~~~~
    help: use `var _ = ...` when this is intentional
    ok |}]

let%expect_test
    "parse: dereference assignment may follow an automatic semicolon" =
  run_src {|func f(p: *i32) {
  var _x = 1
  *p = 2
}|};
  [%expect {| ok |}]

let%expect_test "parse: declarations may appear in a block" =
  (match
     parse
       {|func f() {
  type Coord = i32
  struct Point { x: Coord }
  func read(p: Point) Coord { p.x }
}|}
   with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect
    {|
    (block (local type Coord) (local struct Point) (local func read (block (. p x)))) |}]

let%expect_test "parse: pub on a local declaration is accepted" =
  run_src {|func f() {
  pub type Coord = i32
}|};
  [%expect {| ok |}]

let%expect_test "parse: a bare loop takes a block" =
  (match parse "func f() { loop { break } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (loop (block (break)))) |}]

let%expect_test "parse: a loop takes a label" =
  run_src "func f() { outer: loop { loop { break :outer } } }";
  [%expect {| ok |}]

let%expect_test "parse: a loop rejects a condition" =
  run_src "func f(x: bool) { loop x { } }";
  [%expect
    {|
    error: expected `{`
      at <test>:1:24
        func f(x: bool) { loop x { } }
                               ^ found x
    |}]

let%expect_test "parse: a block is a value in a call argument" =
  parse_expr "f({ g(); 1 })";
  [%expect {| (call f (block (call g) 1)) |}]

let%expect_test "parse: a block is a value in a binop operand" =
  parse_expr "1 + { 2 }";
  [%expect {| (+ 1 (block 2)) |}]

let%expect_test "parse: a block is a value in an assignment" =
  parse_expr "x = { 1 }";
  [%expect {| (= x (block 1)) |}]

let%expect_test "parse: a block is a value in an array element" =
  parse_expr "[{ 1 }, 2]";
  [%expect {| (array (block 1) 2) |}]

let%expect_test "parse: a block is a value in an index" =
  parse_expr "xs[{ 1 }]";
  [%expect {| (index xs (block 1)) |}]

let%expect_test "parse: a block takes a postfix field read" =
  parse_expr "{ p }.x";
  [%expect {| (. (block p) x) |}]

let%expect_test "parse: a statement block is still a statement" =
  (match parse "func f() { { g() } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (block (call g))) |}]

let%expect_test "parse: a struct literal still wins over a block" =
  parse_expr "Point { x: 1 }";
  [%expect {| (struct Point (x 1)) |}]

let%expect_test "parse: break takes a value" =
  (match parse "func f() { loop { break 42 } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (loop (block (break 42)))) |}]

let%expect_test "parse: break takes a label and a value" =
  (match parse "func f() { outer: loop { loop { break :outer 42 } } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (loop (block (loop (block (break 42)))))) |}]

let%expect_test "parse: a bare break ends at a newline" =
  (match parse {|func f() {
  loop {
    break
  }
}|} with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (loop (block (break)))) |}]

let%expect_test "parse: a loop is a value in a binding" =
  parse_expr "x = loop { break 1 }";
  [%expect {| (= x (loop (block (break 1)))) |}]

let%expect_test "parse: an enum declares its variants" =
  (match parse "enum Color { Red, Green, Blue }" with
  | [ Ripe.Ast.Enum ed ] ->
      let name (v : Ripe.Ast.variant) = Ripe.Interner.text v.variant_name in
      print_endline (String.concat " " (List.map name ed.variants))
  | _ -> print_endline "<expected an enum>");
  [%expect.unreachable]
[@@expect.uncaught_exn
  {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)
  ("Ripe.Diagnostic.Errors(_)")
  Raised at Test_ripe__Diag.finish in file "test/diag.ml", line 29, characters 17-51
  Called from Test_ripe__Pipeline.parse_module in file "test/pipeline.ml", lines 7-8, characters 4-63
  Called from Test_ripe__Pipeline.parse in file "test/pipeline.ml", line 10, characters 28-52
  Called from Test_ripe__Test_parser.(fun) in file "test/test_parser.ml", line 1752, characters 9-48
  Called from Ppx_expect_runtime__Test_block.Configured.dump_backtrace in file "runtime/test_block.ml", line 142, characters 10-28
  |}]

let%expect_test "parse: a newline separates variants" =
  (match parse {|enum Color {
  Red
  Green
}|} with
  | [ Ripe.Ast.Enum ed ] ->
      print_endline (string_of_int (List.length ed.variants))
  | _ -> print_endline "<expected an enum>");
  [%expect {| 2 |}]

let%expect_test "parse: an enum may appear in a block" =
  (match parse {|func f() {
  enum Step { First }
}|} with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (local enum Step)) |}]

let%expect_test "parse: an arm takes an expression or a block" =
  (match
     parse
       {|func f() {
  match c {
    0 => 1
    1 => { g() }
    _ => 2
  }
}|}
   with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect
    {| (block (match c (0 (block 1)) (1 (block (block (call g)))) (_ (block 2)))) |}]

let%expect_test "parse: a newline separates arms" =
  (match parse {|func f() {
  match c {
    0 => 1
    _ => 2
  }
}|} with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (match c (0 (block 1)) (_ (block 2)))) |}]

let%expect_test "parse: match is a value" =
  (match parse "func f() { var x = match c { _ => 1 } }" with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (var x (match c (_ (block 1))))) |}]

let%expect_test "parse: an arm body may leave the loop or the function" =
  (match
     parse
       {|func f() {
  match c {
    0 => return
    1 => break
    _ => continue
  }
}|}
   with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect
    {| (block (match c (0 (block (return))) (1 (block (break))) (_ (block (continue))))) |}]

let%expect_test "parse: a scrutinee stops before the arms" =
  run_src "func f(p: i32) i32 { match p { _ => 1 } }";
  [%expect {| ok |}]

let%expect_test "parse: a bare name binds and a dotted one is a constant" =
  (match
     parse {|func f() {
  match c {
    Color.Red => 1
    other => 2
  }
}|}
   with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (match c ((. Color Red) (block 1)) (other (block 2)))) |}]

let%expect_test "parse: a binding may be named with an underscore" =
  (match parse {|func f() {
  var _ = 1
  var _ = 2
}|} with
  | [ Ripe.Ast.Func fd ] -> print_endline (dump_block fd.body)
  | _ -> print_endline "<expected a function>");
  [%expect {| (block (var _ 1) (var _ 2)) |}]
