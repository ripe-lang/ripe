(* SPDX-License-Identifier: Apache-2.0 *)

open Dump
open Pipeline

let%expect_test ("parse: missing rparen" [@tags "disabled"]) =
  run_src "fn f() { g( };";
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:1:13
        fn f() { g( };
                    ^ expected `)`
      at <test>:1:11
        fn f() { g( };
                  ^ to match this `(`
    |}]

let%expect_test "parse: stray token" =
  run_src "fn f() { @ }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:10
        fn f() { @ }
                 ^
    |}]

let%expect_test ("parse: unterminated string" [@tags "disabled"]) =
  run_src "fn f() { var s = \"oops";
  [%expect
    {|
    error: unclosed delimiter
      at <test>:1:8
        fn f() { var s = "oops
               ^
    error: unterminated string
      at <test>:1:18
        fn f() { var s = "oops
                         ^~~~~
    |}]

let%expect_test "parse: hex/binary literals" =
  run_src "fn f() i32 { return 0xff + 0b1010 }";
  [%expect {| ok |}]

let%expect_test "parse: line comments stripped" =
  run_src "fn f() i32 {\n  // comment\n  return 1;\n}";
  [%expect {| ok |}]

let%expect_test "parse: same-line statements require a separator" =
  run_src "fn f(a: i32, b: i32) { var x = 1 x, b = b, a }";
  [%expect
    {|
    error: expected `;`
      at <test>:1:34
        fn f(a: i32, b: i32) { var x = 1 x, b = b, a }
                                         ^ found x
    |}]

let%expect_test "parse: explicit semicolon separates statements" =
  run_src "fn f() i32 { var x = 1; return x }";
  [%expect {| ok |}]

let%expect_test ("parse: semicolon before an initializer" [@tags "disabled"]) =
  run_src "fn f() { var x; = 1 }";
  [%expect
    {|
    error: cannot infer type
      at <test>:1:14
        fn f() { var x; = 1 }
                     ^
    help: write the type or give it a value
    error: expected expression
      at <test>:1:17
        fn f() { var x; = 1 }
                        ^ found =
    |}]

let%expect_test "parse: a declaration ending in a brace needs no separator" =
  run_src "fn f() {} fn g() {}";
  [%expect {| ok |}]

let%expect_test "parse: multiline call with a trailing comma" =
  run_src "fn g(_x: i32) {}\nfn f() {\n  g(\n    1,\n  );\n}";
  [%expect {| ok |}]

let%expect_test ("parse: recover, two broken decls" [@tags "disabled"]) =
  run_src "fn f() { @ }\nfn g() { $ }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:10
        fn f() { @ }
                 ^
    error: unexpected character
      at <test>:2:10
        fn g() { $ }
                 ^
    |}]

let%expect_test "parse: recover, broken then good" =
  run_src "fn f() { return / }\nfn g() i32 { return 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:17
        fn f() { return / }
                        ^ found /
    |}]

let%expect_test
    ("parse: keep binders and collect later type errors" [@tags "disabled"]) =
  run_src {|fn f() i32 {
  var x: = /
  return x;
}
fn g() i32 { return true }|};
  [%expect
    {|
    error: expected type
      at <test>:2:10
          var x: = /
                 ^ found =
    error: type mismatch
      at <test>:5:21
        fn g() i32 { return true }
                            ^~~~ expected i32, found bool
    |}]

let%expect_test
    ("parse: sort diagnostics from every phase by source" [@tags "disabled"]) =
  run_src {|fn g() i32 { return true }
fn f() { return / }|};
  [%expect
    {|
    error: type mismatch
      at <test>:1:21
        fn g() i32 { return true }
                            ^~~~ expected i32, found bool
    error: expected expression
      at <test>:2:17
        fn f() { return / }
                        ^ found /
    |}]

let%expect_test "parse: recover, broken body with local does not cascade" =
  run_src "fn f() { return / var x: i32 = 1 }\nfn g() i32 { return 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:17
        fn f() { return / var x: i32 = 1 }
                        ^ found /
    |}]

let%expect_test
    ("parse: recover, lex error then grammar error" [@tags "disabled"]) =
  run_src "fn f() { @ }\nfn g() { return / }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:10
        fn f() { @ }
                 ^
    error: expected expression
      at <test>:2:17
        fn g() { return / }
                        ^ found /
    |}]

let%expect_test
    ("parse: recover, repeated return after operators" [@tags "disabled"]) =
  run_src {|fn f() {
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

let%expect_test
    ("parse: recover, repeated incomplete unary minus" [@tags "disabled"]) =
  run_src {|fn f() {
  return -
  return -
  return -
}|};
  [%expect
    {|
    error: expected expression
      at <test>:2:10
          return -
                 ^
    error: expected expression
      at <test>:3:10
          return -
                 ^
    error: expected expression
      at <test>:4:10
          return -
                 ^
    |}]

let%expect_test "parse: unary operator keeps a valid operand across newline" =
  run_src {|fn f() i32 {
  return -
  1;
}|};
  [%expect {| ok |}]

let%expect_test
    ("parse: recover, repeated incomplete binary plus" [@tags "disabled"]) =
  run_src {|fn f() {
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

let%expect_test ("parse: recover, errors in nested blocks" [@tags "disabled"]) =
  run_src
    {|fn f() {
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

let%expect_test
    ("parse: recover, errors across top level declarations" [@tags "disabled"])
    =
  run_src
    {|var a: = 1;
var b: = 2;
var c: = 3;
type A = +
struct S { x: };
extern "C" fn e(x:);
fn f(x:) {}|};
  [%expect
    {|
    error: expected type
      at <test>:1:8
        var a: = 1;
               ^ found =
    error: expected type
      at <test>:2:8
        var b: = 2;
               ^ found =
    error: expected type
      at <test>:3:8
        var c: = 3;
               ^ found =
    error: expected type
      at <test>:4:10
        type A = +
                 ^ found +
    error: expected type
      at <test>:5:15
        struct S { x: };
                      ^ found }
    error: expected type
      at <test>:6:19
        extern "C" fn e(x:);
                          ^ found )
    error: expected type
      at <test>:7:8
        fn f(x:) {}
               ^ found )
    |}]

let%expect_test
    ("parse: recover, explicit separators on one line" [@tags "disabled"]) =
  run_src "fn f() { return /; return /; var x =; return / }";
  [%expect
    {|
    error: expected expression
      at <test>:1:17
        fn f() { return /; return /; var x =; return / }
                        ^ found /
    error: expected expression
      at <test>:1:27
        fn f() { return /; return /; var x =; return / }
                                  ^ found /
    error: expected expression
      at <test>:1:37
        fn f() { return /; return /; var x =; return / }
                                            ^ found ;
    error: expected expression
      at <test>:1:46
        fn f() { return /; return /; var x =; return / }
                                                     ^ found /
    |}]

let%expect_test
    ("parse: recover, skip nested expression tokens" [@tags "disabled"]) =
  run_src {|fn f() {
  return call(
    /
    return /
  );
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

let%expect_test
    ("parse: recover, preserve valid multiline expressions" [@tags "disabled"])
    =
  run_src
    {|fn f() {
  var x = 1 +
    2;
  return x +
    3;
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

let%expect_test
    ("parse: recover, comments preserve physical lines" [@tags "disabled"]) =
  run_src
    {|fn f() {
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

let%expect_test
    ("parse: recover, restore struct literal parsing" [@tags "disabled"]) =
  run_src
    {|struct point { x: i32 }
fn f() {
  if /
  var p = point { x: 1 };
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
  let src = {|fn f() {
  return 1 as;
}|} in
  run_parse src;
  [%expect
    {|
    error: expected `;`
      at <test>:2:12
          return 1 as;
                   ^~ found as
    |}]

let%expect_test
    ("parse: recover operators across statement forms" [@tags "disabled"]) =
  let src =
    {|fn f() {
  if 1 *
  var a = ~
  var b = 1 -
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
          var a = ~
                  ^
    error: expected expression
      at <test>:4:13
          var b = 1 -
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
      at <test>:1:24
        fn _f() { return 1 + 2 as i64 }
                               ^~ found as
    |}]

let%expect_test "parse: comparison non-associative" =
  parse_expr "a < b < c";
  [%expect
    {|
    error: comparison operators cannot be chained
      at <test>:1:24
        fn _f() { return a < b < c }
                               ^ second comparison operator
      at <test>:1:20
        fn _f() { return a < b < c }
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
      at <test>:1:18
        fn _f() { return &&x }
                         ^~ found &&
    |}]

let%expect_test "parse: logical or cannot start an expression" =
  parse_expr "||x";
  [%expect
    {|
    error: expected expression
      at <test>:1:18
        fn _f() { return ||x }
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

let%expect_test "parse: cast" =
  parse_expr "cast(*[2]i32, p) + cast(i64, (x))";
  [%expect {| (+ (cast *[2]i32 p) (cast i64 x)) |}]

let%expect_test "parse: cast to a function pointer type" =
  parse_expr {|cast(extern "C" fn (i32) i32, f)(1)|};
  [%expect {| (call (cast (i32) i32 f) 1) |}]

let%expect_test "parse: cast without a comma" =
  parse_expr "cast(i32 x)";
  [%expect
    {|
    error: expected `,`
      at <test>:1:27
        fn _f() { return cast(i32 x) }
                                  ^ found x
    |}]

let%expect_test "parse: the old paren cast is a call" =
  parse_expr "(*i32)(p)";
  [%expect {| (call (* i32) p) |}]

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
      at <test>:1:22
        fn _f() { return 0..5..10 }
                             ^~ second range operator
      at <test>:1:19
        fn _f() { return 0..5..10 }
                          ^~ first range operator
    help: parenthesize a range if nesting is intended
    |}]

let%expect_test "parse: mixed comparison operators cannot be chained" =
  parse_expr "a < b == c";
  [%expect
    {|
    error: comparison operators cannot be chained
      at <test>:1:24
        fn _f() { return a < b == c }
                               ^~ second comparison operator
      at <test>:1:20
        fn _f() { return a < b == c }
                           ^ first comparison operator
    help: split the chain into separate comparisons joined with `&&`
    |}]

let%expect_test "parse: mixed range operators cannot be chained" =
  parse_expr "0..5..=10";
  [%expect
    {|
    error: range operators cannot be chained
      at <test>:1:22
        fn _f() { return 0..5..=10 }
                             ^~~ second range operator
      at <test>:1:19
        fn _f() { return 0..5..=10 }
                          ^~ first range operator
    help: parenthesize a range if nesting is intended
    |}]

let%expect_test "parse: longer comparison chain" =
  parse_expr "a < b < c < d";
  [%expect
    {|
    error: comparison operators cannot be chained
      at <test>:1:24
        fn _f() { return a < b < c < d }
                               ^ second comparison operator
      at <test>:1:20
        fn _f() { return a < b < c < d }
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
  run_src "fn f(a: [4]i32) {}";
  [%expect {| ok |}]

let%expect_test "parse: slice type" =
  run_src "fn f(a: []i32) {}";
  [%expect {| ok |}]

let%expect_test "parse: slice of pointer type" =
  run_src "fn f(a: []*i32) {}";
  [%expect {| ok |}]

let%expect_test "parse: array missing size" =
  run_src "fn f(a: [xyz]i32) {}";
  [%expect
    {|
    error: undefined variable
      at <test>:1:10
        fn f(a: [xyz]i32) {}
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
  run_src "fn f() {\n  var a: [2]i32 = [\n    1,\n    2,\n  ];\n}";
  [%expect {| ok |}]

let%expect_test
    ("parse: line tracking after unterminated string" [@tags "disabled"]) =
  run_src "fn f() {\n  var s = \"line one\n  var t = 1;\n  @\n}";
  [%expect
    {|
    error: unterminated string
      at <test>:2:11
          var s = "line one
                  ^~~~~~~~~
    error: unexpected character
      at <test>:4:3
          @
          ^
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
    {|struct pt { x: i32, y: i32 }
fn f() i32 {
  var p = pt {
    x: 1,
    y: 2,
  };
  return p.x;
}|};
  [%expect {| ok |}]

let%expect_test "parse: if condition is not a struct literal" =
  run_src "fn f(x: bool) i32 {\n  if x { return 1 }\n  return 0;\n}";
  [%expect {| ok |}]

let%expect_test "parse: else if parses" =
  run_src "fn f(x: bool) { if x {} else if x {} }";
  [%expect {| ok |}]

let%expect_test "parse: dangling else has no matching if" =
  run_src "fn f() i32 {\n  { if 1 > 0 { 1 } }\n  else { 2 };\n}";
  [%expect
    {|
    error: `else` without a matching `if`
      at <test>:3:3
          else { 2 };
          ^~~~ found `else`
    help: an `if` used as a value closes at its `}`
    |}]

let%expect_test "parse: while condition is not a struct literal" =
  run_src "fn f(x: bool) { while x { return } }";
  [%expect {| ok |}]

let%expect_test "parse: for iterable is not a struct literal" =
  run_src
    {|fn f(xs: []i32) i32 {
  var s: i32 = 0;
  for x in xs { s += x }
  return s;
}|};
  [%expect {| ok |}]

let%expect_test "parse: parenthesized struct literal in condition" =
  run_src
    {|struct pt { x: i32 }
fn f() i32 {
  if (pt { x: 1 }).x == 1 { return 1 }
  return 0;
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

let%expect_test "parse: multiline positional struct literal" =
  run_src
    {|struct pt { x: i32, y: i32 }
fn f() i32 {
  var p = pt {
    1,
    2,
  };
  return p.x;
}|};
  [%expect {| ok |}]

let%expect_test "parse: named field after a positional one" =
  parse_expr "pt { 1, y: 2 }";
  [%expect
    {|
    error: mixed struct fields
      at <test>:1:26
        fn _f() { return pt { 1, y: 2 } }
                                 ^ expected a positional field
    |}]

let%expect_test "parse: positional field after a named one" =
  parse_expr "pt { x: 1, 2 }";
  [%expect
    {|
    error: mixed struct fields
      at <test>:1:29
        fn _f() { return pt { x: 1, 2 } }
                                    ^ expected a named field
    |}]

let%expect_test "parse: positional struct literal missing comma" =
  parse_expr "pt { 1 2 }";
  [%expect
    {|
    error: expected `,` between fields
      at <test>:1:25
        fn _f() { return pt { 1 2 } }
                                ^ found 2
    |}]

let%expect_test "parse: named struct literal missing comma" =
  parse_expr "pt { x: 1 y: 2 }";
  [%expect
    {|
    error: expected `,` between fields
      at <test>:1:28
        fn _f() { return pt { x: 1 y: 2 } }
                                   ^ found y
    |}]

let%expect_test "parse: positional struct literal double comma" =
  parse_expr "pt { 1, 2,, }";
  [%expect
    {|
    error: expected expression
      at <test>:1:28
        fn _f() { return pt { 1, 2,, } }
                                   ^ found ,
    |}]

let%expect_test "parse: named struct literal double comma" =
  parse_expr "pt { x: 1, y: 2,, }";
  [%expect
    {|
    error: expected identifier
      at <test>:1:34
        fn _f() { return pt { x: 1, y: 2,, } }
                                         ^ found ,
    |}]

let%expect_test "parse: struct literal leading comma" =
  parse_expr "pt { , 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:23
        fn _f() { return pt { , 1 } }
                              ^ found ,
    |}]

let%expect_test "parse: struct literal leading comma before a named field" =
  parse_expr "pt { , x: 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:23
        fn _f() { return pt { , x: 1 } }
                              ^ found ,
    |}]

let%expect_test "parse: struct literal leading double comma" =
  parse_expr "pt { ,, x: 1 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:23
        fn _f() { return pt { ,, x: 1 } }
                              ^ found ,
    |}]

let%expect_test "parse: struct literal of only commas" =
  parse_expr "pt { ,, }";
  [%expect
    {|
    error: expected expression
      at <test>:1:23
        fn _f() { return pt { ,, } }
                              ^ found ,
    |}]

let%expect_test "parse: if body is not a positional struct literal" =
  run_src
    {|struct pt { x: i32 }
fn f(c: bool) i32 {
  if c { 1 }
  return 0;
}|};
  [%expect {| ok |}]

let%expect_test "parse: parenthesized positional struct literal in condition" =
  run_src
    {|struct pt { x: i32 }
fn f() i32 {
  if (pt { 1 }).x == 1 { return 1 }
  return 0;
}|};
  [%expect {| ok |}]

let%expect_test "parse: braces are literal in a string" =
  parse_expr "\"a{x}b\"";
  [%expect {| "a{x}b" |}]

let%expect_test "parse: crlf line endings" =
  run_src "fn f() i32 {\r\n  var x: i32 = 1;\r\n  return x;\r\n}";
  [%expect {| ok |}]

let%expect_test ("parse: stray closing paren" [@tags "disabled"]) =
  run_src "fn f() { ) }";
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:1:10
        fn f() { ) }
                 ^ expected `}`
      at <test>:1:8
        fn f() { ) }
               ^ to match this `{`
    |}]

let%expect_test "parse: comment at eof with no trailing newline" =
  run_src "fn f() i32 { return 1 }\n// trailing comment";
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
      at <test>:1:20
        fn _f() { return x as i32 as f64 }
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

let%expect_test "parse: unary plus is rejected" =
  run_src "fn f() { var _x = +42 }";
  [%expect
    {|
    error: expected expression
      at <test>:1:19
        fn f() { var _x = +42 }
                          ^ found +
    |}]

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
      at <test>:1:20
        fn _f() { return x as i32[0] }
                           ^~ found as
    |}]

let%expect_test "parse: sizeof array type" =
  parse_expr "sizeof([4]i32)";
  [%expect {| (sizeof [4]i32) |}]

let%expect_test "parse: struct literal nested inside array literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
fn f() {
  var a: [2]pt = [pt { x: 1, y: 2 }, pt { x: 3, y: 4 }];
}
|};
  [%expect {| ok |}]

let%expect_test "parse: function pointer parameter type" =
  run_src {|
fn apply(g: fn (i32) i32, v: i32) i32 { return g(v) }
|};
  [%expect {| ok |}]

let%expect_test "parse: C function pointer parameter type" =
  run_src {|
fn apply(g: extern "C" fn (i32) i32, v: i32) i32 { return g(v) }
|};
  [%expect {| ok |}]

let%expect_test "parse: C extern function" =
  run_src {|extern "C" fn exit(code: i32) never;|};
  [%expect {| ok |}]

let%expect_test "parse: Ripe extern function" =
  run_src {|extern "Ripe" fn exit(code: i32) never;|};
  [%expect {| ok |}]

let%expect_test "parse: extern requires ABI" =
  run_src {|extern fn exit(code: i32) never;
fn main() i32 { return 0 }
|};
  [%expect
    {|
    error: expected ABI name
      at <test>:1:8
        extern fn exit(code: i32) never;
               ^~ found `fn`
    |}]

let%expect_test "parse: unsupported extern ABI" =
  run_src
    {|extern "Rust" fn exit(code: i32) never;
fn main() i32 { return 0 }
|};
  [%expect
    {|
    error: unsupported ABI
      at <test>:1:8
        extern "Rust" fn exit(code: i32) never;
               ^~~~~~ this ABI is not supported here
    |}]

let%expect_test "parse: extern function with a body" =
  run_src {|extern "C" fn add(a: i32, b: i32) i32 { return a + b }|};
  [%expect {| ok |}]

let%expect_test "parse: Ripe extern function with a body" =
  run_src {|extern "Ripe" fn add(a: i32, b: i32) i32 { return a + b }|};
  [%expect {| ok |}]

let%expect_test "parse: extern alone" =
  run_src {|extern
fn main() i32 { return 0 }
|};
  [%expect
    {|
    error: expected ABI name
      at <test>:2:1
        fn main() i32 { return 0 }
        ^~ found `fn`
    |}]

let%expect_test "parse: extern at the end of the file" =
  run_src {|fn main() i32 { return 0 }
extern|};
  [%expect
    {|
    error: expected ABI name
      at <test>:2:7
        extern
              ^ found <eof>
    |}]

let%expect_test "parse: ABI on a struct" =
  run_src {|extern "C" struct S { x: i32 }|};
  [%expect
    {|
    error: expected `fn`
      at <test>:1:12
        extern "C" struct S { x: i32 }
                   ^~~~~~ found `struct`
    |}]

let%expect_test "parse: ABI on a global" =
  run_src {|extern "C" var n: i32 = 0;|};
  [%expect
    {|
    error: expected `fn`
      at <test>:1:12
        extern "C" var n: i32 = 0;
                   ^~~ found `var`
    |}]

let%expect_test "parse: missing ABI before a struct" =
  run_src {|extern struct S { x: i32 }|};
  [%expect
    {|
    error: expected ABI name
      at <test>:1:8
        extern struct S { x: i32 }
               ^~~~~~ found `struct`
    |}]

let%expect_test "parse: extern inside a body" =
  run_src {|fn f() i32 {
  extern "C" fn g(a: i32) i32;
  return 0;
}|};
  [%expect
    {|
    error: `extern` must be at the top level
      at <test>:2:3
          extern "C" fn g(a: i32) i32;
          ^~~~~~
    |}]

let%expect_test "parse: extern definition inside a body" =
  run_src
    {|fn f() i32 {
  extern "C" fn g(a: i32) i32 { return a }
  return 0;
}|};
  [%expect
    {|
    error: `extern` must be at the top level
      at <test>:2:3
          extern "C" fn g(a: i32) i32 { return a }
          ^~~~~~
    |}]

let%expect_test "parse: variadic declaration" =
  run_src {|extern "C" fn printf(fmt: cstr, ...) i32;|};
  [%expect {| ok |}]

let%expect_test "parse: variadic with a body" =
  run_src {|extern "C" fn printf(fmt: cstr, ...) i32 { return 0 }|};
  [%expect
    {|
    error: a function with a body cannot be variadic
      at <test>:1:33
        extern "C" fn printf(fmt: cstr, ...) i32 { return 0 }
                                        ^~~
    help: `...` only works on a declaration with no body
    |}]

let%expect_test "parse: plain function with a variadic body" =
  run_src {|fn f(a: i32, ...) i32 { return a }|};
  [%expect
    {|
    error: a function with a body cannot be variadic
      at <test>:1:14
        fn f(a: i32, ...) i32 { return a }
                     ^~~
    help: `...` only works on a declaration with no body
    |}]

let%expect_test "parse: multiple parameters" =
  run_src "fn f(a: i32, b: i32, c: i32) i32 { return a + b + c }";
  [%expect {| ok |}]

let%expect_test "parse: else if chain with else" =
  run_src
    {|
fn f(x: i32) i32 {
  if x < 0 { return 0 } else if x == 0 { return 1 } else if x < 10 { return 2 } else { return 3 }
}
|};
  [%expect {| ok |}]

let%expect_test "parse: function body requires a block" =
  run_src "fn f() i32 = 1;";
  [%expect
    {|
    error: expected `{`
      at <test>:1:12
        fn f() i32 = 1;
                   ^ found =
    |}]

let%expect_test "parse: unknown string escape" =
  run_src {|fn f() { var s = "a\qb" }|};
  [%expect
    {|
    error: unknown escape
      at <test>:1:21
        fn f() { var s = "a\qb" }
                            ^
    |}]

let%expect_test "parse: an unknown escape keeps the rest of its statement" =
  run_src {|extern "C" fn g(s: cstr, n: i32);
fn f() { g("a\qb", nope); }|};
  [%expect
    {|
    error: unknown escape
      at <test>:2:15
        fn f() { g("a\qb", nope); }
                      ^
    error: undefined variable
      at <test>:2:20
        fn f() { g("a\qb", nope); }
                           ^~~~
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
  run_src "type t = fn (i32) [3]i32;";
  [%expect {| ok |}]

let%expect_test "parse: function pointer returning slice" =
  run_src "type t = fn (i32) []i32;";
  [%expect {| ok |}]

let%expect_test "parse: struct fields need a separator" =
  run_src "struct S { x: i32 y: i32 }";
  [%expect
    {|
    error: expected field separator
      at <test>:1:19
        struct S { x: i32 y: i32 }
                          ^
    help: separate fields with `,`
    |}]

(* A dropped field would show up as a later error on p.x or p.y *)
let%expect_test "parse: a semicolon between fields keeps the struct" =
  run_src
    {|struct P { x: i32; y: i32 }
fn main() i32 {
  var p: P = P { x: 12, y: 30 };
  return p.x + p.y;
}|};
  [%expect
    {|
    error: expected field separator
      at <test>:1:18
        struct P { x: i32; y: i32 }
                         ^
    help: separate fields with `,`
    |}]

let%expect_test "parse: a semicolon between variants keeps the enum" =
  run_src
    {|enum C { Red; Green }
fn main() i32 {
  var a: C = C.Red;
  var b: C = C.Green;
  return 0;
}|};
  [%expect
    {|
    error: expected variant separator
      at <test>:1:13
        enum C { Red; Green }
                    ^
    help: separate variants with `,`
    |}]

let%expect_test "parse: a bad field name drops only that field" =
  run_src
    {|struct P { 99: i32,
  y: i32 }
fn main() i32 {
  var p: P;
  return p.y;
}|};
  [%expect
    {|
    error: expected identifier
      at <test>:1:12
        struct P { 99: i32,
                   ^~ found 99
    |}]

let%expect_test "parse: a bad variant name drops only that variant" =
  run_src
    {|enum C { 99,
  Green }
fn main() i32 {
  var c: C = C.Green;
  return 0;
}|};
  [%expect
    {|
    error: expected identifier
      at <test>:1:10
        enum C { 99,
                 ^~ found 99
    |}]

let%expect_test ("parse: two bad fields report once each" [@tags "disabled"]) =
  run_src
    {|struct P { 99: i32,
  88: i32,
  z: i32 }
fn main() i32 {
  var p: P;
  return p.z;
}|};
  [%expect
    {|
    error: expected identifier
      at <test>:1:12
        struct P { 99: i32,
                   ^~ found 99
    error: expected identifier
      at <test>:2:3
          88: i32,
          ^~ found 88
    |}]

let%expect_test "parse: a missing separator keeps both items" =
  run_src
    {|enum C { Red,
  Green @
  Blue }
fn main() i32 {
  var c: C = C.Blue;
  return 0;
}|};
  [%expect
    {|
    error: unexpected character
      at <test>:2:9
          Green @
                ^
    |}]

let%expect_test "parse: a bad parameter name keeps the arity" =
  run_src
    {|fn add(99: i32, b: i32) i32 { return b }
fn main() i32 { return add(1, 2) }|};
  [%expect
    {|
    error: expected identifier
      at <test>:1:8
        fn add(99: i32, b: i32) i32 { return b }
               ^~ found 99
    |}]

let%expect_test
    ("parse: repeated bad parameter names do not collide" [@tags "disabled"]) =
  run_src
    {|fn f(99: i32, 88: i32, 77: i32) i32 { return 1 }
fn main() i32 { return f(1, 2, 3) }|};
  [%expect
    {|
    error: expected identifier
      at <test>:1:6
        fn f(99: i32, 88: i32, 77: i32) i32 { return 1 }
             ^~ found 99
    error: expected identifier
      at <test>:1:15
        fn f(99: i32, 88: i32, 77: i32) i32 { return 1 }
                      ^~ found 88
    error: expected identifier
      at <test>:1:24
        fn f(99: i32, 88: i32, 77: i32) i32 { return 1 }
                               ^~ found 77
    |}]

let%expect_test "parse: a semicolon between parameters keeps the function" =
  run_src
    {|fn add(a: i32; b: i32) i32 { return a + b }
fn main() i32 { return add(1, 2) }|};
  [%expect
    {|
    error: expected parameter separator
      at <test>:1:14
        fn add(a: i32; b: i32) i32 { return a + b }
                     ^
    help: separate parameters with `,`
    |}]

let%expect_test "parse: a missing parameter separator keeps the function" =
  run_src
    {|fn add(a: i32 b: i32) i32 { return a + b }
fn main() i32 { return add(1, 2) }|};
  [%expect
    {|
    error: expected parameter separator
      at <test>:1:15
        fn add(a: i32 b: i32) i32 { return a + b }
                      ^
    help: separate parameters with `,`
    |}]

let%expect_test
    ("parse: a stray ellipsis keeps the function" [@tags "disabled"]) =
  run_src
    {|fn f(a: i32 ...; b: i32) i32 { return a }
fn main() i32 { return f(1, 2) }|};
  [%expect
    {|
    error: expected parameter separator
      at <test>:1:13
        fn f(a: i32 ...; b: i32) i32 { return a }
                    ^~~
    help: separate parameters with `,`
    error: a function with a body cannot be variadic
      at <test>:1:13
        fn f(a: i32 ...; b: i32) i32 { return a }
                    ^~~
    help: `...` only works on a declaration with no body
    error: `...` must be the last parameter
      at <test>:1:16
        fn f(a: i32 ...; b: i32) i32 { return a }
                       ^
    |}]

let%expect_test ("parse: a stray closing brace reports once" [@tags "disabled"])
    =
  run_src {|fn f() {}
}
fn main() i32 { return 0 }|};
  [%expect
    {|
    error: unexpected closing delimiter
      at <test>:2:1
        }
        ^
    |}]

let%expect_test "parse: a stray character reports once" =
  run_src "fn main() i32 { return 1 @ 2 }";
  [%expect
    {|
    error: unexpected character
      at <test>:1:26
        fn main() i32 { return 1 @ 2 }
                                 ^
    |}]

let%expect_test "parse: a bad number literal reports once" =
  run_src {|fn f() i32 { return 0xZZ }
fn main() i32 { return f() }|};
  [%expect
    {|
    error: invalid number literal
      at <test>:1:21
        fn f() i32 { return 0xZZ }
                            ^~~~
    |}]

let%expect_test "parse: match arms name arms in the separator error" =
  run_src
    {|enum C { Red,
  Green }
fn main() i32 {
  var c: C = C.Red;
  return match c { C.Red => 0; C.Green => 1 };
}|};
  [%expect
    {|
    error: expected arm separator
      at <test>:5:30
          return match c { C.Red => 0; C.Green => 1 };
                                     ^
    help: separate arms with `,`
    |}]

let%expect_test "parse: struct literal fields need a separator" =
  run_src "fn f() { var s = S { x: 1 y: 2 } }";
  [%expect
    {|
    error: expected `,` between fields
      at <test>:1:27
        fn f() { var s = S { x: 1 y: 2 } }
                                  ^ found y
    |}]

let%expect_test "parse: never as a return type" =
  run_src {|extern "C" fn exit(code: i32) never;|};
  [%expect {| ok |}]

let%expect_test "parse: block expression needs a trailing value" =
  run_src "fn f() i32 {\n  var x = { var a = 1 };\n  return x;\n}";
  [%expect
    {|
    error: type mismatch
      at <test>:3:10
          return x;
                 ^ expected i32, found ()
    |}]

let%expect_test "parse: if expression needs an else branch" =
  run_src "fn f() i32 {\n  var x = if true { 1 };\n  return x;\n}";
  [%expect
    {|
    error: type mismatch
      at <test>:3:10
          return x;
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
  parse_expr "{ var a = 1;\n a + 2 }";
  [%expect {| (block (var a 1) (+ a 2)) |}]

let%expect_test "parse: nested block" =
  parse_expr "{ { 5 } }";
  [%expect {| (block (block 5)) |}]

let%expect_test "parse: if with no else has no else block" =
  parse_expr "if c { 1 }";
  [%expect {| (if (c (block 1))) |}]

let%expect_test "parse: a bare tail expression is an implicit return" =
  run_src "fn sq(x: i32) i32 { x * x }";
  [%expect {| ok |}]

let%expect_test
    ("parse: a bad char literal does not cascade" [@tags "disabled"]) =
  run_src "fn f() i32 { return 'AA'i32() }";
  [%expect
    {|
    error: character literal must be a single character
      at <test>:1:21
        fn f() i32 { return 'AA'i32() }
                            ^~~~
    error: expected `;`
      at <test>:1:25
        fn f() i32 { return 'AA'i32() }
                                ^~~ found i32
    |}]

let%expect_test
    ("parse: unclosed paren in a while condition points at the paren"
     [@tags "disabled"]) =
  run_src "fn f() { var j = 0 while (j >= 0 && j < 5 { j = j + 1 } };";
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:1:57
        fn f() { var j = 0 while (j >= 0 && j < 5 { j = j + 1 } };
                                                                ^ expected `)`
      at <test>:1:26
        fn f() { var j = 0 while (j >= 0 && j < 5 { j = j + 1 } };
                                 ^ to match this `(`
    |}]

let%expect_test
    ("parse: unclosed bracket in an index points at the bracket"
     [@tags "disabled"]) =
  run_src "fn f() { var arr = [1, 2, 3] if (arr[0 { 1 } }";
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:1:46
        fn f() { var arr = [1, 2, 3] if (arr[0 { 1 } }
                                                     ^ expected `]`
      at <test>:1:37
        fn f() { var arr = [1, 2, 3] if (arr[0 { 1 } }
                                            ^ to match this `[`
    |}]

let%expect_test
    ("parse: stray closing paren with nothing open" [@tags "disabled"]) =
  run_src ")";
  [%expect
    {|
    error: unexpected closing delimiter
      at <test>:1:1
        )
        ^
    |}]

let%expect_test
    ("parse: multiple unclosed delimiters at eof" [@tags "disabled"]) =
  run_src "fn f() { ( [";
  [%expect
    {|
    error: unclosed delimiter
      at <test>:1:12
        fn f() { ( [
                   ^
      at <test>:1:10
        fn f() { ( [
                 ^ to match this `(`
      at <test>:1:8
        fn f() { ( [
               ^ to match this `{`
    |}]

let%expect_test "parse: regular assignment remains accepted" =
  run_src "fn f(b: i32) { var a = 1; a = b }";
  [%expect {| ok |}]

let%expect_test "parse: a dotted field read stays a field read" =
  parse_expr "math.origin.x";
  [%expect {| (. math origin x) |}]

let%expect_test "parse: an if condition still reads a field access" =
  parse_body "fn f(p: point) { if p.flag { } }";
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
  parse_body "fn f() { if 1 == if c { P { x: 1 }.x } else { 0 } { } }";
  [%expect
    {| (block (if ((== 1 (if (c (block (. (struct P (x 1)) x))) (block 0))) (block )))) |}]

let%expect_test "parse: operator may follow an explicit semicolon" =
  run_src
    {|fn f(x: i32) i32 { return x }
fn main() i32 {
  var _x = f(1); -f(2); return 0;
}|};
  [%expect {| ok |}]

let%expect_test "parse: declarations may appear in a block" =
  parse_body
    {|fn f() {
  type Coord = i32;
  struct Point { x: Coord }
  fn read(p: Point) Coord { p.x }
}|};
  [%expect
    {|
    (block (local type Coord) (local struct Point) (local fn read (block (. p x)))) |}]

let%expect_test "parse: a bare loop takes a block" =
  parse_body "fn f() { loop { break } }";
  [%expect {| (block (loop (block (break)))) |}]

let%expect_test "parse: a loop takes a label" =
  run_src "fn f() { outer: loop { loop { break :outer } } }";
  [%expect {| ok |}]

let%expect_test "parse: a loop rejects a condition" =
  run_src "fn f(x: bool) { loop x { } }";
  [%expect
    {|
    error: expected `{`
      at <test>:1:22
        fn f(x: bool) { loop x { } }
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
  parse_body "fn f() { { g() } }";
  [%expect {| (block (block (call g))) |}]

let%expect_test "parse: a struct literal still wins over a block" =
  parse_expr "Point { x: 1 }";
  [%expect {| (struct Point (x 1)) |}]

let%expect_test "parse: a struct literal in an if header points at the brace" =
  run_src "struct Point { x: i32 }\nfn f() { if Point { x: 1 }.x == 1 { } }";
  [%expect
    {|
    error: a struct literal can't go in a header
      at <test>:2:19
        fn f() { if Point { x: 1 }.x == 1 { } }
                          ^ this `{` starts the body
    help: wrap the literal in parentheses
    |}]

let%expect_test
    "parse: a struct literal in a match scrutinee points at the brace" =
  run_src "struct Point { x: i32 }\nfn f() { match Point { x: 1 }.x { } }";
  [%expect
    {|
    error: a struct literal can't go in a header
      at <test>:2:22
        fn f() { match Point { x: 1 }.x { } }
                             ^ this `{` starts the body
    help: wrap the literal in parentheses
    |}]

let%expect_test "parse: a label in a header body is not a struct literal" =
  parse_body "fn f() { if g() { outer: loop { break :outer } } }";
  [%expect {| (block (if ((call g) (block (loop (block (break))))))) |}]

let%expect_test "parse: break takes a value" =
  parse_body "fn f() { loop { break 42 } }";
  [%expect {| (block (loop (block (break 42)))) |}]

let%expect_test "parse: break takes a label and a value" =
  parse_body "fn f() { outer: loop { loop { break :outer 42 } } }";
  [%expect {| (block (loop (block (loop (block (break 42)))))) |}]

let%expect_test "parse: a bare break ends at a semicolon" =
  parse_body {|fn f() {
  loop {
    break;
  }
}|};
  [%expect {| (block (loop (block (break)))) |}]

let%expect_test "parse: a loop is a value in a binding" =
  parse_expr "x = loop { break 1 }";
  [%expect {| (= x (loop (block (break 1)))) |}]

let%expect_test "parse: an enum declares its variants" =
  (match parse {|enum Color {
  Red,
  Green,
  Blue,
}|} with
  | [ Ripe.Ast.Enum { variants; _ } ] ->
      print_endline (String.concat " " (List.map dump_ident variants))
  | _ -> print_endline "<expected an enum>");
  [%expect {| Red Green Blue |}]

let%expect_test "parse: an enum may appear in a block" =
  parse_body {|fn f() {
  enum Step { First }
}|};
  [%expect {| (block (local enum Step)) |}]

let%expect_test "parse: an arm takes an expression or a block" =
  parse_body
    {|fn f() {
  match c {
    0 => 1,
    1 => { g() }
    _ => 2,
  }
}|};
  [%expect
    {| (block (match c (0 (block 1)) (1 (block (block (call g)))) (_ (block 2)))) |}]

let%expect_test "parse: match is a value" =
  parse_body "fn f() { var x = match c { _ => 1 } }";
  [%expect {| (block (var x (match c (_ (block 1))))) |}]

let%expect_test "parse: an arm body may leave the loop or the function" =
  parse_body
    {|fn f() {
  match c {
    0 => return,
    1 => break,
    _ => continue,
  }
}|};
  [%expect
    {| (block (match c (0 (block (return))) (1 (block (break))) (_ (block (continue))))) |}]

let%expect_test "parse: a scrutinee stops before the arms" =
  run_src "fn f(p: i32) i32 { match p { _ => 1 } }";
  [%expect {| ok |}]

let%expect_test "parse: a bare name binds and a dotted one is a constant" =
  parse_body {|fn f() {
  match c {
    Color.Red => 1,
    other => 2,
  }
}|};
  [%expect {| (block (match c ((. Color Red) (block 1)) (other (block 2)))) |}]

let%expect_test "parse: a binding may be named with an underscore" =
  parse_body {|fn f() {
  var _ = 1;
  var _ = 2;
}|};
  [%expect {| (block (var _ 1) (var _ 2)) |}]

let%expect_test
    ("parse: a local enum body keeps the brace it was given" [@tags "disabled"])
    =
  run_src
    {|fn f() i32 {
  enum side  Left, Right }
  var s = side.Right;
  return 0;
}|};
  [%expect
    {|
    error: unexpected closing delimiter
      at <test>:5:1
        }
        ^
    |}]

let%expect_test "parse: a binding with two names keeps the second" =
  run_src {|fn f() i32 {
  var q n: i32 = 1;
  return n;
}|};
  [%expect
    {|
    error: expected `;`
      at <test>:2:9
          var q n: i32 = 1;
                ^ found n
    |}]

let%expect_test "parse: a run of names leaves only the annotated one" =
  run_src {|fn f() i32 {
  var q f x: i32 = 1;
  return x;
}|};
  [%expect
    {|
    error: expected `;`
      at <test>:2:9
          var q f x: i32 = 1;
                ^ found f
    |}]

let%expect_test
    ("parse: an extra name and a missing colon are both said once"
     [@tags "disabled"]) =
  run_src {|fn f() i32 {
  var q n i32 = 1;
  return n;
}|};
  [%expect
    {|
    error: expected `;`
      at <test>:2:9
          var q n i32 = 1;
                ^ found n
    error: expected `:`
      at <test>:2:11
          var q n i32 = 1;
                  ^~~ found i32
    |}]

let%expect_test
    ("parse: a missing colon before a dotted type is said once"
     [@tags "disabled"]) =
  run_src {|fn f() i32 {
  var n m.t = 1;
  return 0;
}|};
  [%expect
    {|
    error: expected `:`
      at <test>:2:9
          var n m.t = 1;
                ^ found m
    error: undefined type
      at <test>:2:9
          var n m.t = 1;
                ^~~
    |}]

let%expect_test "parse: a value as a binding type keeps the initializer" =
  run_src {|fn f() i32 {
  var n: 5 = 1;
  return n;
}|};
  [%expect
    {|
    error: expected type
      at <test>:2:10
          var n: 5 = 1;
                 ^ found 5
    |}]

let%expect_test "parse: a wrong alias separator is said once" =
  run_src {|type t: i32;
fn main() i32 { return 0 }|};
  [%expect
    {|
    error: expected `=`
      at <test>:1:7
        type t: i32;
              ^ found :
    |}]

let%expect_test "parse: a value as an alias type is said once" =
  run_src {|type t = 5;
fn main() i32 { return 0 }|};
  [%expect
    {|
    error: expected type
      at <test>:1:10
        type t = 5;
                 ^ found 5
    |}]

let%expect_test
    ("parse: a broken statement keeps the rest of a value block"
     [@tags "disabled"]) =
  run_src
    {|fn main() i32 {
  var x: i32 = {
    var a = nope;
    );
    1
  };
  return x;
}|};
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:4:5
            );
            ^ expected `}`
      at <test>:2:16
          var x: i32 = {
                       ^ to match this `{`
    |}]

let%expect_test
    ("parse: an unknown escape after a delimiter fault" [@tags "disabled"]) =
  run_src {|fn f() { var s = (1]; var t = "a\qb"; }|};
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:1:20
        fn f() { var s = (1]; var t = "a\qb"; }
                           ^ expected `)`
      at <test>:1:18
        fn f() { var s = (1]; var t = "a\qb"; }
                         ^ to match this `(`
    error: unknown escape
      at <test>:1:34
        fn f() { var s = (1]; var t = "a\qb"; }
                                         ^
    |}]

let%expect_test
    ("parse: lexer errors after a delimiter fault" [@tags "disabled"]) =
  run_src {|fn f() {
  var s = (1];
  var t = "abc
  var c = @;
}|};
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:2:13
          var s = (1];
                    ^ expected `)`
      at <test>:2:11
          var s = (1];
                  ^ to match this `(`
    error: unterminated string
      at <test>:3:11
          var t = "abc
                  ^~~~
    error: unexpected character
      at <test>:4:11
          var c = @;
                  ^
    |}]
