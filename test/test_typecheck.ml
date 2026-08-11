(* SPDX-License-Identifier: GPL-2.0-only *)

open Span_utils
open Pipeline

let%expect_test "typecheck: break outside loop" =
  run_src "func f() { break }";
  [%expect
    {|
    error: `break` outside a loop
      at <test>:1:12
        func f() { break }
                   ^~~~~
    |}]

let%expect_test "typecheck: continue outside loop" =
  run_src "func f() { continue }";
  [%expect
    {|
    error: `continue` outside a loop
      at <test>:1:12
        func f() { continue }
                   ^~~~~~~~
    |}]

let%expect_test "typecheck: unbound variable" =
  run_src "func f() { x }";
  [%expect
    {|
    error: undefined variable
      at <test>:1:12
        func f() { x }
                   ^
    |}]

let%expect_test "typecheck: type mismatch in let" =
  run_src "func f() { let x: bool = 42 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x: bool = 42 }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:26
        func f() { let x: bool = 42 }
                                 ^~ expected bool, found i32
    |}]

let%expect_test "typecheck: int literal a float can't hold exactly" =
  run_src "func f() { var x: f32 = 16777217 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x: f32 = 16777217 }
                       ^
    help: prefix with an underscore: _x
    error: integer literal loses precision
      at <test>:1:25
        func f() { var x: f32 = 16777217 }
                                ^~~~~~~~ f32 can't represent this exactly
    |}]

let%expect_test "typecheck: unused parameter warns" =
  run_src "func g(used: i32, _skip: i32, dead: i32) i32 { return used }";
  [%expect
    {|
    warning: unused variable: dead
      at <test>:1:31
        func g(used: i32, _skip: i32, dead: i32) i32 { return used }
                                      ^~~~~~~~~
    help: prefix with an underscore: _dead
    ok
    |}]

let%expect_test "typecheck: wrong number of arguments" =
  run_src {|
func g() {}
func f() { g(1) }
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:3:12
        func f() { g(1) }
                   ^~~~ expected 0 arguments, found 1
    |}]

let%expect_test "typecheck: null assigned to non-pointer" =
  run_src "func f() { let x: i32 = null }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x: i32 = null }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:25
        func f() { let x: i32 = null }
                                ^~~~ expected i32, found null
    |}]

let%expect_test "typecheck: identity function" =
  run_src "func id(a: i32) i32 { return a }";
  [%expect {| ok |}]

let%expect_test "typecheck: null assigned to pointer" =
  run_src "func f() { let p: *i32 = null }";
  [%expect
    {|
    warning: unused variable: p
      at <test>:1:16
        func f() { let p: *i32 = null }
                       ^
    help: prefix with an underscore: _p
    ok
    |}]

let%expect_test "typecheck: array does not coerce to slice under a pointer" =
  run_src "func takes(p: *[]i32) { }\nfunc f() { var a: [3]i32\n  takes(&a) }";
  [%expect
    {|
    warning: unused variable: p
      at <test>:1:12
        func takes(p: *[]i32) { }
                   ^~~~~~~~~
    help: prefix with an underscore: _p
    error: type mismatch
      at <test>:3:9
          takes(&a) }
                ^~ expected *[]i32, found *[3]i32
    |}]

let%expect_test "typecheck: break inside while" =
  run_src "func f() { while true { break } }";
  [%expect {| ok |}]

let%expect_test "typecheck: unreachable code after break" =
  run_src "func f() { while true { break\n    g() } }\nfunc g() {}";
  [%expect
    {|
    warning: unreachable code
      at <test>:2:5
            g() } }
            ^~~
    ok
    |}]

let%expect_test "typecheck: unreachable code after continue" =
  run_src "func f() { while true { continue\n    g() } }\nfunc g() {}";
  [%expect
    {|
    warning: unreachable code
      at <test>:2:5
            g() } }
            ^~~
    ok
    |}]

let%expect_test "typecheck: unreachable code after a returning if" =
  run_src
    "func f() i32 { if true { return 1 } else { return 2 }\n\
    \    g() }\n\
     func g() {}";
  [%expect
    {|
    warning: unreachable code
      at <test>:2:5
            g() }
            ^~~
    ok
    |}]

let%expect_test "typecheck: forward reference" =
  run_src {|
func f() { g() }
func g() {}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: call no args" =
  run_src {|
func g() {}
func f() { g() }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: call with args" =
  run_src {|
func add(x: i32, y: i32) {}
func f() { add(1, 2) }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:2:10
        func add(x: i32, y: i32) {}
                 ^~~~~~
    help: prefix with an underscore: _x
    warning: unused variable: y
      at <test>:2:18
        func add(x: i32, y: i32) {}
                         ^~~~~~
    help: prefix with an underscore: _y
    ok
    |}]

let%expect_test "typecheck: fn ptr assign and call" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() {
  var op: func (i32, i32) i32 = add
  op(1, 2)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr inferred from function name" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() {
  var op = add
  op(1, 2)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr signature mismatch" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() {
  var op: func (i32) i32 = add
}
|};
  [%expect
    {|
    warning: unused variable: op
      at <test>:4:7
          var op: func (i32) i32 = add
              ^~
    help: prefix with an underscore: _op
    error: type mismatch
      at <test>:4:28
          var op: func (i32) i32 = add
                                   ^~~ expected func (i32) i32, found func (i32, i32) i32
    |}]

let%expect_test "typecheck: non-callable variable" =
  run_src {|
func f() {
  var x: i32 = 5
  x(1)
}
|};
  [%expect
    {|
    error: not callable
      at <test>:4:3
          x(1)
          ^ this has type i32
    |}]

let%expect_test "typecheck: fn ptr as parameter" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func apply(f: func (i32, i32) i32, a: i32, b: i32) i32 { return f(a, b) }
func g() { apply(add, 1, 2) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr wrong arity at call" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() {
  var op: func (i32, i32) i32 = add
  op(1)
}
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:5:3
          op(1)
          ^~~~~ expected 2 arguments, found 1
    |}]

let%expect_test "typecheck: fn ptr forward reference" =
  run_src
    {|
func f() {
  var op: func (i32, i32) i32 = add
  op(1, 2)
}
func add(a: i32, b: i32) i32 { return a + b }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr returning fn ptr" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func get_op() func (i32, i32) i32 { return add }
func f() {
  var op = get_op()
  op(1, 2)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: void fn ptr zero args" =
  run_src {|
func noop() {}
func f() {
  var p: func () = noop
  p()
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global let read from function" =
  run_src {|
let X: i32 = 42
func f() i32 { return X }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global var read and write" =
  run_src {|
var n: i32 = 0
func f() i32 {
  n = n + 1
  return n
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global var zero init" =
  run_src {|
var flag: bool
func f() bool { return flag }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global forward reference" =
  run_src {|
func f() i32 { return X }
let X: i32 = 7
|};
  [%expect {| ok |}]

let%expect_test "typecheck: assign to let global" =
  run_src {|
let X: i32 = 1
func f() { X = 2 }
|};
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:3:12
        func f() { X = 2 }
                   ^
    |}]

let%expect_test "typecheck: write to a let struct field" =
  run_src
    {|
struct P { x: i32, y: i32 }
func f() {
  let p: P = P { x: 1, y: 2 }
  p.x = 5
}
|};
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:5:3
          p.x = 5
          ^~~
    |}]

let%expect_test "typecheck: write to a let array element" =
  run_src {|
func f() {
  let arr: [3]i32 = [1, 2, 3]
  arr[0] = 9
}
|};
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:4:3
          arr[0] = 9
          ^~~~~~
    |}]

let%expect_test "typecheck: write through a let pointer" =
  run_src {|
var g: i32 = 0
func f() {
  let p: *i32 = &g
  *p = 5
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: non-let global initializer" =
  run_src {|
func g() i32 { return 1 }
let X: i32 = g()
|};
  [%expect
    {|
    error: initializer must be constant
      at <test>:3:14
        let X: i32 = g()
                     ^~~
    |}]

let%expect_test "typecheck: let requires initializer" =
  run_src "let X: i32";
  [%expect
    {|
    error: let without initializer
      at <test>:1:5
        let X: i32
            ^
    |}]

let%expect_test "typecheck: comptime requires initializer" =
  run_src "comptime X: i32";
  [%expect
    {|
    error: comptime without initializer
      at <test>:1:10
        comptime X: i32
                 ^
    |}]

let%expect_test "typecheck: comptime cannot be undefined" =
  run_src "comptime N: i32 = undefined";
  [%expect
    {|
    error: comptime cannot be undefined
      at <test>:1:19
        comptime N: i32 = undefined
                          ^~~~~~~~~
    help: use let for values that need storage
    |}]

let%expect_test "typecheck: cannot take address of a comptime global" =
  run_src {|
comptime N: i32 = 4
func f() *i32 { return &N }
|};
  [%expect
    {|
    error: cannot take address of a constant
      at <test>:3:25
        func f() *i32 { return &N }
                                ^
    help: a const has no storage, use let
    |}]

let%expect_test "typecheck: cannot take address of a local comptime" =
  run_src {|
func f() {
  comptime c: i32 = 2
  var p: *i32 = &c
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:4:7
          var p: *i32 = &c
              ^
    help: prefix with an underscore: _p
    error: cannot take address of a constant
      at <test>:4:18
          var p: *i32 = &c
                         ^
    help: a const has no storage, use let
    |}]

let%expect_test "typecheck: comptime must be a scalar" =
  run_src {|
func f() {
  comptime a: [2]i32 = [1, 2]
}
|};
  [%expect
    {|
    error: comptime must be a scalar
      at <test>:3:12
          comptime a: [2]i32 = [1, 2]
                   ^ on [2]i32
    help: use let for values that need storage
    warning: unused variable: a
      at <test>:3:12
          comptime a: [2]i32 = [1, 2]
                   ^
    help: prefix with an underscore: _a
    |}]

let%expect_test "typecheck: comptime cstr is not a scalar" =
  run_src {|
comptime S: cstr = "x"
|};
  [%expect
    {|
    error: comptime must be a scalar
      at <test>:2:1
        comptime S: cstr = "x"
        ^~~~~~~~~~~~~~~~~~~~~~ on cstr
    help: use let for values that need storage
    |}]

let%expect_test "typecheck: local comptime initializer must fold" =
  run_src
    {|
func g() i32 { return 3 }
func f() i32 {
  comptime c: i32 = g()
  return c
}
|};
  [%expect
    {|
    error: unsupported constant expression
      at <test>:4:21
          comptime c: i32 = g()
                            ^~~
    help: constant initializers must fold to a compile-time value
    |}]

let%expect_test "typecheck: mutually referential consts are a cycle" =
  run_src {|
comptime A: i32 = B
comptime B: i32 = A
|};
  [%expect
    {|
    error: cyclic constant
      at <test>:3:19
        comptime B: i32 = A
                          ^
    |}]

let%expect_test "typecheck: cannot assign to a comptime" =
  run_src {|
func f() i32 {
  comptime c: i32 = 2
  c = 3
  return c
}
|};
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:4:3
          c = 3
          ^
    |}]

let%expect_test "typecheck: local comptime reads an earlier comptime" =
  run_src
    {|
func f() i32 {
  comptime a: i32 = 2
  comptime b: i32 = a * 3
  return b
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size from a later comptime" =
  run_src
    {|
var a: [N]i32 = undefined
comptime N: i32 = 3
func f() i32 { return a[0] }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size expression" =
  run_src
    {|
comptime N: i32 = 4
func f() i32 {
  var a: [N * 2 + 1]i32 = undefined
  a[8] = 1
  return a[8]
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size with a suffix" =
  run_src {|
func f() i32 {
  var a: [2u8]i32 = [1, 2]
  return a[1]
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: struct field sized by a later comptime" =
  run_src
    {|
struct S { buf: [N]i32 }
comptime N: i32 = 2
func f(s: S) i32 { return s.buf[1] }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: local comptime sizes a local array" =
  run_src
    {|
func f() i32 {
  comptime n: i32 = 3
  var a: [n]i32 = [1, 2, 3]
  return a[2]
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: negative array size" =
  run_src {|
var a: [0 - 1]i32 = undefined
|};
  [%expect
    {|
    error: array size is negative: -1
      at <test>:2:9
        var a: [0 - 1]i32 = undefined
                ^~~~~
    |}]

let%expect_test "typecheck: bad array size in a param errors once" =
  run_src {|
func f(a: [0 - 1]i32) {}
|};
  [%expect
    {|
    warning: unused variable: a
      at <test>:2:8
        func f(a: [0 - 1]i32) {}
               ^~~~~~~~~~~~~
    help: prefix with an underscore: _a
    error: array size is negative: -1
      at <test>:2:12
        func f(a: [0 - 1]i32) {}
                   ^~~~~
    |}]

let%expect_test "typecheck: huge array size" =
  run_src {|
var a: [9999999999i64]i32 = undefined
|};
  [%expect
    {|
    error: array size is too large: 9999999999
      at <test>:2:9
        var a: [9999999999i64]i32 = undefined
                ^~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: huge unsigned array size" =
  run_src {|
var a: [(0 - 1) as u64]i32 = undefined
|};
  [%expect
    {|
    error: array size is too large: 18446744073709551615
      at <test>:2:9
        var a: [(0 - 1) as u64]i32 = undefined
                ^~~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: array size literal with a type suffix" =
  run_src {|
var a: [2u8]i32 = undefined
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size literal suffix still range checks" =
  run_src {|
var a: [300u8]i32 = undefined
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:2:9
        var a: [300u8]i32 = undefined
                ^~~~~ does not fit in u8
    |}]

let%expect_test "typecheck: float array size" =
  run_src {|
var a: [1.5]i32 = undefined
|};
  [%expect
    {|
    error: array size must be an integer
      at <test>:2:9
        var a: [1.5]i32 = undefined
                ^~~
    |}]

let%expect_test "typecheck: array size names a var" =
  run_src {|
var n: i32 = 3
var a: [n]i32 = undefined
|};
  [%expect
    {|
    error: unsupported constant expression
      at <test>:3:9
        var a: [n]i32 = undefined
                ^
    help: constant initializers must fold to a compile-time value
    |}]

let%expect_test "typecheck: array size calls a function" =
  run_src {|
func g() i32 { return 3 }
var a: [g()]i32 = undefined
|};
  [%expect
    {|
    error: unsupported constant expression
      at <test>:3:9
        var a: [g()]i32 = undefined
                ^~~
    help: constant initializers must fold to a compile-time value
    |}]

let%expect_test "typecheck: cycle through an array size" =
  run_src
    {|
comptime N: i32 = sizeof([M]i32)
comptime M: i32 = sizeof([N]i32)
|};
  [%expect
    {|
    error: type mismatch
      at <test>:2:19
        comptime N: i32 = sizeof([M]i32)
                          ^~~~~~~~~~~~~~ expected i32, found usize
    error: type mismatch
      at <test>:3:19
        comptime M: i32 = sizeof([N]i32)
                          ^~~~~~~~~~~~~~ expected i32, found usize
    error: cyclic constant
      at <test>:3:27
        comptime M: i32 = sizeof([N]i32)
                                  ^
    |}]

let%expect_test "typecheck: int arithmetic ok" =
  run_src "func f() i32 { return 1 + 2 * 3 - 4 }";
  [%expect {| ok |}]

let%expect_test "typecheck: mixed int widths" =
  run_src {|
func f() {
  let a: i32 = 1
  let b: i64 = 2
  let c = a + b
}
|};
  [%expect
    {|
    warning: unused variable: c
      at <test>:5:7
          let c = a + b
              ^
    help: prefix with an underscore: _c
    error: type mismatch
      at <test>:5:15
          let c = a + b
                      ^ expected i32, found i64
    |}]

let%expect_test "typecheck: bool arithmetic rejected" =
  run_src "func f() { let x = true + false }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x = true + false }
                       ^
    help: prefix with an underscore: _x
    error: invalid operand
      at <test>:1:20
        func f() { let x = true + false }
                           ^~~~ cannot apply `+` to bool
    |}]

let%expect_test "typecheck: comparison yields bool" =
  run_src "func f() bool { return 1 < 2 }";
  [%expect {| ok |}]

let%expect_test "typecheck: logical and/or require bool" =
  run_src "func f() { let x = 1 && 2 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x = 1 && 2 }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:20
        func f() { let x = 1 && 2 }
                           ^ expected bool, found i32
    error: type mismatch
      at <test>:1:25
        func f() { let x = 1 && 2 }
                                ^ expected bool, found i32
    |}]

let%expect_test "typecheck: not on non-bool" =
  run_src "func f() { let x = !1 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x = !1 }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:21
        func f() { let x = !1 }
                            ^ expected bool, found i32
    |}]

let%expect_test "typecheck: address of rvalue rejected" =
  run_src "func f() { let x = &5 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x = &5 }
                       ^
    help: prefix with an underscore: _x
    error: cannot take address of expression
      at <test>:1:21
        func f() { let x = &5 }
                            ^
    |}]

let%expect_test "typecheck: shift on int ok" =
  run_src "func f() i32 { return 1 << 3 }";
  [%expect {| ok |}]

let%expect_test "typecheck: shift literal takes target type" =
  run_src "func f() u8 { return 5 << 2 }";
  [%expect {| ok |}]

let%expect_test "typecheck: bitwise on bool rejected" =
  run_src "func f() { let x = true & false }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x = true & false }
                       ^
    help: prefix with an underscore: _x
    error: invalid operand
      at <test>:1:20
        func f() { let x = true & false }
                           ^~~~ cannot apply `&` to bool
    |}]

let%expect_test "typecheck: int to int cast" =
  run_src "func f() i64 { return 1 as i64 }";
  [%expect {| ok |}]

let%expect_test "typecheck: checked int cast" =
  run_src "func f() u8 { return 300 as! u8 }";
  [%expect {| ok |}]

let%expect_test "typecheck: checked cast on a float rejected" =
  run_src "func f() i32 { var x: f64 = 2.5; return x as! i32 }";
  [%expect
    {|
    error: checked cast only supports integers
      at <test>:1:41
        func f() i32 { var x: f64 = 2.5; return x as! i32 }
                                                ^~~~~~~~~ `as!` traps on integer overflow only
    help: use a plain `as` cast here
    |}]

let%expect_test "typecheck: sizeof has usize type" =
  run_src "func f() usize { return sizeof(i32) }";
  [%expect {| ok |}]

let%expect_test "typecheck: cast bool to ptr rejected" =
  run_src "func f() { let p: *i32 = true as *i32 }";
  [%expect
    {|
    warning: unused variable: p
      at <test>:1:16
        func f() { let p: *i32 = true as *i32 }
                       ^
    help: prefix with an underscore: _p
    ok
    |}]

let%expect_test "typecheck: cast cstr to float rejected" =
  run_src {|func f() { let x: f32 = "hi" as f32 }|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { let x: f32 = "hi" as f32 }
                       ^
    help: prefix with an underscore: _x
    error: invalid cast
      at <test>:1:25
        func f() { let x: f32 = "hi" as f32 }
                                ^~~~~~~~~~~ cannot cast *i8 to f32
    |}]

let%expect_test "typecheck: cast struct to float rejected" =
  run_src {|
struct S { x: i32 }
func f() { var s: S; let y: f64 = s as f64 }
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:3:26
        func f() { var s: S; let y: f64 = s as f64 }
                                 ^
    help: prefix with an underscore: _y
    error: invalid cast
      at <test>:3:35
        func f() { var s: S; let y: f64 = s as f64 }
                                          ^~~~~~~~ cannot cast S to f64
    |}]

let%expect_test "typecheck: cast int to bool rejected" =
  run_src "func f() { let b: bool = 256 as bool }";
  [%expect
    {|
    warning: unused variable: b
      at <test>:1:16
        func f() { let b: bool = 256 as bool }
                       ^
    help: prefix with an underscore: _b
    error: invalid cast
      at <test>:1:26
        func f() { let b: bool = 256 as bool }
                                 ^~~~~~~~~~~ cannot cast i32 to bool
    help: compare with zero instead e.g. `x != 0`
    |}]

let%expect_test "typecheck: missing return value" =
  run_src "func f() i32 { return }";
  [%expect
    {|
    error: empty return in non-void function
      at <test>:1:16
        func f() i32 { return }
                       ^~~~~~
    |}]

let%expect_test "typecheck: return value in void fn" =
  run_src "func f() { return 1 }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:19
        func f() { return 1 }
                          ^ expected void, found i32
    |}]

let%expect_test "typecheck: return type mismatch" =
  run_src "func f() i32 { return true }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:23
        func f() i32 { return true }
                              ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: if condition must be bool" =
  run_src "func f() { if 1 {} }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:15
        func f() { if 1 {} }
                      ^ expected bool, found i32
    |}]

let%expect_test "typecheck: while condition must be bool" =
  run_src "func f() { while 1 {} }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:18
        func f() { while 1 {} }
                         ^ expected bool, found i32
    |}]

let%expect_test "typecheck: if/else ok" =
  run_src "func f() i32 { if true { return 1 } else { return 2 } }";
  [%expect {| ok |}]

let%expect_test "typecheck: nested loops break ok" =
  run_src "func f() { while true { while true { break } } }";
  [%expect {| ok |}]

let%expect_test "typecheck: assign to let local" =
  run_src {|
func f() {
  let x: i32 = 1
  x = 2
}
|};
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:4:3
          x = 2
          ^
    |}]

let%expect_test "typecheck: redeclare local shadows" =
  run_src {|
func f() {
  let x: i32 = 1
  let x: i32 = 2
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:7
          let x: i32 = 1
              ^
    help: prefix with an underscore: _x
    warning: unused variable: x
      at <test>:4:7
          let x: i32 = 2
              ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: shadow can change type and the new type wins" =
  run_src {|
func f() i64 {
  var x: i32 = 1
  var x: i64 = 2
  return x
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:7
          var x: i32 = 1
              ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: shadow reads the old binding in its initializer" =
  run_src
    {|
func f() i32 {
  var x: i32 = 1
  var x: i32 = x + 4
  return x
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type annot mismatch on var" =
  run_src "func f() { var x: bool = 1 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x: bool = 1 }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:26
        func f() { var x: bool = 1 }
                                 ^ expected bool, found i32
    |}]

let%expect_test "typecheck: use before decl" =
  run_src {|
func f() {
  x
  let x: i32 = 1
}
|};
  [%expect
    {|
    error: undefined variable
      at <test>:3:3
          x
          ^
    |}]

let%expect_test "typecheck: deref non-pointer" =
  run_src {|
func f() {
  let x: i32 = 1
  let y = *x
}
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:4:7
          let y = *x
              ^
    help: prefix with an underscore: _y
    error: cannot dereference
      at <test>:4:12
          let y = *x
                   ^ on i32
    |}]

let%expect_test "typecheck: a failed check does not cascade" =
  run_src {|
func f() {
  let n: i32 = 1
  let _y = *n.x + 1
}
|};
  [%expect
    {|
    error: type has no fields
      at <test>:4:13
          let _y = *n.x + 1
                    ^~~ on i32
    |}]

let%expect_test "typecheck: address-of and deref roundtrip" =
  run_src {|
func f() i32 {
  var x: i32 = 5
  let p: *i32 = &x
  return *p
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: struct field read" =
  run_src {|
struct pt { x: i32, y: i32 }
func f(p: pt) i32 { return p.x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: unknown struct field" =
  run_src {|
struct pt { x: i32, y: i32 }
func f(p: pt) i32 { return p.z }
|};
  [%expect
    {|
    error: no field
      at <test>:3:30
        func f(p: pt) i32 { return p.z }
                                     ^ on struct pt
    |}]

let%expect_test "typecheck: field access on non-struct" =
  run_src {|
func f() {
  let x: i32 = 1
  let y = x.foo
}
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:4:7
          let y = x.foo
              ^
    help: prefix with an underscore: _y
    error: type has no fields
      at <test>:4:11
          let y = x.foo
                  ^~~~~ on i32
    |}]

let%expect_test "typecheck: field access auto-deref through ptr" =
  run_src {|
struct pt { x: i32 }
func f(p: *pt) i32 { return p.x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: field access through a double pointer" =
  run_src {|
struct pt { x: i32 }
func f(p: **pt) i32 { return p.x }
|};
  [%expect
    {|
    error: too many pointer levels
      at <test>:3:30
        func f(p: **pt) i32 { return p.x }
                                     ^~~
    help: dereference first: `(*p).x`
    |}]

let%expect_test "typecheck: duplicate function" =
  run_src {|
func f() {}
func f() {}
|};
  [%expect
    {|
    error: already defined
      at <test>:3:6
        func f() {}
             ^
      at <test>:2:6
        func f() {}
             ^ previous definition here
    |}]

let%expect_test "typecheck: arg type mismatch" =
  run_src {|
func g(x: i32) {}
func f() { g(true) }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:2:8
        func g(x: i32) {}
               ^~~~~~
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:3:14
        func f() { g(true) }
                     ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: extern decl callable" =
  run_src
    {|
extern "C" func puts(s: *i8) i32
func f() {
  var p: *i8 = null
  puts(p)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array literal inferred" =
  run_src "func f() { let a = [1, 2, 3]; a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: array annotated ok" =
  run_src "func f() { var a: [3]i32 = [1, 2, 3]; a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: array element type mismatch" =
  run_src "func f() { var a: [2]i32 = [1, true] }";
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:16
        func f() { var a: [2]i32 = [1, true] }
                       ^
    help: prefix with an underscore: _a
    error: type mismatch
      at <test>:1:32
        func f() { var a: [2]i32 = [1, true] }
                                       ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: array wrong element count" =
  run_src "func f() { var a: [3]i32 = [1, 2] }";
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:16
        func f() { var a: [3]i32 = [1, 2] }
                       ^
    help: prefix with an underscore: _a
    error: wrong number of arguments
      at <test>:1:28
        func f() { var a: [3]i32 = [1, 2] }
                                   ^~~~~~ expected 3 elements, found 2
    |}]

let%expect_test "typecheck: heterogeneous inferred literal" =
  run_src "func f() { let a = [1, true]; a[0] }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:24
        func f() { let a = [1, true]; a[0] }
                               ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: empty array literal needs annotation" =
  run_src "func f() { let a = [] }";
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:16
        func f() { let a = [] }
                       ^
    help: prefix with an underscore: _a
    error: cannot infer type of empty array literal
      at <test>:1:20
        func f() { let a = [] }
                           ^~
    |}]

let%expect_test "typecheck: index non-array" =
  run_src "func f() { var x: i32 = 0; x[0] }";
  [%expect
    {|
    error: cannot index
      at <test>:1:28
        func f() { var x: i32 = 0; x[0] }
                                   ^~~~ on i32
    |}]

let%expect_test "typecheck: index non-integer" =
  run_src "func f() { var a: [2]i32 = [1, 2]; a[true] }";
  [%expect
    {|
    error: array index must be an integer
      at <test>:1:38
        func f() { var a: [2]i32 = [1, 2]; a[true] }
                                             ^~~~
    |}]

let%expect_test "typecheck: index result type" =
  run_src "func f() i32 { var a: [2]i32 = [1, 2]; return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: len is usize" =
  run_src "func f() usize { var a: [2]i32 = [1, 2]; return a.len }";
  [%expect {| ok |}]

let%expect_test "typecheck: len mismatched with i32" =
  run_src "func f() i32 { var a: [2]i32 = [1, 2]; return a.len }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:47
        func f() i32 { var a: [2]i32 = [1, 2]; return a.len }
                                                      ^~~~~ expected i32, found usize
    |}]

let%expect_test "typecheck: array no such field" =
  run_src "func f() { var a: [2]i32 = [1, 2]; a.foo }";
  [%expect
    {|
    error: no field
      at <test>:1:38
        func f() { var a: [2]i32 = [1, 2]; a.foo }
                                             ^~~ on [2]i32
    |}]

let%expect_test "typecheck: assign to index" =
  run_src "func f() { var a: [2]i32 = [1, 2]; a[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: index element assign type mismatch" =
  run_src "func f() { var a: [2]i32 = [1, 2]; a[0] = true }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:43
        func f() { var a: [2]i32 = [1, 2]; a[0] = true }
                                                  ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: for over range ok (branch 2)" =
  run_src "func f() { for i in 0..5 { let x = i } }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:32
        func f() { for i in 0..5 { let x = i } }
                                       ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: for over inclusive range ok (branch 2)" =
  run_src "func f() { for i in 0..=5 { let x = i } }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:33
        func f() { for i in 0..=5 { let x = i } }
                                        ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: for over array binds element type" =
  run_src
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  for x in a { let y: i32 = x }
}
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:4:20
          for x in a { let y: i32 = x }
                           ^
    help: prefix with an underscore: _y
    ok
    |}]

let%expect_test "typecheck: for over array wrong element use" =
  run_src
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  for x in a { let y: bool = x }
}
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:4:20
          for x in a { let y: bool = x }
                           ^
    help: prefix with an underscore: _y
    error: type mismatch
      at <test>:4:30
          for x in a { let y: bool = x }
                                     ^ expected bool, found i32
    |}]

let%expect_test "typecheck: for over non-iterable" =
  run_src "func f() { for x in 5 { let y = x } }";
  [%expect
    {|
    error: cannot iterate
      at <test>:1:21
        func f() { for x in 5 { let y = x } }
                            ^ on i32
    warning: unused variable: y
      at <test>:1:29
        func f() { for x in 5 { let y = x } }
                                    ^
    help: prefix with an underscore: _y
    |}]

let%expect_test "typecheck: range bounds must be integers (branch 2)" =
  run_src "func f() { for i in true..5 { let x = i } }";
  [%expect
    {|
    error: range bounds must be integers
      at <test>:1:21
        func f() { for i in true..5 { let x = i } }
                            ^~~~
    error: type mismatch
      at <test>:1:27
        func f() { for i in true..5 { let x = i } }
                                  ^ expected bool, found i32
    warning: unused variable: x
      at <test>:1:35
        func f() { for i in true..5 { let x = i } }
                                          ^
    help: prefix with an underscore: _x
    |}]

let%expect_test "typecheck: range literal bends to typed endpoint (branch 1)" =
  run_src {|
func f() {
  let n: i64 = 5
  for i in 0..n { let x = i }
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:4:23
          for i in 0..n { let x = i }
                              ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: range over len needs no cast (branch 1)" =
  run_src
    {|
func f() {
  var a: [4]i32 = [1, 2, 3, 4]
  for i in 0..a.len { a[i] = 0 }
}
|};
  [%expect {| ok |}]

let%expect_test
    "typecheck: typed left endpoint bends the literal right (branch 2)" =
  run_src {|
func f() {
  let n: i64 = 5
  for i in n..10 { let x = i }
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:4:24
          for i in n..10 { let x = i }
                               ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: slice bound over len needs no cast (branch 1)" =
  run_src
    {|
func f() {
  var a: [4]i32 = [1, 2, 3, 4]
  let s: []i32 = a[0..a.len]
}
|};
  [%expect
    {|
    warning: unused variable: s
      at <test>:4:7
          let s: []i32 = a[0..a.len]
              ^
    help: prefix with an underscore: _s
    ok
    |}]

let%expect_test "typecheck: two typed endpoints still must match (branch 2)" =
  run_src
    {|
func f() {
  let m: i32 = 0
  let n: i64 = 5
  for i in m..n { let x = i }
}
|};
  [%expect
    {|
    error: type mismatch
      at <test>:5:15
          for i in m..n { let x = i }
                      ^ expected i32, found i64
    warning: unused variable: x
      at <test>:5:23
          for i in m..n { let x = i }
                              ^
    help: prefix with an underscore: _x
    |}]

let%expect_test "typecheck: break inside for" =
  run_src "func f() { for i in 0..5 { break } }";
  [%expect
    {|
    warning: unused variable: i
      at <test>:1:16
        func f() { for i in 0..5 { break } }
                       ^
    help: prefix with an underscore: _i
    ok
    |}]

let%expect_test "typecheck: continue inside for" =
  run_src "func f() { for i in 0..5 { continue } }";
  [%expect
    {|
    warning: unused variable: i
      at <test>:1:16
        func f() { for i in 0..5 { continue } }
                       ^
    help: prefix with an underscore: _i
    ok
    |}]

let%expect_test "typecheck: unused loop variable warns" =
  run_src "func f() { for i in 0..5 { let _x = 1 } }";
  [%expect
    {|
    warning: unused variable: i
      at <test>:1:16
        func f() { for i in 0..5 { let _x = 1 } }
                       ^
    help: prefix with an underscore: _i
    ok
    |}]

let%expect_test "typecheck: a bare array is not a slice param" =
  run_src
    {|
func sum(xs: []i32) i32 { return 0 }
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  return sum(a)
}
|};
  [%expect
    {|
    warning: unused variable: xs
      at <test>:2:10
        func sum(xs: []i32) i32 { return 0 }
                 ^~~~~~~~~
    help: prefix with an underscore: _xs
    error: type mismatch
      at <test>:5:14
          return sum(a)
                     ^ expected []i32, found [3]i32
    |}]

let%expect_test "typecheck: slice element wrong type rejected" =
  run_src
    {|
func sum(xs: []i32) {}
func f() {
  var a: [2]f32 = [1.0, 2.0]
  sum(a)
}
|};
  [%expect
    {|
    warning: unused variable: xs
      at <test>:2:10
        func sum(xs: []i32) {}
                 ^~~~~~~~~
    help: prefix with an underscore: _xs
    error: type mismatch
      at <test>:5:7
          sum(a)
              ^ expected []i32, found [2]f32
    |}]

let%expect_test "typecheck: sub-slice ok" =
  run_src
    "func f() i32 { var a: [4]i32 = [1,2,3,4]; let s: []i32 = a[1..3]; return \
     s[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: slice of a slice ok" =
  run_src
    {|
func f() i32 {
  var a: [5]i32 = [1, 2, 3, 4, 5]
  let s: []i32 = a[1..5]
  let t: []i32 = s[1..3]
  return t[0]
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: returning a slice of a local rejected" =
  run_src "func f() []i32 { var a: [3]i32 = [1,2,3]; return a[0..2] }";
  [%expect
    {|
    error: slice of a local escapes
      at <test>:1:50
        func f() []i32 { var a: [3]i32 = [1,2,3]; return a[0..2] }
                                                         ^~~~~~~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning a slice of a local let rejected" =
  run_src "func f() []i32 { let a: [3]i32 = [1,2,3]; return a[0..2] }";
  [%expect
    {|
    error: slice of a local escapes
      at <test>:1:50
        func f() []i32 { let a: [3]i32 = [1,2,3]; return a[0..2] }
                                                         ^~~~~~~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning a local array as a slice rejected" =
  run_src "func f() []i32 { var a: [3]i32 = [1,2,3]; return a[..] }";
  [%expect
    {|
    error: slice of a local escapes
      at <test>:1:50
        func f() []i32 { var a: [3]i32 = [1,2,3]; return a[..] }
                                                         ^~~~~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning a slice of an array param rejected" =
  run_src "func f(a: [3]i32) []i32 { return a[0..2] }";
  [%expect
    {|
    error: slice of a local escapes
      at <test>:1:34
        func f(a: [3]i32) []i32 { return a[0..2] }
                                         ^~~~~~~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning a sub-slice of a slice param ok" =
  run_src "func f(xs: []i32) []i32 { return xs[0..2] }";
  [%expect {| ok |}]

let%expect_test "typecheck: returning a slice param ok" =
  run_src "func f(xs: []i32) []i32 { return xs }";
  [%expect {| ok |}]

let%expect_test "typecheck: returning the address of a local rejected" =
  run_src "func f() *i32 { var x: i32 = 5; return &x }";
  [%expect
    {|
    error: address of a local escapes
      at <test>:1:40
        func f() *i32 { var x: i32 = 5; return &x }
                                               ^~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning the address of a local field rejected" =
  run_src
    "struct S { a: i32 }; func f() *i32 { var s: S = S{a: 1}; return &s.a }";
  [%expect
    {|
    error: address of a local escapes
      at <test>:1:65
        struct S { a: i32 }; func f() *i32 { var s: S = S{a: 1}; return &s.a }
                                                                        ^~~~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning the address of a local element rejected" =
  run_src "func f() *i32 { var a: [3]i32 = [1,2,3]; return &a[0] }";
  [%expect
    {|
    error: address of a local escapes
      at <test>:1:49
        func f() *i32 { var a: [3]i32 = [1,2,3]; return &a[0] }
                                                        ^~~~~ points into freed stack memory
    |}]

let%expect_test
    "typecheck: returning the address of a whole local array rejected" =
  run_src "func f() *[3]i32 { var a: [3]i32 = [1,2,3]; return &a }";
  [%expect
    {|
    error: address of a local escapes
      at <test>:1:52
        func f() *[3]i32 { var a: [3]i32 = [1,2,3]; return &a }
                                                           ^~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning the address of a local pointer rejected" =
  run_src "func f() **i32 { var x: i32 = 5; var p: *i32 = &x; return &p }";
  [%expect
    {|
    error: address of a local escapes
      at <test>:1:59
        func f() **i32 { var x: i32 = 5; var p: *i32 = &x; return &p }
                                                                  ^~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning a pointer param ok" =
  run_src "func f(p: *i32) *i32 { return p }";
  [%expect {| ok |}]

let%expect_test
    "typecheck: returning the address of a field through a pointer ok" =
  run_src "struct S { a: i32 }; func f(p: *S) *i32 { return &p.a }";
  [%expect {| ok |}]

let%expect_test "typecheck: returning a slice of a returned array rejected" =
  run_src
    "func g() [3]i32 { var a: [3]i32 = [1,2,3]; return a }; func f() []i32 { \
     return g()[0..2] }";
  [%expect
    {|
    error: slice of a local escapes
      at <test>:1:80
        func g() [3]i32 { var a: [3]i32 = [1,2,3]; return a }; func f() []i32 { return g()[0..2] }
                                                                                       ^~~~~~~~~ points into freed stack memory
    |}]

let%expect_test "typecheck: returning a slice from a slice call ok" =
  run_src
    "func g(xs: []i32) []i32 { return xs }; func f(xs: []i32) []i32 { return \
     g(xs) }";
  [%expect {| ok |}]

let%expect_test "typecheck: inclusive range slice ok (branch 2)" =
  run_src
    "func f() i32 { var a: [3]i32 = [1,2,3]; let s: []i32 = a[0..=2]; return \
     s[2] }";
  [%expect {| ok |}]

let%expect_test "typecheck: slice bounds must be integers (branch 2)" =
  run_src "func f() { var a: [3]i32 = [1,2,3]; let s: []i32 = a[true..2] }";
  [%expect
    {|
    warning: unused variable: s
      at <test>:1:41
        func f() { var a: [3]i32 = [1,2,3]; let s: []i32 = a[true..2] }
                                                ^
    help: prefix with an underscore: _s
    error: range bounds must be integers
      at <test>:1:54
        func f() { var a: [3]i32 = [1,2,3]; let s: []i32 = a[true..2] }
                                                             ^~~~
    error: type mismatch
      at <test>:1:60
        func f() { var a: [3]i32 = [1,2,3]; let s: []i32 = a[true..2] }
                                                                   ^ expected bool, found i32
    |}]

let%expect_test "typecheck: slice .len is usize" =
  run_src
    "func f() usize { var a: [3]i32 = [1,2,3]; let s: []i32 = a[0..3]; return \
     s.len }";
  [%expect {| ok |}]

let%expect_test "typecheck: slice .ptr is pointer" =
  run_src
    {|
func first(p: *i32) i32 { return 0 }
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  let s: []i32 = a[0..3]
  return first(s.ptr)
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:2:12
        func first(p: *i32) i32 { return 0 }
                   ^~~~~~~
    help: prefix with an underscore: _p
    ok
    |}]

let%expect_test "typecheck: slice does not coerce back to array" =
  run_src "func f() { var a: [3]i32 = [1,2,3]; var b: [3]i32 = a[0..3] }";
  [%expect
    {|
    warning: unused variable: b
      at <test>:1:41
        func f() { var a: [3]i32 = [1,2,3]; var b: [3]i32 = a[0..3] }
                                                ^
    help: prefix with an underscore: _b
    error: type mismatch
      at <test>:1:53
        func f() { var a: [3]i32 = [1,2,3]; var b: [3]i32 = a[0..3] }
                                                            ^~~~~~~ expected [3]i32, found []i32
    |}]

let%expect_test "typecheck: for over slice binds element" =
  run_src
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  let s: []i32 = a[0..3]
  for x in s { let y: i32 = x }
}
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:5:20
          for x in s { let y: i32 = x }
                           ^
    help: prefix with an underscore: _y
    ok
    |}]

let%expect_test "typecheck: slice index element assignable" =
  run_src
    "func f() { var a: [3]i32 = [1,2,3]; var s: []i32 = a[0..3]; s[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: compound assign to array element" =
  run_src "func f() i32 { var a: [3]i32 = [1,2,3]; a[0] += 5; return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: multidimensional array" =
  run_src "func f() i32 { var m: [2][2]i32 = [[1,2],[3,4]]; return m[1][0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: multidim wrong inner count" =
  run_src "func f() { var m: [2][2]i32 = [[1,2],[3]] }";
  [%expect
    {|
    warning: unused variable: m
      at <test>:1:16
        func f() { var m: [2][2]i32 = [[1,2],[3]] }
                       ^
    help: prefix with an underscore: _m
    error: wrong number of arguments
      at <test>:1:38
        func f() { var m: [2][2]i32 = [[1,2],[3]] }
                                             ^~~ expected 2 elements, found 1
    |}]

let%expect_test "typecheck: global array" =
  run_src {|
var g: [3]i32 = [7, 8, 9]
func f() i32 { return g[1] }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global array non-constant element rejected" =
  run_src {|
func k() i32 { return 1 }
var g: [2]i32 = [k(), 2]
|};
  [%expect
    {|
    error: initializer must be constant
      at <test>:3:17
        var g: [2]i32 = [k(), 2]
                        ^~~~~~~~
    |}]

let%expect_test "typecheck: iterate array of arrays" =
  run_src
    {|
func f() i32 {
  var m: [2][2]i32 = [[1, 2], [3, 4]]
  var s: i32 = 0
  for row in m { s += row[0] }
  return s
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: range as value is an error" =
  run_src "func f() i32 { return 0..5 }";
  [%expect
    {|
    error: range is only valid in a for loop or slice
      at <test>:1:23
        func f() i32 { return 0..5 }
                              ^~~~
    |}]

let%expect_test "typecheck: range in condition is an error" =
  run_src "func f() { if 0..5 { } }";
  [%expect
    {|
    error: range is only valid in a for loop or slice
      at <test>:1:15
        func f() { if 0..5 { } }
                      ^~~~
    |}]

let%expect_test "typecheck: for over array literal" =
  run_src
    "func f() i32 { var s: i32 = 0; for x in [1,2,3] { s += x }; return s }";
  [%expect {| ok |}]

let%expect_test "typecheck: array literal as slice argument" =
  run_src
    {|
func sum(xs: []i32) i32 { return 0 }
func f() i32 { return sum([1, 2, 3]) }
|};
  [%expect
    {|
    warning: unused variable: xs
      at <test>:2:10
        func sum(xs: []i32) i32 { return 0 }
                 ^~~~~~~~~
    help: prefix with an underscore: _xs
    error: type mismatch
      at <test>:3:27
        func f() i32 { return sum([1, 2, 3]) }
                                  ^~~~~~~~~ expected []i32, found [3]i32
    |}]

let%expect_test "typecheck: scalar zero init" =
  run_src "func f() i32 { var x: i32; return x }";
  [%expect {| ok |}]

let%expect_test "typecheck: array zero init" =
  run_src "func f() i32 { var a: [3]i32; return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: var without type or value cannot infer" =
  run_src "func f() { var x }";
  [%expect
    {|
    error: cannot infer type
      at <test>:1:16
        func f() { var x }
                       ^
    warning: unused variable: x
      at <test>:1:16
        func f() { var x }
                       ^
    help: prefix with an underscore: _x
    |}]

let%expect_test "typecheck: cannot infer does not cascade into the assignment" =
  run_src "func f() { var x\n  x = [1,2,3,4] }";
  [%expect
    {|
    error: cannot infer type
      at <test>:1:16
        func f() { var x
                       ^
    |}]

let%expect_test "typecheck: undefined without type cannot infer" =
  run_src "func f() { var x = undefined }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x = undefined }
                       ^
    help: prefix with an underscore: _x
    error: cannot infer type of undefined
      at <test>:1:20
        func f() { var x = undefined }
                           ^~~~~~~~~
    |}]

let%expect_test "typecheck: undefined with type ok" =
  run_src "func f() i32 { var x: i32 = undefined; return x }";
  [%expect {| ok |}]

let%expect_test "typecheck: missing return on a path" =
  run_src "func f(n: i32) i32 { if n > 0 { return 1 } }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:22
        func f(n: i32) i32 { if n > 0 { return 1 } }
                             ^~~~~~~~~~~~~~~~~~~~~ expected i32, found void
    |}]

let%expect_test "typecheck: if and else both return ok" =
  run_src "func f(n: i32) i32 { if n > 0 { return 1 } else { return 0 } }";
  [%expect {| ok |}]

let%expect_test "typecheck: while true diverges, no return needed" =
  run_src "func f() i32 { while true { } }";
  [%expect {| ok |}]

let%expect_test "typecheck: while true with break still needs a return" =
  run_src "func f() i32 { while true { break } }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:16
        func f() i32 { while true { break } }
                       ^~~~~~~~~~~~~~~~~~~~ expected i32, found void
    |}]

let%expect_test "typecheck: inner loop break does not exit outer" =
  run_src "func f() i32 { while true { while true { break } } }";
  [%expect {| ok |}]

let%expect_test "typecheck: break under if still needs a return" =
  run_src "func f() i32 { var c: bool = true\n while true { if c { break } } }";
  [%expect
    {|
    error: type mismatch
      at <test>:2:2
         while true { if c { break } } }
         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expected i32, found void
    |}]

let%expect_test "typecheck: struct literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  let p = pt { x: 3, y: 4 }
  return p.x + p.y
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: empty struct literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  let p = pt { }
  return p.x
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: struct literal unknown field" =
  run_src {|
struct pt { x: i32, y: i32 }
func f() {
  let p = pt { z: 1 }
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:4:7
          let p = pt { z: 1 }
              ^
    help: prefix with an underscore: _p
    error: no field
      at <test>:4:16
          let p = pt { z: 1 }
                       ^
    |}]

let%expect_test "typecheck: struct literal duplicate field" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() {
  let p = pt { x: 1, x: 2 }
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:4:7
          let p = pt { x: 1, x: 2 }
              ^
    help: prefix with an underscore: _p
    error: duplicate field
      at <test>:4:22
          let p = pt { x: 1, x: 2 }
                             ^
    |}]

let%expect_test "typecheck: struct literal wrong field type" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() {
  let p = pt { x: true }
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:4:7
          let p = pt { x: true }
              ^
    help: prefix with an underscore: _p
    error: type mismatch
      at <test>:4:19
          let p = pt { x: true }
                          ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: undefined struct literal" =
  run_src {|
func f() {
  let p = nope { x: 1 }
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:3:7
          let p = nope { x: 1 }
              ^
    help: prefix with an underscore: _p
    error: undefined struct
      at <test>:3:11
          let p = nope { x: 1 }
                  ^~~~
    |}]

let%expect_test "typecheck: let global struct literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
let origin: pt = pt { x: 1, y: 2 }
func f() i32 { return origin.x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global struct literal must be constant" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func g() i32 { return 1 }
let p: pt = pt { x: g(), y: 2 }
|};
  [%expect
    {|
    error: initializer must be constant
      at <test>:4:13
        let p: pt = pt { x: g(), y: 2 }
                    ^~~~~~~~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: duplicate struct field" =
  run_src {|
struct pt { x: i32, x: i64 }
|};
  [%expect
    {|
    error: duplicate field
      at <test>:2:21
        struct pt { x: i32, x: i64 }
                            ^
    |}]

let%expect_test "typecheck: three duplicate struct fields" =
  run_src {|
struct pt { x: i32, x: i64, x: bool }
|};
  [%expect
    {|
    error: duplicate field
      at <test>:2:21
        struct pt { x: i32, x: i64, x: bool }
                            ^
    error: duplicate field
      at <test>:2:29
        struct pt { x: i32, x: i64, x: bool }
                                    ^
    |}]

let%expect_test "typecheck: type alias mismatch across types" =
  run_src
    {|
type myint = i64
func f(x: myint) i32 { return 0 }
func g() { f(true) }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:8
        func f(x: myint) i32 { return 0 }
               ^~~~~~~~
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:4:14
        func g() { f(true) }
                     ^~~~ expected myint, found bool
    |}]

let%expect_test "typecheck: newtype does not mix with its base type" =
  run_src
    {|
newtype Celsius = f32
func f(x: Celsius) i32 { return 0 }
func g() { f(1.0) }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:8
        func f(x: Celsius) i32 { return 0 }
               ^~~~~~~~~~
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:4:14
        func g() { f(1.0) }
                     ^~~ expected Celsius, found f64
    |}]

let%expect_test "typecheck: newtype casts to and from its base type" =
  run_src
    {|
newtype Celsius = f32
func to_f32(c: Celsius) f32 { return c as f32 }
func to_celsius(x: f32) Celsius { return x as Celsius }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: cstr parameter accepts string literal" =
  run_src
    {|
extern "C" func strlen(s: cstr) i64
func f() i64 { return strlen("hi") }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern variadic accepts extra args" =
  run_src
    {|
extern "C" func printf(fmt: cstr, ...) i32
func f() { printf("%d %d", 1, 2) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: pointer equality yields bool" =
  run_src {|
func f(a: *i32, b: *i32) bool { return a == b }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: nested struct field type mismatch" =
  run_src
    {|
struct inner { a: i32 }
struct outer { i: inner }
func f() {
  var o: outer = outer { i: inner { a: 1 } }
  o.i.a = true
}
|};
  [%expect
    {|
    error: type mismatch
      at <test>:6:11
          o.i.a = true
                  ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: struct with array field initializes ok" =
  run_src
    {|
struct buf { data: [4]i32, n: i32 }
func f() i32 {
  var b: buf = buf { data: [1, 2, 3, 4], n: 4 }
  return b.n
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: function returning struct ok" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func origin() pt { return pt { x: 0, y: 0 } }
func f() i32 { return origin().x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: void call result cannot be assigned" =
  run_src {|
func g() { }
func f() { var x: i32 = g() }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:16
        func f() { var x: i32 = g() }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:3:25
        func f() { var x: i32 = g() }
                                ^~~ expected i32, found void
    |}]

let%expect_test "typecheck: struct field whose type is another struct" =
  run_src
    {|
struct b_t { x: i32 }
struct a { b: b_t }
func f() i32 {
  var v: a = a { b: b_t { x: 1 } }
  return v.b.x
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array of structs iterates element type" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  var pts: [2]pt = [pt { x: 1, y: 2 }, pt { x: 3, y: 4 }]
  var s: i32 = 0
  for p in pts { s += p.x }
  return s
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: cast int to float ok" =
  run_src {|
func f() f64 {
  var a: i32 = 3
  return a as f64
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: cast float to int ok" =
  run_src {|
func f() i32 {
  var a: f64 = 3.5
  return a as i32
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size mismatch as argument" =
  run_src
    {|
func take(a: [4]i32) {}
func f() {
  var a: [3]i32 = [1, 2, 3]
  take(a)
}
|};
  [%expect
    {|
    warning: unused variable: a
      at <test>:2:11
        func take(a: [4]i32) {}
                  ^~~~~~~~~
    help: prefix with an underscore: _a
    error: type mismatch
      at <test>:5:8
          take(a)
               ^ expected [4]i32, found [3]i32
    |}]

let%expect_test "typecheck: global var initialized from let" =
  run_src
    {|
let base: i32 = 10
var counter: i32 = base
func f() i32 { return counter }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern variadic requires the fixed args" =
  run_src {|
extern "C" func printf(fmt: cstr, ...) i32
func f() { printf() }
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:3:12
        func f() { printf() }
                   ^~~~~~~~ expected at least 1 argument, found 0
    |}]

let%expect_test "typecheck: continue inside while" =
  run_src "func f() { while true { continue } }";
  [%expect {| ok |}]

let%expect_test "typecheck: bool relational comparison rejected" =
  run_src
    {|
func f() bool {
  var a: bool = true
  var b: bool = false
  return a < b
}
|};
  [%expect
    {|
    error: invalid operand
      at <test>:5:10
          return a < b
                 ^ cannot apply `<` to bool
    |}]

let%expect_test "typecheck: struct equality rejected" =
  run_src
    {|
struct P { x: i32 }
func f() bool {
  var a: P = P { x: 1 }
  var b: P = P { x: 1 }
  return a == b
}
|};
  [%expect
    {|
    error: invalid operand
      at <test>:6:10
          return a == b
                 ^ cannot apply `==` to P
    |}]

let%expect_test "typecheck: void equality rejected" =
  run_src {|
func g() {}
func f() bool {
  return g() == g()
}
|};
  [%expect
    {|
    error: invalid operand
      at <test>:4:10
          return g() == g()
                 ^~~ cannot apply `==` to void
    |}]

let%expect_test "typecheck: float modulo rejected" =
  run_src {|
func f() f64 {
  return 5.0 % 2.0
}
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:10
          return 5.0 % 2.0
                 ^~~ cannot apply `%` to f64
    |}]

let%expect_test "typecheck: bare return in main accepted" =
  run_src {|
func main() i32 {
  return
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: bare return in non-main i32 rejected" =
  run_src {|
func g() i32 {
  return
}
|};
  [%expect
    {|
    error: empty return in non-void function
      at <test>:3:3
          return
          ^~~~~~
    |}]

let%expect_test "typecheck: int literal out of range rejected" =
  run_src {|
func main() i32 {
  var x: u8 = 300
  return 0
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:7
          var x: u8 = 300
              ^
    help: prefix with an underscore: _x
    error: integer literal out of range
      at <test>:3:15
          var x: u8 = 300
                      ^~~ does not fit in u8
    |}]

let%expect_test "typecheck: negative literal into unsigned rejected" =
  run_src {|
func main() i32 {
  var x: u8 = -1
  return 0
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:7
          var x: u8 = -1
              ^
    help: prefix with an underscore: _x
    error: integer literal out of range
      at <test>:3:15
          var x: u8 = -1
                      ^~ does not fit in u8
    |}]

let%expect_test "typecheck: int literal at type bound accepted" =
  run_src
    {|
func main() i32 {
  var x: u8 = 255
  var y: i8 = -128
  return 0
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:7
          var x: u8 = 255
              ^
    help: prefix with an underscore: _x
    warning: unused variable: y
      at <test>:4:7
          var y: i8 = -128
              ^
    help: prefix with an underscore: _y
    ok
    |}]

let%expect_test "typecheck: inferred literal overflowing i32 rejected" =
  run_src {|
func main() i32 {
  var x = 3000000000
  return x
}
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:11
          var x = 3000000000
                  ^~~~~~~~~~ does not fit in i32
    |}]

let%expect_test "typecheck: i64 max accepted" =
  run_src
    {|
func main() i32 {
  var _x: i64 = 9223372036854775807
  return 0
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: u64 max accepted" =
  run_src
    {|
func main() i32 {
  var _x: u64 = 18446744073709551615
  return 0
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: i64 max plus one rejected" =
  run_src
    {|
func main() i32 {
  var _x: i64 = 9223372036854775808
  return 0
}
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:17
          var _x: i64 = 9223372036854775808
                        ^~~~~~~~~~~~~~~~~~~ does not fit in i64
    |}]

let%expect_test "typecheck: literal above u64 max rejected by lexer" =
  run_src
    {|
func main() i32 {
  var _x: u64 = 18446744073709551616
  return 0
}
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:17
          var _x: u64 = 18446744073709551616
                        ^~~~~~~~~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: negative literal into u64 rejected" =
  run_src {|
func main() i32 {
  var _x: u64 = -1
  return 0
}
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:17
          var _x: u64 = -1
                        ^~ does not fit in u64
    |}]

let%expect_test "typecheck: non-i32 main rejected" =
  run_src "func main() f64 { return 0.0 }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:13
        func main() f64 { return 0.0 }
                    ^~~ expected i32, found f64
    |}]

let%expect_test "typecheck: type alias is transparent to its base" =
  run_src
    {|
type Meters = i32
func f() i32 { var d: Meters = 5; return d + 1 }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a struct allows field access" =
  run_src
    {|
struct Point { x: i32, y: i32 }
type Pt = Point
func f() i32 { var p: Pt = Point { x: 1, y: 2 }; return p.x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a function pointer is callable" =
  run_src
    {|
type BinOp = func (i32, i32) i32
func add(a: i32, b: i32) i32 { return a + b }
func f() i32 { var op: BinOp = add; return op(2, 3) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: newtype value satisfies its own parameter" =
  run_src
    {|
newtype Id = i32
func take(x: Id) i32 { return 0 }
func f() i32 { var a: Id = 5 as Id; return take(a) }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:11
        func take(x: Id) i32 { return 0 }
                  ^~~~~
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test
    "typecheck: type alias is interchangeable with its base both ways" =
  run_src
    {|
type Meters = i32
func take_base(x: i32) i32 { return x }
func take_alias(x: Meters) i32 { return x }
func f() i32 { var m: Meters = 5; var b: i32 = 7; return take_base(m) + take_alias(b) }
|};
  [%expect {| ok |}]

let%expect_test
    "typecheck: alias element inside an array is transparent both ways" =
  run_src
    {|
type Meters = i32
func take_base(x: [3]i32) i32 { return x[0] }
func take_alias(x: [3]Meters) i32 { return x[0] }
func f() i32 {
  var m: [3]Meters = [1, 2, 3]
  var b: [3]i32 = [4, 5, 6]
  return take_base(m) + take_alias(b)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: alias of an array coerces to a slice" =
  run_src
    {|
type Row = [3]i32
func take(s: []i32) i32 { return s[0] }
func f() i32 { var r: Row = [1, 2, 3]; return take(r) }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:52
        func f() i32 { var r: Row = [1, 2, 3]; return take(r) }
                                                           ^ expected []i32, found Row
    |}]

let%expect_test "typecheck: aggregate cast sees through an alias element" =
  run_src
    {|
type Meters = i32
func f() i32 { var a: [3]Meters = [1, 2, 3]; var b: [3]i32 = a as [3]i32; return b[1] }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: alias is transparent under a slice and a pointer" =
  run_src
    {|
type Meters = i32
func take_slice(s: []i32) i32 { return s[0] }
func take_ptr(p: *i32) i32 { return *p }
func f() i32 {
  var a: [3]Meters = [1, 2, 3]
  var s: []Meters = a
  var m: Meters = 7
  return take_slice(s) + take_ptr(&m)
}
|};
  [%expect
    {|
    error: type mismatch
      at <test>:7:21
          var s: []Meters = a
                            ^ expected []Meters, found [3]Meters
    |}]

let%expect_test "typecheck: alias and base compare with each other" =
  run_src
    {|
type Meters = i32
func f() bool { var m: Meters = 5; var b: i32 = 5; return m == b }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: alias of a newtype keeps the newtype opaque" =
  run_src
    {|
newtype Id = i32
type Handle = Id
func take(x: i32) i32 { return x }
func f() i32 { var h: Handle = 5 as Id; return take(h) }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:5:53
        func f() i32 { var h: Handle = 5 as Id; return take(h) }
                                                            ^ expected i32, found Handle
    |}]

let%expect_test "typecheck: two newtypes with the same base do not mix" =
  run_src
    {|
newtype Id = i32
newtype Age = i32
func take(x: Id) i32 { return 0 }
func f() i32 { var a: Age = 5 as Age; return take(a) }
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:4:11
        func take(x: Id) i32 { return 0 }
                  ^~~~~
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:5:51
        func f() i32 { var a: Age = 5 as Age; return take(a) }
                                                          ^ expected Id, found Age
    |}]

let%expect_test "typecheck: newtype stays opaque inside an array" =
  run_src
    {|
newtype Id = i32
func take(x: [3]i32) i32 { return x[0] }
func f() i32 { var a: [3]Id; return take(a) }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:42
        func f() i32 { var a: [3]Id; return take(a) }
                                                 ^ expected [3]i32, found [3]Id
    |}]

let%expect_test "typecheck: newtype stays opaque under a pointer" =
  run_src
    {|
newtype Id = i32
func take(p: *i32) i32 { return *p }
func f() i32 { var a: Id = 5 as Id; return take(&a) }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:49
        func f() i32 { var a: Id = 5 as Id; return take(&a) }
                                                        ^~ expected *i32, found *Id
    |}]

let%expect_test "typecheck: newtype of an array does not coerce to a slice" =
  run_src
    {|
newtype Row = [3]i32
func take(s: []i32) i32 { return s[0] }
func f() i32 { var r: Row; return take(r) }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:40
        func f() i32 { var r: Row; return take(r) }
                                               ^ expected []i32, found Row
    |}]

let%expect_test "typecheck: newtype does not compare even with itself" =
  run_src
    {|
newtype Id = i32
func f() bool { var a: Id = 5 as Id; var b: Id = 6 as Id; return a == b }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:66
        func f() bool { var a: Id = 5 as Id; var b: Id = 6 as Id; return a == b }
                                                                         ^ cannot apply `==` to Id
    |}]

let%expect_test "typecheck: newtype does not compare with its base" =
  run_src
    {|
newtype Id = i32
func f() bool { var a: Id = 5 as Id; return a == 5 }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:45
        func f() bool { var a: Id = 5 as Id; return a == 5 }
                                                    ^ cannot apply `==` to Id
    error: type mismatch
      at <test>:3:50
        func f() bool { var a: Id = 5 as Id; return a == 5 }
                                                         ^ expected Id, found i32
    |}]

let%expect_test "typecheck: newtype has no arithmetic without a cast" =
  run_src
    {|
newtype Id = i32
func f() i32 { var a: Id = 5 as Id; var b: Id = a + a; return b as i32 }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:49
        func f() i32 { var a: Id = 5 as Id; var b: Id = a + a; return b as i32 }
                                                        ^ cannot apply `+` to Id
    |}]

let%expect_test "typecheck: newtype of a struct hides its fields" =
  run_src
    {|
struct P { x: i32 }
newtype Q = P
func f() i32 { var q: Q = P { x: 3 } as Q; return q.x }
|};
  [%expect
    {|
    error: type has no fields
      at <test>:4:51
        func f() i32 { var q: Q = P { x: 3 } as Q; return q.x }
                                                          ^~~ on Q
    |}]

let%expect_test "typecheck: newtype has no ordering without a cast" =
  run_src
    {|
newtype Id = i32
func f() bool { var a: Id = 5 as Id; var b: Id = 6 as Id; return a < b }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:66
        func f() bool { var a: Id = 5 as Id; var b: Id = 6 as Id; return a < b }
                                                                         ^ cannot apply `<` to Id
    |}]

let%expect_test "typecheck: newtype of a float has no comparisons" =
  run_src
    {|
newtype Temp = f32
func f() bool { var a: Temp = 1.5 as Temp; var b: Temp = 2.5 as Temp; return a == b }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:78
        func f() bool { var a: Temp = 1.5 as Temp; var b: Temp = 2.5 as Temp; return a == b }
                                                                                     ^ cannot apply `==` to Temp
    |}]

let%expect_test "typecheck: newtype of a float has no ordering" =
  run_src
    {|
newtype Temp = f32
func f() bool { var a: Temp = 1.5 as Temp; var b: Temp = 2.5 as Temp; return a <= b }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:78
        func f() bool { var a: Temp = 1.5 as Temp; var b: Temp = 2.5 as Temp; return a <= b }
                                                                                     ^ cannot apply `<=` to Temp
    |}]

let%expect_test "typecheck: newtype of a bool has no equality" =
  run_src
    {|
newtype Flag = bool
func f() bool { var a: Flag = true as Flag; var b: Flag = false as Flag; return a != b }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:81
        func f() bool { var a: Flag = true as Flag; var b: Flag = false as Flag; return a != b }
                                                                                        ^ cannot apply `!=` to Flag
    |}]

let%expect_test "typecheck: newtype has no compound assignment" =
  run_src {|
newtype Id = i32
func f() { var a: Id = 5 as Id; a += 6 as Id }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:33
        func f() { var a: Id = 5 as Id; a += 6 as Id }
                                        ^ cannot apply `+=` to Id
    |}]

let%expect_test "typecheck: newtype has no unary negation" =
  run_src
    {|
newtype Id = i32
func f() i32 { var a: Id = 5 as Id; return (-a) as i32 }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:46
        func f() i32 { var a: Id = 5 as Id; return (-a) as i32 }
                                                     ^ cannot apply `-` to Id
    |}]

let%expect_test "typecheck: unary plus accepts numeric operands" =
  run_src {|
func f() {
  var _x: i64 = +3000000000
  var _y: f32 = +1.5
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: unary plus rejects bool" =
  run_src "func f() { let _x = +true }";
  [%expect
    {|
    error: invalid operand
      at <test>:1:22
        func f() { let _x = +true }
                             ^~~~ cannot apply `+` to bool
    |}]

let%expect_test "typecheck: newtype has no unary plus" =
  run_src
    {|
newtype Id = i32
func f() i32 { var a: Id = 5 as Id; return (+a) as i32 }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:46
        func f() i32 { var a: Id = 5 as Id; return (+a) as i32 }
                                                     ^ cannot apply `+` to Id
    |}]

let%expect_test "typecheck: suffixed unary plus range includes operator" =
  run_src "func f() { let _x = +128i8 }";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:21
        func f() { let _x = +128i8 }
                            ^~~~~~ does not fit in i8
    |}]

let%expect_test "typecheck: explicit positive literal reports full span" =
  run_src "func f() { let _x: i8 = +128 }";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:25
        func f() { let _x: i8 = +128 }
                                ^~~~ does not fit in i8
    |}]

let%expect_test "typecheck: newtype has no remainder" =
  run_src
    {|
newtype Id = i32
func f() i32 { var a: Id = 5 as Id; return (a % a) as i32 }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:45
        func f() i32 { var a: Id = 5 as Id; return (a % a) as i32 }
                                                    ^ cannot apply `%` to Id
    |}]

let%expect_test "typecheck: newtype compares after casting both sides" =
  run_src
    {|
newtype Id = i32
func f() bool { var a: Id = 5 as Id; var b: Id = 6 as Id; return (a as i32) < (b as i32) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias keeps every comparison of its base" =
  run_src
    {|
type Meters = i32
func f() bool {
  var a: Meters = 5
  var b: Meters = 6
  return a == b || a != b || a < b || a > b || a <= b || a >= b
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias keeps arithmetic and bitwise operators" =
  run_src
    {|
type Meters = i32
func f() i32 {
  var a: Meters = 12
  var b: Meters = 5
  return a + b - a * b / (a % b) + (a & b) + (a | b) + (a ^ b) + (a << 1) + (a >> 1)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a float keeps its operators" =
  run_src
    {|
type Temp = f32
func f() bool {
  var a: Temp = 1.5
  var b: Temp = 2.5
  var c: Temp = a + b - a * b / a
  c += 1.0
  return -c < b && a <= b && a == a && b >= a
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a float still has no remainder" =
  run_src {|
type Temp = f32
func f() f32 { var a: Temp = 5.0; return a % a }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:42
        func f() f32 { var a: Temp = 5.0; return a % a }
                                                 ^ cannot apply `%` to Temp
    |}]

let%expect_test "typecheck: type alias mixes with its base in comparisons" =
  run_src
    {|
type Meters = i32
func f() bool { var a: Meters = 5; var raw: i32 = 6; return a < raw && raw > a }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: alias of a newtype still has no operators" =
  run_src
    {|
newtype Id = i32
type Handle = Id
func f() bool { var a: Handle = 5 as Id; var b: Handle = 6 as Id; return a == b }
|};
  [%expect
    {|
    error: invalid operand
      at <test>:4:74
        func f() bool { var a: Handle = 5 as Id; var b: Handle = 6 as Id; return a == b }
                                                                                 ^ cannot apply `==` to Handle
    |}]

let%expect_test "typecheck: a type alias name collides with a struct" =
  run_src {|
type Foo = i32
struct Foo { x: i32 }
|};
  [%expect
    {|
    error: already defined
      at <test>:3:8
        struct Foo { x: i32 }
               ^~~
      at <test>:2:6
        type Foo = i32
             ^~~ previous definition here
    |}]

let%expect_test "typecheck: a type alias name collides with a newtype" =
  run_src {|
newtype Foo = i32
type Foo = i64
|};
  [%expect
    {|
    error: already defined
      at <test>:3:6
        type Foo = i64
             ^~~
      at <test>:2:9
        newtype Foo = i32
                ^~~ previous definition here
    |}]

let%expect_test "typecheck: a type name shadows a builtin" =
  run_src {|
type i32 = i64
func f(x: i32) i64 { return x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: shadowing a builtin reaches its own definition" =
  run_src {|
type i32 = bool
func f(x: i32) i64 { return x }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:29
        func f(x: i32) i64 { return x }
                                    ^ expected i64, found i32
    |}]

let%expect_test "typecheck: sizeof of a struct type" =
  run_src
    {|
struct S { a: i32, b: i32 }
func f() i64 { return sizeof(S) as i64 }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: sizeof of an array type" =
  run_src "func f() i64 { return sizeof([4]i32) as i64 }";
  [%expect {| ok |}]

let%expect_test "typecheck: a struct field names a struct defined later" =
  run_src {|
struct A { b: *B }
struct B { n: i32 }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a struct points at itself" =
  run_src "struct Node { val: i32, next: *Node }";
  [%expect {| ok |}]

let%expect_test "typecheck: a struct that holds itself by value has no size" =
  run_src "struct Node { n: Node }";
  [%expect
    {|
    error: recursive struct has infinite size
      at <test>:1:8
        struct Node { n: Node }
               ^~~~
    |}]

let%expect_test
    "typecheck: two structs that hold each other by value have no size" =
  run_src {|
struct A { b: B }
struct B { a: A }
|};
  [%expect
    {|
    error: recursive struct has infinite size
      at <test>:2:8
        struct A { b: B }
               ^
    error: recursive struct has infinite size
      at <test>:3:8
        struct B { a: A }
               ^
    |}]

let%expect_test "typecheck: int literal suffix pins the type" =
  run_src "func f() u8 { return 200u8 }";
  [%expect {| ok |}]

let%expect_test "typecheck: int literal suffix that mismatches the target" =
  run_src "func f() { var x: u8 = 5u16 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x: u8 = 5u16 }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:24
        func f() { var x: u8 = 5u16 }
                               ^~~~ expected u8, found u16
    |}]

let%expect_test "typecheck: int literal suffix out of range" =
  run_src "func f() u8 { return 256u8 }";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:22
        func f() u8 { return 256u8 }
                             ^~~~~ does not fit in u8
    |}]

let%expect_test "typecheck: negative unsigned suffix" =
  run_src "func f() i8 { return -1u8 }";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:22
        func f() i8 { return -1u8 }
                             ^~~~ does not fit in u8
    error: type mismatch
      at <test>:1:22
        func f() i8 { return -1u8 }
                             ^~~~ expected i8, found u8
    |}]

let%expect_test "typecheck: assignment in condition is not a value" =
  run_src "func f() { var b: bool = false; if b = true { } }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:36
        func f() { var b: bool = false; if b = true { } }
                                           ^~~~~~~~ expected bool, found void
    help: did you mean `==` to compare?
    |}]

let%expect_test "typecheck: chained assignment is not a value" =
  run_src "func f() { var a: i32 = 0; var b: i32 = 0; a = b = 5 }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:48
        func f() { var a: i32 = 0; var b: i32 = 0; a = b = 5 }
                                                       ^~~~~ expected i32, found void
    |}]

let%expect_test "typecheck: assign to for loop variable" =
  run_src "func f() { for i in 0..3 { i = 99 } }";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:28
        func f() { for i in 0..3 { i = 99 } }
                                   ^
    |}]

let%expect_test "typecheck: never rejected as a var type" =
  run_src "func f() { var x: never = 0 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x: never = 0 }
                       ^
    help: prefix with an underscore: _x
    error: never is only valid as a function return type
      at <test>:1:19
        func f() { var x: never = 0 }
                          ^~~~~
    help: a value of type never cannot exist
    |}]

let%expect_test "typecheck: never rejected as a param type" =
  run_src "func f(x: never) {}";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:8
        func f(x: never) {}
               ^~~~~~~~
    help: prefix with an underscore: _x
    error: never is only valid as a function return type
      at <test>:1:11
        func f(x: never) {}
                  ^~~~~
    help: a value of type never cannot exist
    |}]

let%expect_test "typecheck: never rejected as a pointee type" =
  run_src "func f() { var p: *never = null }";
  [%expect
    {|
    warning: unused variable: p
      at <test>:1:16
        func f() { var p: *never = null }
                       ^
    help: prefix with an underscore: _p
    error: never is only valid as a function return type
      at <test>:1:20
        func f() { var p: *never = null }
                           ^~~~~
    help: a value of type never cannot exist
    |}]

let%expect_test "typecheck: never rejected as a field type" =
  run_src "struct S { x: never }";
  [%expect
    {|
    error: never is only valid as a function return type
      at <test>:1:15
        struct S { x: never }
                      ^~~~~
    help: a value of type never cannot exist
    |}]

let%expect_test "typecheck: return in a never function is rejected" =
  run_src "func spin() never { return }";
  [%expect
    {|
    error: a never function cannot return
      at <test>:1:21
        func spin() never { return }
                            ^~~~~~
    |}]

let%expect_test "typecheck: return value in a never function is rejected" =
  run_src "func spin() never { return 5 }";
  [%expect
    {|
    error: a never function cannot return
      at <test>:1:21
        func spin() never { return 5 }
                            ^~~~~~~~
    |}]

let%expect_test "typecheck: a never call satisfies the missing return check" =
  run_src {|
extern "C" func exit(code: i32) never
func f() i32 { exit(1) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a never call coerces to the return type" =
  run_src
    {|
extern "C" func exit(code: i32) never
func f() i32 { return exit(1) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: function pointer may return never" =
  run_src
    {|
extern "C" func exit(code: i32) never
func f() { let stop: extern "C" func (i32) never = exit
 stop(1) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a typed pointer flows into *opaque" =
  run_src "func f(p: *i32) *opaque { return p }";
  [%expect {| ok |}]

let%expect_test "typecheck: cstr flows into *opaque" =
  run_src "func f(s: cstr) *opaque { return s }";
  [%expect {| ok |}]

let%expect_test "typecheck: null flows into *opaque" =
  run_src "func f() *opaque { return null }";
  [%expect {| ok |}]

let%expect_test "typecheck: *opaque needs a cast back to a typed pointer" =
  run_src "func f(a: *opaque) *i32 { return a }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        func f(a: *opaque) *i32 { return a }
                                         ^ expected *i32, found *opaque
    |}]

let%expect_test "typecheck: *opaque casts back to a typed pointer" =
  run_src "func f(a: *opaque) *i32 { return a as *i32 }";
  [%expect {| ok |}]

let%expect_test "typecheck: cannot dereference *opaque" =
  run_src "func f(a: *opaque) i32 { return *a }";
  [%expect
    {|
    error: cannot dereference *opaque
      at <test>:1:34
        func f(a: *opaque) i32 { return *a }
                                         ^
    help: cast to a typed pointer first
    |}]

let%expect_test "typecheck: cannot index *opaque" =
  run_src "func f(a: *opaque) i32 { return a[0] }";
  [%expect
    {|
    error: cannot index *opaque
      at <test>:1:33
        func f(a: *opaque) i32 { return a[0] }
                                        ^~~~
    help: cast to a typed pointer first
    |}]

let%expect_test "typecheck: cannot access a field of *opaque" =
  run_src "func f(a: *opaque) i32 { return a.x }";
  [%expect
    {|
    error: cannot access a field of *opaque
      at <test>:1:33
        func f(a: *opaque) i32 { return a.x }
                                        ^~~
    help: cast to a typed pointer first
    |}]

let%expect_test "typecheck: no arithmetic on *opaque" =
  run_src "func f(a: *opaque) *opaque { return a + 1 }";
  [%expect
    {|
    error: invalid operand
      at <test>:1:37
        func f(a: *opaque) *opaque { return a + 1 }
                                            ^ cannot apply `+` to *opaque
    error: type mismatch
      at <test>:1:41
        func f(a: *opaque) *opaque { return a + 1 }
                                                ^ expected *opaque, found i32
    |}]

let%expect_test "typecheck: *opaque compares to null" =
  run_src "func f(a: *opaque) bool { return a == null }";
  [%expect {| ok |}]

let%expect_test "typecheck: two *opaque values compare" =
  run_src "func f(a: *opaque, b: *opaque) bool { return a != b }";
  [%expect {| ok |}]

let%expect_test "typecheck: bare opaque as a param is rejected" =
  run_src "func f(x: opaque) { }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:8
        func f(x: opaque) { }
               ^~~~~~~~~
    help: prefix with an underscore: _x
    error: opaque is only valid as a pointee
      at <test>:1:11
        func f(x: opaque) { }
                  ^~~~~~
    help: use *opaque for an untyped pointer
    |}]

let%expect_test "typecheck: bare opaque as a var is rejected" =
  run_src "func f() { var x: opaque }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x: opaque }
                       ^
    help: prefix with an underscore: _x
    error: opaque is only valid as a pointee
      at <test>:1:19
        func f() { var x: opaque }
                          ^~~~~~
    help: use *opaque for an untyped pointer
    |}]

let%expect_test "typecheck: if-expr never arm bends to the other arm" =
  run_src
    {|
extern "C" func exit(c: i32) never
func f() i32 { let y = if true { 10 } else { exit(1) }; return y }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: if-expr arm type is order independent" =
  run_src
    "func f() i32 { var x: i64 = 5\n\
    \ let y = if true { x } else { 10 }; return y as i32 }";
  [%expect {| ok |}]

let%expect_test "typecheck: all-never if-expr binds as never" =
  run_src
    {|
extern "C" func exit(c: i32) never
func f() i32 { let _y = if true { exit(3) } else { exit(4) }; return 0 }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: nested if-expr never arm bends to the other arm" =
  run_src
    {|
extern "C" func exit(c: i32) never
func f() i32 { let y = if true { if false { 10 } else { exit(1) } } else
 { 20 }; return y }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: nested concrete arm anchors the outer if-expr" =
  run_src
    "func f() i32 { var x: i64 = 7\n\
    \ let y = if true { if false { x } else { 5 } } else { 10 }; return y as \
     i32 }";
  [%expect {| ok |}]

(* Expression oriented collapse edge cases *)

let%expect_test "collapse: trailing if is an implicit return" =
  run_src "func abs(x: i32) i32 { if x < 0 { -x } else { x } }";
  [%expect {| ok |}]

let%expect_test "collapse: trailing block is an implicit return" =
  run_src "func f(x: i32) i32 { { let a = x * 2\n a + 1 } }";
  [%expect {| ok |}]

let%expect_test "collapse: nested block tail flows to the return" =
  run_src "func f() i32 { { let a: i32 = 1\n { a + 2 } } }";
  [%expect {| ok |}]

let%expect_test "collapse: deeply nested implicit return" =
  run_src
    "func f(x: i32) i32 { if x > 0 { { if x > 10 { 1 } else { 2 } } } else { 3 \
     } }";
  [%expect {| ok |}]

let%expect_test "collapse: a binding is not a value operand" =
  run_src "func f() i32 { let x: i32 = let y: i32 = 5\n return x }";
  [%expect
    {|
    error: expected expression
      at <test>:1:29
        func f() i32 { let x: i32 = let y: i32 = 5
                                    ^~~ found `let`
    |}]

let%expect_test "collapse: a block ending in a binding is void" =
  run_src "func f() i32 { let x: i32 = { var a: i32 = 1 }\n return x }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:31
        func f() i32 { let x: i32 = { var a: i32 = 1 }
                                      ^~~~~~~~~~~~~~ expected i32, found void
    warning: unused variable: a
      at <test>:1:35
        func f() i32 { let x: i32 = { var a: i32 = 1 }
                                          ^
    help: prefix with an underscore: _a
    |}]

let%expect_test "collapse: value if arms must agree" =
  run_src
    "func f(c: bool) i32 { let x: i32 = if c { 1 } else { true }\n return x }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:54
        func f(c: bool) i32 { let x: i32 = if c { 1 } else { true }
                                                             ^~~~ expected i32, found bool
    |}]

let%expect_test "collapse: a never arm coerces to the live arm" =
  run_src
    "func f(c: bool) i32 { let x: i32 = if c { 1 } else { return 0 }\n\
    \ return x }";
  [%expect {| ok |}]

let%expect_test "collapse: value if without else is void" =
  run_src "func f(c: bool) i32 { let x: i32 = if c { 1 }\n return x }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:36
        func f(c: bool) i32 { let x: i32 = if c { 1 }
                                           ^~~~~~~~~~ expected i32, found void
    |}]

let%expect_test "collapse: while true with no break is never" =
  run_src "func f() i32 { while true { } }";
  [%expect {| ok |}]

let%expect_test "collapse: never function may loop forever" =
  run_src "func spin() never { while true { } }";
  [%expect {| ok |}]

let%expect_test "collapse: break in a value arm inside a loop" =
  run_src
    "func f() i32 { while true { let x: i32 = if false { 1 } else { break }\n\
    \ return x }; return 0 }";
  [%expect {| ok |}]

let%expect_test "collapse: continue as a value runs the step" =
  run_src
    "func f() i32 { var i: i32 = 0\n\
    \ while i < 3 { let x: i32 = if i == 2 { i } else { i = i + 1\n\
    \ continue }\n\
    \ return x }; return 9 }";
  [%expect {| ok |}]

let%expect_test "collapse: discarded arithmetic warns" =
  run_src "func f() i32 { 1 + 2\n return 5 }";
  [%expect
    {|
    warning: discarded operation result
      at <test>:1:16
        func f() i32 { 1 + 2
                       ^~~~~
    help: use `let _ = ...` when this is intentional
    ok |}]

let%expect_test "collapse: discarded call stays quiet" =
  run_src {|
extern "C" func run() i32
func f() { run() }
|};
  [%expect {| ok |}]

let%expect_test "collapse: discarded tail arithmetic warns" =
  run_src "func f() { 1 + 2 }";
  [%expect
    {|
    warning: discarded operation result
      at <test>:1:12
        func f() { 1 + 2 }
                   ^~~~~
    help: use `let _ = ...` when this is intentional
    ok |}]

let%expect_test "collapse: explicit discard stays quiet" =
  run_src "func f() { let _ = 1 + 2 }";
  [%expect {| ok |}]

let%expect_test "collapse: implicit return of a wrong tail type" =
  run_src "func f() i32 { true }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:16
        func f() i32 { true }
                       ^~~~ expected i32, found bool
    |}]

let%expect_test "collapse: return if with diverging arms" =
  run_src "func f(c: bool) i32 { return if c { return 1 } else { return 2 } }";
  [%expect {| ok |}]

let%expect_test "collapse: break as a value outside a loop still errors" =
  run_src
    "func f() i32 { let x: i32 = if true { 1 } else { break }\n return x }";
  [%expect
    {|
    error: `break` outside a loop
      at <test>:1:50
        func f() i32 { let x: i32 = if true { 1 } else { break }
                                                         ^~~~~
    |}]

let%expect_test "collapse: nested value block anchors its type" =
  run_src
    "func f() i64 { let x: i64 = { let a: i64 = 3\n { a + 1 } }\n return x }";
  [%expect {| ok |}]

let%expect_test "typecheck: char is distinct from i32" =
  run_src "func f() { var x: i32 = 'A' }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:16
        func f() { var x: i32 = 'A' }
                       ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:25
        func f() { var x: i32 = 'A' }
                                ^~~ expected i32, found char
    |}]

let%expect_test "typecheck: no arithmetic on a char" =
  run_src "func f() i32 { return ('A' + 1) as i32 }";
  [%expect
    {|
    error: invalid operand
      at <test>:1:24
        func f() i32 { return ('A' + 1) as i32 }
                               ^~~ cannot apply `+` to char
    error: type mismatch
      at <test>:1:30
        func f() i32 { return ('A' + 1) as i32 }
                                     ^ expected char, found i32
    |}]

let%expect_test "typecheck: char casts to and from an integer" =
  run_src "func f() i32 { var c: char = 65 as char; return c as i32 }";
  [%expect {| ok |}]

let%expect_test "typecheck: chars compare for equality and order" =
  run_src "func f() bool { return 'A' == 'B' && 'A' < 'B' }";
  [%expect {| ok |}]

let%expect_test "typecheck: char does not cast to a float" =
  run_src "func f() f32 { return 'A' as f32 }";
  [%expect
    {|
    error: invalid cast
      at <test>:1:23
        func f() f32 { return 'A' as f32 }
                              ^~~~~~~~~~ cannot cast char to f32
    |}]

let%expect_test "typecheck: binding a void call is rejected" =
  run_src "func foo() { }; func f() { var x = foo() }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:32
        func foo() { }; func f() { var x = foo() }
                                       ^
    help: prefix with an underscore: _x
    error: cannot bind void value
      at <test>:1:36
        func foo() { }; func f() { var x = foo() }
                                           ^~~~~
    |}]

let%expect_test "typecheck: pair assignment checks each value" =
  run_src "func f(a: i32, b: bool) { a, b = b, a }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        func f(a: i32, b: bool) { a, b = b, a }
                                         ^ expected i32, found bool
    error: type mismatch
      at <test>:1:37
        func f(a: i32, b: bool) { a, b = b, a }
                                            ^ expected bool, found i32
    |}]

let%expect_test "typecheck: pair assignment checks each target" =
  run_src "func f(a: i32, b: i32) { let x = 1; x, b = b, a }";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:37
        func f(a: i32, b: i32) { let x = 1; x, b = b, a }
                                            ^
    |}]

let%expect_test "typecheck: pair assignment rejects an expression target" =
  run_src "func f(a: i32, b: i32) { (a + 1), b = b, a }";
  [%expect
    {|
    error: cannot assign to expression
      at <test>:1:27
        func f(a: i32, b: i32) { (a + 1), b = b, a }
                                  ^~~~~
    |}]

let%expect_test "typecheck: pair assignment allows different target types" =
  run_src "func f(a: i32, b: bool) { a, b = 1, true }";
  [%expect {| ok |}]

let%expect_test "typecheck: newline operator continues into void call" =
  run_src {|func g() {}
func f() i32 {
  return 1 +
    g()
}|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:5
            g()
            ^~~ expected i32, found void
    |}]

let%expect_test "typecheck: newline operator continues into integer call" =
  run_src {|func g() i32 { return 2 }
func f() i32 {
  return 1 +
    g()
}|};
  [%expect {| ok |}]

let%expect_test "typecheck: a parameter names a type declared later" =
  run_src {|
func take(value: Meters) i32 { return value }
type Meters = i32
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a global names a type declared later" =
  run_src {|
let width: Meters = 3
type Meters = i32
|};
  [%expect {| ok |}]

let%expect_test "typecheck: an alias names itself" =
  run_src {|
type Loop = Loop
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:6
        type Loop = Loop
             ^~~~
    |}]

let%expect_test "typecheck: two aliases name each other" =
  run_src {|
type First = Second
type Second = First
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:6
        type First = Second
             ^~~~~
    |}]

let%expect_test "typecheck: an alias names itself through a pointer" =
  run_src {|
type Loop = *Loop
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:6
        type Loop = *Loop
             ^~~~
    |}]

let%expect_test "typecheck: a newtype names itself" =
  run_src {|
newtype Loop = Loop
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:9
        newtype Loop = Loop
                ^~~~
    |}]

let%expect_test "typecheck: an alias chain resolves in either order" =
  run_src
    {|
type Feet = Meters
type Meters = i32
func take(value: Feet) i32 { return value }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: inferred storage rejects never and void" =
  run_src
    {|
extern "C" func stop() never
func noop() {}
func f() {
  var never_array = [stop(), stop()]
  var void_array = [noop(), noop()]
}
|};
  [%expect
    {|
    warning: unused variable: never_array
      at <test>:5:7
          var never_array = [stop(), stop()]
              ^~~~~~~~~~~
    help: prefix with an underscore: _never_array
    error: array element cannot have type never
      at <test>:5:22
          var never_array = [stop(), stop()]
                             ^~~~~~
    warning: unused variable: void_array
      at <test>:6:7
          var void_array = [noop(), noop()]
              ^~~~~~~~~~
    help: prefix with an underscore: _void_array
    error: array element cannot have type void
      at <test>:6:21
          var void_array = [noop(), noop()]
                            ^~~~~~
    |}]

let%expect_test "typecheck: a qualified struct literal" =
  run_program
    [
      ("main.rp", {|
import math
func main() { let _p = math.Point { x: 1 } }
|});
      ("math.rp", {|
pub struct Point { x: i32 }
|});
    ];
  [%expect {| ok |}]

let%expect_test "typecheck: a module needs a member" =
  run_program
    [
      ("main.rp", {|
import math
func main() { let _value = math }
|});
      ("math.rp", {|
pub func add(_x: i32) {}
|});
    ];
  [%expect {| module requires a member |}]

let%expect_test "typecheck: modules can repeat a type spelling" =
  run_program
    [
      ("main.rp", {|
import math
type Pair = i32
func main() { math.check() }
|});
      ("math.rp", {|
type Pair = bool
pub func check() { var v: Pair = 1 }
|});
    ];
  [%expect {|
    unused variable: v
    type mismatch
    |}]

let%expect_test "typecheck: singular argument count" =
  run_src {|func take(_value: i32) {}
func main() { take() }
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:2:15
        func main() { take() }
                      ^~~~~~ expected 1 argument, found 0
    |}]

let%expect_test "typecheck: nonliteral operand types binary expression" =
  run_src
    {|
func add_two(x: i64) i64 {
  let left = 1 + x
  let right = x + 1
  return left + right
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a local type may be used before its declaration" =
  run_src {|func f() i32 {
  let x: Coord = 4
  type Coord = i32
  x
}|};
  [%expect {| ok |}]

let%expect_test "typecheck: local aliases may repeat in separate blocks" =
  run_src
    {|func f() {
  { type Value = i32; let _x: Value = 1 }
  { type Value = bool; let _x: Value = true }
}|};
  [%expect {| ok |}]

let%expect_test "typecheck: a local newtype keeps its identity" =
  run_src
    {|func f() {
  newtype Id = i32
  let base: i32 = 1
  let _id: Id = base
}|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:17
          let _id: Id = base
                        ^~~~ expected Id, found i32
    |}]

let%expect_test "typecheck: an unreachable local declaration warns" =
  run_src {|func f() i32 {
  return 1
  func unused() i32 { 0 }
}|};
  [%expect
    {|
    warning: unreachable code
      at <test>:3:3
          func unused() i32 { 0 }
          ^~~~~~~~~~~~~~~~~~~~~~~
    ok
    |}]

let%expect_test "typecheck: write to an array parameter" =
  run_src "func f(a: [3]i32) { a[0] = 9 }";
  [%expect
    {|
    error: cannot assign to a by value parameter
      at <test>:1:21
        func f(a: [3]i32) { a[0] = 9 }
                            ^~~~ the caller keeps its own copy
    help: take a pointer to write through it: a: *[3]i32
    |}]

let%expect_test "typecheck: write to a struct parameter field" =
  run_src "struct P { x: i32 }\nfunc f(p: P) { p.x = 9 }";
  [%expect
    {|
    error: cannot assign to a by value parameter
      at <test>:2:16
        func f(p: P) { p.x = 9 }
                       ^~~ the caller keeps its own copy
    help: take a pointer to write through it: p: *P
    |}]

let%expect_test "typecheck: write to a whole aggregate parameter" =
  run_src "func f(a: [3]i32) { a = [4, 5, 6] }";
  [%expect
    {|
    error: cannot assign to a by value parameter
      at <test>:1:21
        func f(a: [3]i32) { a = [4, 5, 6] }
                            ^ the caller keeps its own copy
    help: take a pointer to write through it: a: *[3]i32
    |}]

let%expect_test "typecheck: write through a slice parameter" =
  run_src "func f(s: []i32) { s[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: write through a pointer parameter" =
  run_src "struct P { x: i32 }\nfunc f(p: *P) { p.x = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: write to a copy of an array parameter" =
  run_src "func f(a: [3]i32) { var local: [3]i32 = a; local[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: discarded if arms need not agree" =
  run_src
    {|
extern "C" func printf(fmt: *i8, ...) i32
func f() { if true { printf("x") } else {} }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: if arms still agree where the value is used" =
  run_src
    {|
extern "C" func printf(fmt: *i8, ...) i32
func f() i32 { let x = if true { printf("x") } else {}; return x }
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:53
        func f() i32 { let x = if true { printf("x") } else {}; return x }
                                                            ^~ expected i32, found void
    |}]

let%expect_test "typecheck: int is i64" =
  run_src "func f() i64 { let a: int = 1\n  return a }";
  [%expect {| ok |}]

let%expect_test "typecheck: float is f64" =
  run_src "func f() f64 { let a: float = 1.5\n  return a }";
  [%expect {| ok |}]

let%expect_test "typecheck: int is not i32" =
  run_src "func f() i32 { let a: int = 1\n  return a }";
  [%expect
    {|
    error: type mismatch
      at <test>:2:10
          return a }
                 ^ expected i32, found i64
    |}]

let%expect_test "typecheck: float is not f32" =
  run_src "func f() f32 { let a: float = 1.5\n  return a }";
  [%expect
    {|
    error: type mismatch
      at <test>:2:10
          return a }
                 ^ expected f32, found f64
    |}]

let%expect_test "typecheck: literal too big for int" =
  run_src "func f() { let _a: int = 9223372036854775808 }";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:26
        func f() { let _a: int = 9223372036854775808 }
                                 ^~~~~~~~~~~~~~~~~~~ does not fit in i64
    |}]

let%expect_test "typecheck: int shadowed by an alias" =
  run_src "type int = i32\nfunc f() i32 { let a: int = 1\n  return a }";
  [%expect {| ok |}]

let%expect_test "typecheck: str literal and len" =
  run_src "func f() usize { let s: str = \"hello\"\n  return s.len }";
  [%expect {| ok |}]

let%expect_test "typecheck: str is not cstr" =
  run_src "func f() { let s: str = \"a\"\n  let _c: cstr = s }";
  [%expect
    {|
    error: type mismatch
      at <test>:2:18
          let _c: cstr = s }
                         ^ expected cstr, found str
    |}]

let%expect_test "typecheck: cstr is not str" =
  run_src "func f() { let s: cstr = \"a\"\n  let _t: str = s }";
  [%expect
    {|
    error: type mismatch
      at <test>:2:17
          let _t: str = s }
                        ^ expected str, found cstr
    |}]

let%expect_test "typecheck: a bare literal is still cstr" =
  run_src "func f() { let _s = \"a\" }";
  [%expect {| ok |}]

let%expect_test "typecheck: str has no ptr field" =
  run_src "func f() { let s: str = \"a\"\n  let _p = s.ptr }";
  [%expect
    {|
    error: no field
      at <test>:2:14
          let _p = s.ptr }
                     ^~~ on str
    |}]

let%expect_test "typecheck: str cannot be indexed" =
  run_src "func f() { let s: str = \"a\"\n  let _b = s[0] }";
  [%expect
    {|
    error: cannot index
      at <test>:2:12
          let _b = s[0] }
                   ^~~~ on str
    |}]

let%expect_test "typecheck: str cannot be compared" =
  run_src "func f() bool { let s: str = \"a\"\n  return s == \"a\" }";
  [%expect
    {|
    error: invalid operand
      at <test>:2:10
          return s == "a" }
                 ^ cannot apply `==` to str
    |}]

let%expect_test "typecheck: a str global is constant" =
  run_src "var g: str = \"a\"";
  [%expect {| ok |}]

let%expect_test "typecheck: a str comptime is rejected" =
  run_src "comptime C: str = \"a\"";
  [%expect
    {|
    error: comptime must be a scalar
      at <test>:1:1
        comptime C: str = "a"
        ^~~~~~~~~~~~~~~~~~~~~ on str
    help: use let for values that need storage
    |}]

let%expect_test "typecheck: a labeled break exits an outer loop" =
  run_src
    {|func f() {
  outer: while true { while true { break :outer } }
  g()
}
func g() {}|};
  [%expect {| ok |}]

let%expect_test "typecheck: a shadowed label leaves the outer loop diverging" =
  run_src
    {|func f() {
  outer: while true { outer: while true { break :outer } }
  g()
}
func g() {}|};
  [%expect
    {|
    warning: unreachable code
      at <test>:3:3
          g()
          ^~~
    ok
    |}]

let%expect_test "typecheck: break inside loop" =
  run_src "func f() { loop { break } }";
  [%expect {| ok |}]

let%expect_test "typecheck: a loop with no break diverges" =
  run_src {|func f() {
  loop {}
  g()
}
func g() {}|};
  [%expect
    {|
    warning: unreachable code
      at <test>:3:3
          g()
          ^~~
    ok
    |}]

let%expect_test "typecheck: a loop takes its value from break" =
  run_src {|func f() i32 {
  return loop { break 42 }
}|};
  [%expect {| ok |}]

let%expect_test "typecheck: break with a value needs a loop" =
  run_src "func f() { while true { break 5 } }";
  [%expect
    {|
    error: `break` with a value outside a `loop`
      at <test>:1:31
        func f() { while true { break 5 } }
                                      ^
    help: use `loop` when the loop produces a value
    |}]

let%expect_test "typecheck: every valued break has to agree" =
  run_src
    {|func f() i32 {
  return loop {
    if true { break 1 }
    break true
  }
}|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:11
            break true
                  ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: a bare break after a valued one is rejected" =
  run_src
    {|func f() i32 {
  return loop {
    if true { break 1 }
    break
  }
}|};
  [%expect
    {|
    error: `break` values disagree
      at <test>:4:5
            break
            ^~~~~ no value here
      at <test>:3:21
            if true { break 1 }
                            ^ breaks with i32
    |}]

let%expect_test "typecheck: a valued break after a bare one is rejected" =
  run_src
    {|func f() i32 {
  return loop {
    if true { break }
    break 1
  }
}|};
  [%expect
    {|
    error: `break` values disagree
      at <test>:4:11
            break 1
                  ^ breaks with i32
      at <test>:3:15
            if true { break }
                      ^~~~~ no value here
    |}]
