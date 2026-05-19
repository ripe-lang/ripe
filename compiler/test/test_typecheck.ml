(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

let%expect_test "typecheck: break outside loop" =
  run_src "func f() { break }";
  [%expect {| TypeError: <test>:1:12: break outside loop |}]

let%expect_test "typecheck: continue outside loop" =
  run_src "func f() { continue }";
  [%expect {| TypeError: <test>:1:12: continue outside loop |}]

let%expect_test "typecheck: unbound variable" =
  run_src "func f() { x }";
  [%expect {| TypeError: <test>:1:12: undefined variable 'x' |}]

let%expect_test "typecheck: type mismatch in let" =
  run_src "func f() { const x: bool = 42 }";
  [%expect
    {|
    <test>:1:28: warning: 'x' declared but never used
    TypeError: <test>:1:28: expected bool but found i32
    |}]

let%expect_test "typecheck: wrong number of arguments" =
  run_src {|
func g() {}
func f() { g(1) }
|};
  [%expect {| TypeError: <test>:3:12: expected 0 arguments but got 1 |}]

let%expect_test "typecheck: null assigned to non-pointer" =
  run_src "func f() { const x: i32 = null }";
  [%expect
    {|
    <test>:1:27: warning: 'x' declared but never used
    TypeError: <test>:1:27: expected i32 but found null
    |}]

let%expect_test "typecheck: identity function" =
  run_src "func id(a: i32) i32 { return a }";
  [%expect {| ok |}]

let%expect_test "typecheck: null assigned to pointer" =
  run_src "func f() { const p: *i32 = null }";
  [%expect
    {|
    <test>:1:28: warning: 'p' declared but never used
    ok
    |}]

let%expect_test "typecheck: break inside while" =
  run_src "func f() { while true { break } }";
  [%expect {| ok |}]

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
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr assign and call" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() {
  var op: (i32, i32) i32 = add
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
  var op: (i32) i32 = add
}
|};
  [%expect
    {|
    <test>:4:23: warning: 'op' declared but never used
    TypeError: <test>:4:23: expected (i32) i32 but found (i32, i32) i32
    |}]

let%expect_test "typecheck: non-callable variable" =
  run_src {|
func f() {
  var x: i32 = 5
  x(1)
}
|};
  [%expect {| TypeError: <test>:4:3: 'x' is not callable |}]

let%expect_test "typecheck: fn ptr as parameter" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func apply(f: (i32, i32) i32, a: i32, b: i32) i32 { return f(a, b) }
func g() { apply(add, 1, 2) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr wrong arity at call" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() {
  var op: (i32, i32) i32 = add
  op(1)
}
|};
  [%expect {| TypeError: <test>:5:3: expected 2 arguments but got 1 |}]

let%expect_test "typecheck: fn ptr forward reference" =
  run_src
    {|
func f() {
  var op: (i32, i32) i32 = add
  op(1, 2)
}
func add(a: i32, b: i32) i32 { return a + b }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr returning fn ptr" =
  run_src
    {|
func add(a: i32, b: i32) i32 { return a + b }
func get_op() (i32, i32) i32 { return add }
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
  var p: () = noop
  p()
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global const read from function" =
  run_src {|
const X: i32 = 42
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
const X: i32 = 7
|};
  [%expect {| ok |}]

let%expect_test "typecheck: assign to const global" =
  run_src {|
const X: i32 = 1
func f() { X = 2 }
|};
  [%expect {| TypeError: <test>:3:12: cannot assign to const 'X' |}]

let%expect_test "typecheck: non-const global initializer" =
  run_src {|
func g() i32 { return 1 }
const X: i32 = g()
|};
  [%expect
    {| TypeError: <test>:3:16: initializer for 'X' must be a constant expression |}]

let%expect_test "typecheck: const requires initializer" =
  run_src "const X: i32";
  [%expect
    {| TypeError: <test>:1:1: 'X' is const and must have an initializer |}]

let%expect_test "typecheck: int arithmetic ok" =
  run_src "func f() i32 { return 1 + 2 * 3 - 4 }";
  [%expect {| ok |}]

let%expect_test "typecheck: mixed int widths" =
  run_src
    {|
func f() {
  const a: i32 = 1
  const b: i64 = 2
  const c = a + b
}
|};
  [%expect
    {|
    <test>:5:17: warning: 'c' declared but never used
    TypeError: <test>:5:17: expected i32 but found i64
    |}]

let%expect_test "typecheck: bool arithmetic rejected" =
  run_src "func f() { const x = true + false }";
  [%expect
    {|
    <test>:1:29: warning: 'x' declared but never used
    TypeError: <test>:1:22: cannot apply '+' to type 'bool'
    |}]

let%expect_test "typecheck: comparison yields bool" =
  run_src "func f() bool { return 1 < 2 }";
  [%expect {| ok |}]

let%expect_test "typecheck: logical and/or require bool" =
  run_src "func f() { const x = 1 && 2 }";
  [%expect
    {|
    <test>:1:27: warning: 'x' declared but never used
    TypeError: <test>:1:22: expected bool but found i32
    TypeError: <test>:1:27: expected bool but found i32
    |}]

let%expect_test "typecheck: not on non-bool" =
  run_src "func f() { const x = !1 }";
  [%expect
    {|
    <test>:1:23: warning: 'x' declared but never used
    TypeError: <test>:1:23: expected bool but found i32
    |}]

let%expect_test "typecheck: shift on int ok" =
  run_src "func f() i32 { return 1 << 3 }";
  [%expect {| ok |}]

let%expect_test "typecheck: bitwise on bool rejected" =
  run_src "func f() { const x = true & false }";
  [%expect
    {|
    <test>:1:29: warning: 'x' declared but never used
    TypeError: <test>:1:22: cannot apply '&' to type 'bool'
    |}]

let%expect_test "typecheck: int to int cast" =
  run_src "func f() i64 { return 1 as i64 }";
  [%expect {| ok |}]

let%expect_test "typecheck: sizeof has int type" =
  run_src "func f() i64 { return sizeof(i32) as i64 }";
  [%expect {| ok |}]

let%expect_test "typecheck: cast bool to ptr rejected" =
  run_src "func f() { const p: *i32 = true as *i32 }";
  [%expect
    {|
    <test>:1:28: warning: 'p' declared but never used
    ok
    |}]

let%expect_test "typecheck: missing return value" =
  run_src "func f() i32 { return }";
  [%expect {| TypeError: <test>:1:16: empty return in non-void function |}]

let%expect_test "typecheck: return value in void fn" =
  run_src "func f() { return 1 }";
  [%expect {| TypeError: <test>:1:19: expected void but found i32 |}]

let%expect_test "typecheck: return type mismatch" =
  run_src "func f() i32 { return true }";
  [%expect {| TypeError: <test>:1:23: expected i32 but found bool |}]

let%expect_test "typecheck: if condition must be bool" =
  run_src "func f() { if 1 {} }";
  [%expect {| TypeError: <test>:1:15: expected bool but found i32 |}]

let%expect_test "typecheck: while condition must be bool" =
  run_src "func f() { while 1 {} }";
  [%expect {| TypeError: <test>:1:18: expected bool but found i32 |}]

let%expect_test "typecheck: if/else ok" =
  run_src "func f() i32 { if true { return 1 } else { return 2 } }";
  [%expect {| ok |}]

let%expect_test "typecheck: nested loops break ok" =
  run_src "func f() { while true { while true { break } } }";
  [%expect {| ok |}]

let%expect_test "typecheck: assign to const local" =
  run_src {|
func f() {
  const x: i32 = 1
  x = 2
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: redeclare local" =
  run_src {|
func f() {
  const x: i32 = 1
  const x: i32 = 2
}
|};
  [%expect
    {|
    <test>:4:18: warning: 'x' declared but never used
    <test>:3:18: warning: 'x' declared but never used
    TypeError: <test>:4:18: 'x' is already declared in this scope
    |}]

let%expect_test "typecheck: type annot mismatch on var" =
  run_src "func f() { var x: bool = 1 }";
  [%expect
    {|
    <test>:1:26: warning: 'x' declared but never used
    TypeError: <test>:1:26: expected bool but found i32
    |}]

let%expect_test "typecheck: use before decl" =
  run_src {|
func f() {
  x
  const x: i32 = 1
}
|};
  [%expect
    {|
    <test>:4:18: warning: 'x' declared but never used
    TypeError: <test>:3:3: undefined variable 'x'
    |}]

let%expect_test "typecheck: deref non-pointer" =
  run_src {|
func f() {
  const x: i32 = 1
  const y = *x
}
|};
  [%expect
    {|
    <test>:4:14: warning: 'y' declared but never used
    TypeError: <test>:4:14: cannot dereference non-pointer type 'i32'
    |}]

let%expect_test "typecheck: address-of and deref roundtrip" =
  run_src
    {|
func f() i32 {
  var x: i32 = 5
  const p: *i32 = &x
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
  [%expect {| TypeError: <test>:3:28: 'pt' has no field 'z' |}]

let%expect_test "typecheck: field access on non-struct" =
  run_src {|
func f() {
  const x: i32 = 1
  const y = x.foo
}
|};
  [%expect
    {|
    <test>:4:13: warning: 'y' declared but never used
    TypeError: <test>:4:13: type 'i32' has no fields
    |}]

let%expect_test "typecheck: field access auto-deref through ptr" =
  run_src {|
struct pt { x: i32 }
func f(p: *pt) i32 { return p.x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: duplicate function" =
  run_src {|
func f() {}
func f() {}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: arg type mismatch" =
  run_src {|
func g(x: i32) {}
func f() { g(true) }
|};
  [%expect {| TypeError: <test>:3:14: expected i32 but found bool |}]

let%expect_test "typecheck: extern decl callable" =
  run_src
    {|
extern func puts(s: *i8) i32
func f() {
  var p: *i8 = null
  puts(p)
}
|};
  [%expect {| ok |}]
