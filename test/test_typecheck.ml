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

let%expect_test "typecheck: array literal inferred" =
  run_src "func f() { const a = [1, 2, 3] a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: array annotated ok" =
  run_src "func f() { var a: [3]i32 = [1, 2, 3] a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: array element type mismatch" =
  run_src "func f() { var a: [2]i32 = [1, true] }";
  [%expect
    {|
    <test>:1:32: warning: 'a' declared but never used
    TypeError: <test>:1:32: expected i32 but found bool
    |}]

let%expect_test "typecheck: array wrong element count" =
  run_src "func f() { var a: [3]i32 = [1, 2] }";
  [%expect
    {|
    <test>:1:32: warning: 'a' declared but never used
    TypeError: <test>:1:28: expected 3 elements but found 2
    |}]

let%expect_test "typecheck: heterogeneous inferred literal" =
  run_src "func f() { const a = [1, true] a[0] }";
  [%expect {| TypeError: <test>:1:26: expected i32 but found bool |}]

let%expect_test "typecheck: empty array literal needs annotation" =
  run_src "func f() { const a = [] }";
  [%expect
    {|
    <test>:1:22: warning: 'a' declared but never used
    TypeError: <test>:1:22: cannot infer type of empty array literal
    |}]

let%expect_test "typecheck: index non-array" =
  run_src "func f() { var x: i32 = 0 x[0] }";
  [%expect {| TypeError: <test>:1:27: cannot index type 'i32' |}]

let%expect_test "typecheck: index non-integer" =
  run_src "func f() { var a: [2]i32 = [1, 2] a[true] }";
  [%expect {| TypeError: <test>:1:37: array index must be an integer |}]

let%expect_test "typecheck: index result type" =
  run_src "func f() i32 { var a: [2]i32 = [1, 2] return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: len is usize" =
  run_src "func f() usize { var a: [2]i32 = [1, 2] return a.len }";
  [%expect {| ok |}]

let%expect_test "typecheck: len mismatched with i32" =
  run_src "func f() i32 { var a: [2]i32 = [1, 2] return a.len }";
  [%expect {| TypeError: <test>:1:46: expected i32 but found usize |}]

let%expect_test "typecheck: array no such field" =
  run_src "func f() { var a: [2]i32 = [1, 2] a.foo }";
  [%expect {| TypeError: <test>:1:35: type '[2]i32' has no field 'foo' |}]

let%expect_test "typecheck: assign to index" =
  run_src "func f() { var a: [2]i32 = [1, 2] a[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: index element assign type mismatch" =
  run_src "func f() { var a: [2]i32 = [1, 2] a[0] = true }";
  [%expect {| TypeError: <test>:1:42: expected i32 but found bool |}]

let%expect_test "typecheck: for over range ok (branch 2)" =
  run_src "func f() { for i in 0..5 { const x = i } }";
  [%expect
    {|
    <test>:1:38: warning: 'x' declared but never used
    ok
    |}]

let%expect_test "typecheck: for over inclusive range ok (branch 2)" =
  run_src "func f() { for i in 0..=5 { const x = i } }";
  [%expect
    {|
    <test>:1:39: warning: 'x' declared but never used
    ok
    |}]

let%expect_test "typecheck: for over array binds element type" =
  run_src
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  for x in a { const y: i32 = x }
}
|};
  [%expect
    {|
    <test>:4:31: warning: 'y' declared but never used
    ok
    |}]

let%expect_test "typecheck: for over array wrong element use" =
  run_src
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  for x in a { const y: bool = x }
}
|};
  [%expect
    {|
    <test>:4:32: warning: 'y' declared but never used
    TypeError: <test>:4:32: expected bool but found i32
    |}]

let%expect_test "typecheck: for over non-iterable" =
  run_src "func f() { for x in 5 { const y = x } }";
  [%expect
    {|
    <test>:1:35: warning: 'y' declared but never used
    TypeError: <test>:1:21: cannot iterate over type 'i32'
    |}]

let%expect_test "typecheck: range bounds must be integers (branch 2)" =
  run_src "func f() { for i in true..5 { const x = i } }";
  [%expect
    {|
    <test>:1:41: warning: 'x' declared but never used
    TypeError: <test>:1:27: expected bool but found i32
    TypeError: <test>:1:27: range bounds must be integers
    |}]

let%expect_test "typecheck: range literal bends to typed endpoint (branch 1)" =
  run_src {|
func f() {
  const n: i64 = 5
  for i in 0..n { const x = i }
}
|};
  [%expect
    {|
    <test>:4:29: warning: 'x' declared but never used
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
  const n: i64 = 5
  for i in n..10 { const x = i }
}
|};
  [%expect
    {|
    <test>:4:30: warning: 'x' declared but never used
    ok
    |}]

let%expect_test "typecheck: slice bound over len needs no cast (branch 1)" =
  run_src
    {|
func f() {
  var a: [4]i32 = [1, 2, 3, 4]
  const s: []i32 = a[0..a.len]
}
|};
  [%expect
    {|
    <test>:4:22: warning: 's' declared but never used
    ok
    |}]

let%expect_test "typecheck: two typed endpoints still must match (branch 2)" =
  run_src
    {|
func f() {
  const m: i32 = 0
  const n: i64 = 5
  for i in m..n { const x = i }
}
|};
  [%expect
    {|
    <test>:5:29: warning: 'x' declared but never used
    TypeError: <test>:5:15: expected i32 but found i64
    |}]

let%expect_test "typecheck: break inside for" =
  run_src "func f() { for i in 0..5 { break } }";
  [%expect
    {|
    <test>:1:24: warning: 'i' declared but never used
    ok
    |}]

let%expect_test "typecheck: continue inside for" =
  run_src "func f() { for i in 0..5 { continue } }";
  [%expect
    {|
    <test>:1:24: warning: 'i' declared but never used
    ok
    |}]

let%expect_test "typecheck: unused loop variable warns" =
  run_src "func f() { for i in 0..5 { const _x = 1 } }";
  [%expect
    {|
    <test>:1:24: warning: 'i' declared but never used
    ok
    |}]

let%expect_test "typecheck: array coerces to slice param" =
  run_src
    {|
func sum(xs: []i32) i32 { return 0 }
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  return sum(a)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: slice element wrong type rejected" =
  run_src
    {|
func sum(xs: []i32) {}
func f() {
  var a: [2]f32 = [1.0, 2.0]
  sum(a)
}
|};
  [%expect {| TypeError: <test>:5:7: expected []i32 but found [2]f32 |}]

let%expect_test "typecheck: sub-slice ok" =
  run_src
    "func f() i32 { var a: [4]i32 = [1,2,3,4] const s: []i32 = a[1..3] return \
     s[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: slice of a slice ok" =
  run_src
    {|
func f() i32 {
  var a: [5]i32 = [1, 2, 3, 4, 5]
  const s: []i32 = a[1..5]
  const t: []i32 = s[1..3]
  return t[0]
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: inclusive range slice ok (branch 2)" =
  run_src
    "func f() i32 { var a: [3]i32 = [1,2,3] const s: []i32 = a[0..=2] return \
     s[2] }";
  [%expect {| ok |}]

let%expect_test "typecheck: slice bounds must be integers (branch 2)" =
  run_src "func f() { var a: [3]i32 = [1,2,3] const s: []i32 = a[true..2] }";
  [%expect
    {|
    <test>:1:61: warning: 's' declared but never used
    TypeError: <test>:1:61: expected bool but found i32
    TypeError: <test>:1:61: range bounds must be integers
    |}]

let%expect_test "typecheck: slice .len is usize" =
  run_src
    "func f() usize { var a: [3]i32 = [1,2,3] const s: []i32 = a[0..3] return \
     s.len }";
  [%expect {| ok |}]

let%expect_test "typecheck: slice .ptr is pointer" =
  run_src
    {|
func first(p: *i32) i32 { return 0 }
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  const s: []i32 = a[0..3]
  return first(s.ptr)
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: slice does not coerce back to array" =
  run_src "func f() { var a: [3]i32 = [1,2,3] var b: [3]i32 = a[0..3] }";
  [%expect
    {|
    <test>:1:57: warning: 'b' declared but never used
    TypeError: <test>:1:57: expected [3]i32 but found []i32
    |}]

let%expect_test "typecheck: for over slice binds element" =
  run_src
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  const s: []i32 = a[0..3]
  for x in s { const y: i32 = x }
}
|};
  [%expect
    {|
    <test>:5:31: warning: 'y' declared but never used
    ok
    |}]

let%expect_test "typecheck: slice index element assignable" =
  run_src "func f() { var a: [3]i32 = [1,2,3] var s: []i32 = a[0..3] s[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: compound assign to array element" =
  run_src "func f() i32 { var a: [3]i32 = [1,2,3] a[0] += 5 return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: multidimensional array" =
  run_src "func f() i32 { var m: [2][2]i32 = [[1,2],[3,4]] return m[1][0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: multidim wrong inner count" =
  run_src "func f() { var m: [2][2]i32 = [[1,2],[3]] }";
  [%expect
    {|
    <test>:1:39: warning: 'm' declared but never used
    TypeError: <test>:1:38: expected 2 elements but found 1
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
    {| TypeError: <test>:3:23: initializer for 'g' must be a constant expression |}]

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
    {| TypeError: <test>:1:23: a range can only be used in a for-loop or a slice |}]

let%expect_test "typecheck: range in condition is an error" =
  run_src "func f() { if 0..5 { } }";
  [%expect
    {|
    TypeError: <test>:1:15: a range can only be used in a for-loop or a slice
    TypeError: <test>:1:15: expected bool but found i32
    |}]

let%expect_test "typecheck: for over array literal" =
  run_src "func f() i32 { var s: i32 = 0 for x in [1,2,3] { s += x } return s }";
  [%expect {| ok |}]

let%expect_test "typecheck: array literal as slice argument" =
  run_src
    {|
func sum(xs: []i32) i32 { return 0 }
func f() i32 { return sum([1, 2, 3]) }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: scalar zero init" =
  run_src "func f() i32 { var x: i32 return x }";
  [%expect {| ok |}]

let%expect_test "typecheck: array zero init" =
  run_src "func f() i32 { var a: [3]i32 return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: var without type or value cannot infer" =
  run_src "func f() { var x }";
  [%expect
    {|
    <test>:1:12: warning: 'x' declared but never used
    TypeError: <test>:1:12: cannot infer type of 'x'
    |}]

let%expect_test "typecheck: undefined without type cannot infer" =
  run_src "func f() { var x = undefined }";
  [%expect
    {|
    <test>:1:20: warning: 'x' declared but never used
    TypeError: <test>:1:20: cannot infer type of undefined
    |}]

let%expect_test "typecheck: undefined with type ok" =
  run_src "func f() i32 { var x: i32 = undefined return x }";
  [%expect {| ok |}]

let%expect_test "typecheck: missing return on a path" =
  run_src "func f(n: i32) i32 { if n > 0 { return 1 } }";
  [%expect {| TypeError: <test>:1:1: missing return in 'f' |}]

let%expect_test "typecheck: if and else both return ok" =
  run_src "func f(n: i32) i32 { if n > 0 { return 1 } else { return 0 } }";
  [%expect {| ok |}]

let%expect_test "typecheck: struct literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  const p = pt { x: 3, y: 4 }
  return p.x + p.y
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: empty struct literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  const p = pt { }
  return p.x
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: struct literal unknown field" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() {
  const p = pt { z: 1 }
}
|};
  [%expect
    {|
    <test>:4:13: warning: 'p' declared but never used
    TypeError: <test>:4:13: 'pt' has no field 'z'
    |}]

let%expect_test "typecheck: struct literal duplicate field" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() {
  const p = pt { x: 1, x: 2 }
}
|};
  [%expect
    {|
    <test>:4:21: warning: 'p' declared but never used
    TypeError: <test>:4:13: duplicate field 'x'
    |}]

let%expect_test "typecheck: struct literal wrong field type" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func f() {
  const p = pt { x: true }
}
|};
  [%expect
    {|
    <test>:4:21: warning: 'p' declared but never used
    TypeError: <test>:4:21: expected i32 but found bool
    |}]

let%expect_test "typecheck: undefined struct literal" =
  run_src {|
func f() {
  const p = nope { x: 1 }
}
|};
  [%expect
    {|
    <test>:3:13: warning: 'p' declared but never used
    TypeError: <test>:3:13: undefined struct 'nope'
    |}]

let%expect_test "typecheck: const global struct literal" =
  run_src
    {|
struct pt { x: i32, y: i32 }
const origin: pt = pt { x: 1, y: 2 }
func f() i32 { return origin.x }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global struct literal must be constant" =
  run_src
    {|
struct pt { x: i32, y: i32 }
func g() i32 { return 1 }
const p: pt = pt { x: g(), y: 2 }
|};
  [%expect
    {| TypeError: <test>:4:31: initializer for 'p' must be a constant expression |}]

let%expect_test "typecheck: duplicate struct field" =
  run_src {|
struct pt { x: i32, x: i64 }
|};
  [%expect {| TypeError: <test>:2:21: duplicate field 'x' in struct 'pt' |}]

let%expect_test "typecheck: three duplicate struct fields" =
  run_src {|
struct pt { x: i32, x: i64, x: bool }
|};
  [%expect
    {|
    TypeError: <test>:2:21: duplicate field 'x' in struct 'pt'
    TypeError: <test>:2:29: duplicate field 'x' in struct 'pt'
    |}]
