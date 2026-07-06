(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

let%expect_test "typecheck: break outside loop" =
  run_src "func f() { break }";
  [%expect
    {|
    error: break outside loop
      at <test>:1:12
        func f() { break }
                   ^~~~~
    |}]

let%expect_test "typecheck: continue outside loop" =
  run_src "func f() { continue }";
  [%expect
    {|
    error: continue outside loop
      at <test>:1:12
        func f() { continue }
                   ^~~~~~~~
    |}]

let%expect_test "typecheck: unbound variable" =
  run_src "func f() { x }";
  [%expect
    {|
    error: undefined variable: x
      at <test>:1:12
        func f() { x }
                   ^
    |}]

let%expect_test "typecheck: type mismatch in let" =
  run_src "func f() { const x: bool = 42 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x: bool = 42 }
                         ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:28
        func f() { const x: bool = 42 }
                                   ^~ expected bool, found i32
    |}]

let%expect_test "typecheck: wrong number of arguments" =
  run_src {|
func g() {}
func f() { g(1) }
|};
  [%expect
    {|
    error: expected 0 arguments, found 1
      at <test>:3:12
        func f() { g(1) }
                   ^~~~
    |}]

let%expect_test "typecheck: null assigned to non-pointer" =
  run_src "func f() { const x: i32 = null }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x: i32 = null }
                         ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:27
        func f() { const x: i32 = null }
                                  ^~~~ expected i32, found null
    |}]

let%expect_test "typecheck: identity function" =
  run_src "func id(a: i32) i32 { return a }";
  [%expect {| ok |}]

let%expect_test "typecheck: null assigned to pointer" =
  run_src "func f() { const p: *i32 = null }";
  [%expect
    {|
    warning: unused variable: p
      at <test>:1:18
        func f() { const p: *i32 = null }
                         ^
    help: prefix with an underscore: _p
    ok
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
    warning: unused variable: op
      at <test>:4:7
          var op: (i32) i32 = add
              ^~
    help: prefix with an underscore: _op
    error: type mismatch
      at <test>:4:23
          var op: (i32) i32 = add
                              ^~~ expected (i32) i32, found (i32, i32) i32
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
    error: not callable: x
      at <test>:4:3
          x(1)
          ^~~~
    |}]

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
  [%expect
    {|
    error: expected 2 arguments, found 1
      at <test>:5:3
          op(1)
          ^~~~~
    |}]

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
  [%expect
    {|
    error: cannot assign to const: X
      at <test>:3:12
        func f() { X = 2 }
                   ^
    |}]

let%expect_test "typecheck: non-const global initializer" =
  run_src {|
func g() i32 { return 1 }
const X: i32 = g()
|};
  [%expect
    {|
    error: initializer must be constant: X
      at <test>:3:16
        const X: i32 = g()
                       ^~~
    |}]

let%expect_test "typecheck: const requires initializer" =
  run_src "const X: i32";
  [%expect
    {|
    error: const without initializer: X
      at <test>:1:1
        const X: i32
        ^~~~~~~~~~~~
    |}]

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
    warning: unused variable: c
      at <test>:5:9
          const c = a + b
                ^
    help: prefix with an underscore: _c
    error: type mismatch
      at <test>:5:17
          const c = a + b
                        ^ expected i32, found i64
    |}]

let%expect_test "typecheck: bool arithmetic rejected" =
  run_src "func f() { const x = true + false }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x = true + false }
                         ^
    help: prefix with an underscore: _x
    error: cannot apply `+` to bool
      at <test>:1:22
        func f() { const x = true + false }
                             ^~~~
    |}]

let%expect_test "typecheck: comparison yields bool" =
  run_src "func f() bool { return 1 < 2 }";
  [%expect {| ok |}]

let%expect_test "typecheck: logical and/or require bool" =
  run_src "func f() { const x = 1 && 2 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x = 1 && 2 }
                         ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:22
        func f() { const x = 1 && 2 }
                             ^ expected bool, found i32
    error: type mismatch
      at <test>:1:27
        func f() { const x = 1 && 2 }
                                  ^ expected bool, found i32
    |}]

let%expect_test "typecheck: not on non-bool" =
  run_src "func f() { const x = !1 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x = !1 }
                         ^
    help: prefix with an underscore: _x
    error: type mismatch
      at <test>:1:23
        func f() { const x = !1 }
                              ^ expected bool, found i32
    |}]

let%expect_test "typecheck: address of rvalue rejected" =
  run_src "func f() { const x = &5 }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x = &5 }
                         ^
    help: prefix with an underscore: _x
    error: cannot take address of expression
      at <test>:1:23
        func f() { const x = &5 }
                              ^
    |}]

let%expect_test "typecheck: shift on int ok" =
  run_src "func f() i32 { return 1 << 3 }";
  [%expect {| ok |}]

let%expect_test "typecheck: bitwise on bool rejected" =
  run_src "func f() { const x = true & false }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:18
        func f() { const x = true & false }
                         ^
    help: prefix with an underscore: _x
    error: cannot apply `&` to bool
      at <test>:1:22
        func f() { const x = true & false }
                             ^~~~
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
    warning: unused variable: p
      at <test>:1:18
        func f() { const p: *i32 = true as *i32 }
                         ^
    help: prefix with an underscore: _p
    ok
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

let%expect_test "typecheck: assign to const local" =
  run_src {|
func f() {
  const x: i32 = 1
  x = 2
}
|};
  [%expect {| ok |}]

let%expect_test "typecheck: redeclare local shadows" =
  run_src {|
func f() {
  const x: i32 = 1
  const x: i32 = 2
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:3:9
          const x: i32 = 1
                ^
    help: prefix with an underscore: _x
    warning: unused variable: x
      at <test>:4:9
          const x: i32 = 2
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
  const x: i32 = 1
}
|};
  [%expect
    {|
    error: undefined variable: x
      at <test>:3:3
          x
          ^
    warning: unused variable: x
      at <test>:4:9
          const x: i32 = 1
                ^
    help: prefix with an underscore: _x
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
    warning: unused variable: y
      at <test>:4:9
          const y = *x
                ^
    help: prefix with an underscore: _y
    error: cannot dereference type: i32
      at <test>:4:14
          const y = *x
                     ^
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
  [%expect
    {|
    error: no field: z
      at <test>:3:28
        func f(p: pt) i32 { return p.z }
                                   ^~~ on struct pt
    |}]

let%expect_test "typecheck: field access on non-struct" =
  run_src {|
func f() {
  const x: i32 = 1
  const y = x.foo
}
|};
  [%expect
    {|
    warning: unused variable: y
      at <test>:4:9
          const y = x.foo
                ^
    help: prefix with an underscore: _y
    error: type has no fields: i32
      at <test>:4:13
          const y = x.foo
                    ^~~~~
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
  [%expect
    {|
    error: type mismatch
      at <test>:3:14
        func f() { g(true) }
                     ^~~~ expected i32, found bool
    |}]

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
    error: expected 3 elements, found 2
      at <test>:1:28
        func f() { var a: [3]i32 = [1, 2] }
                                   ^~~~~~
    |}]

let%expect_test "typecheck: heterogeneous inferred literal" =
  run_src "func f() { const a = [1, true] a[0] }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:26
        func f() { const a = [1, true] a[0] }
                                 ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: empty array literal needs annotation" =
  run_src "func f() { const a = [] }";
  [%expect
    {|
    warning: unused variable: a
      at <test>:1:18
        func f() { const a = [] }
                         ^
    help: prefix with an underscore: _a
    error: cannot infer type of empty array literal
      at <test>:1:22
        func f() { const a = [] }
                             ^~
    |}]

let%expect_test "typecheck: index non-array" =
  run_src "func f() { var x: i32 = 0 x[0] }";
  [%expect
    {|
    error: cannot index type: i32
      at <test>:1:27
        func f() { var x: i32 = 0 x[0] }
                                  ^~~~
    |}]

let%expect_test "typecheck: index non-integer" =
  run_src "func f() { var a: [2]i32 = [1, 2] a[true] }";
  [%expect
    {|
    error: array index must be an integer
      at <test>:1:37
        func f() { var a: [2]i32 = [1, 2] a[true] }
                                            ^~~~
    |}]

let%expect_test "typecheck: index result type" =
  run_src "func f() i32 { var a: [2]i32 = [1, 2] return a[0] }";
  [%expect {| ok |}]

let%expect_test "typecheck: len is usize" =
  run_src "func f() usize { var a: [2]i32 = [1, 2] return a.len }";
  [%expect {| ok |}]

let%expect_test "typecheck: len mismatched with i32" =
  run_src "func f() i32 { var a: [2]i32 = [1, 2] return a.len }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:46
        func f() i32 { var a: [2]i32 = [1, 2] return a.len }
                                                     ^~~~~ expected i32, found usize
    |}]

let%expect_test "typecheck: array no such field" =
  run_src "func f() { var a: [2]i32 = [1, 2] a.foo }";
  [%expect
    {|
    error: no field: foo
      at <test>:1:35
        func f() { var a: [2]i32 = [1, 2] a.foo }
                                          ^~~~~ on [2]i32
    |}]

let%expect_test "typecheck: assign to index" =
  run_src "func f() { var a: [2]i32 = [1, 2] a[0] = 9 }";
  [%expect {| ok |}]

let%expect_test "typecheck: index element assign type mismatch" =
  run_src "func f() { var a: [2]i32 = [1, 2] a[0] = true }";
  [%expect
    {|
    error: type mismatch
      at <test>:1:42
        func f() { var a: [2]i32 = [1, 2] a[0] = true }
                                                 ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: for over range ok (branch 2)" =
  run_src "func f() { for i in 0..5 { const x = i } }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:34
        func f() { for i in 0..5 { const x = i } }
                                         ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "typecheck: for over inclusive range ok (branch 2)" =
  run_src "func f() { for i in 0..=5 { const x = i } }";
  [%expect
    {|
    warning: unused variable: x
      at <test>:1:35
        func f() { for i in 0..=5 { const x = i } }
                                          ^
    help: prefix with an underscore: _x
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
    warning: unused variable: y
      at <test>:4:22
          for x in a { const y: i32 = x }
                             ^
    help: prefix with an underscore: _y
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
    warning: unused variable: y
      at <test>:4:22
          for x in a { const y: bool = x }
                             ^
    help: prefix with an underscore: _y
    error: type mismatch
      at <test>:4:32
          for x in a { const y: bool = x }
                                       ^ expected bool, found i32
    |}]

let%expect_test "typecheck: for over non-iterable" =
  run_src "func f() { for x in 5 { const y = x } }";
  [%expect
    {|
    error: cannot iterate over type: i32
      at <test>:1:21
        func f() { for x in 5 { const y = x } }
                            ^
    warning: unused variable: y
      at <test>:1:31
        func f() { for x in 5 { const y = x } }
                                      ^
    help: prefix with an underscore: _y
    |}]

let%expect_test "typecheck: range bounds must be integers (branch 2)" =
  run_src "func f() { for i in true..5 { const x = i } }";
  [%expect
    {|
    error: range bounds must be integers
      at <test>:1:21
        func f() { for i in true..5 { const x = i } }
                            ^~~~
    error: type mismatch
      at <test>:1:27
        func f() { for i in true..5 { const x = i } }
                                  ^ expected bool, found i32
    warning: unused variable: x
      at <test>:1:37
        func f() { for i in true..5 { const x = i } }
                                            ^
    help: prefix with an underscore: _x
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
    warning: unused variable: x
      at <test>:4:25
          for i in 0..n { const x = i }
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
  const n: i64 = 5
  for i in n..10 { const x = i }
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:4:26
          for i in n..10 { const x = i }
                                 ^
    help: prefix with an underscore: _x
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
    warning: unused variable: s
      at <test>:4:9
          const s: []i32 = a[0..a.len]
                ^
    help: prefix with an underscore: _s
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
    error: type mismatch
      at <test>:5:15
          for i in m..n { const x = i }
                      ^ expected i32, found i64
    warning: unused variable: x
      at <test>:5:25
          for i in m..n { const x = i }
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
  run_src "func f() { for i in 0..5 { const _x = 1 } }";
  [%expect
    {|
    warning: unused variable: i
      at <test>:1:16
        func f() { for i in 0..5 { const _x = 1 } }
                       ^
    help: prefix with an underscore: _i
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
  [%expect
    {|
    error: type mismatch
      at <test>:5:7
          sum(a)
              ^ expected []i32, found [2]f32
    |}]

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
    warning: unused variable: s
      at <test>:1:42
        func f() { var a: [3]i32 = [1,2,3] const s: []i32 = a[true..2] }
                                                 ^
    help: prefix with an underscore: _s
    error: range bounds must be integers
      at <test>:1:55
        func f() { var a: [3]i32 = [1,2,3] const s: []i32 = a[true..2] }
                                                              ^~~~
    error: type mismatch
      at <test>:1:61
        func f() { var a: [3]i32 = [1,2,3] const s: []i32 = a[true..2] }
                                                                    ^ expected bool, found i32
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
    warning: unused variable: b
      at <test>:1:40
        func f() { var a: [3]i32 = [1,2,3] var b: [3]i32 = a[0..3] }
                                               ^
    help: prefix with an underscore: _b
    error: type mismatch
      at <test>:1:52
        func f() { var a: [3]i32 = [1,2,3] var b: [3]i32 = a[0..3] }
                                                           ^~~~~~~ expected [3]i32, found []i32
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
    warning: unused variable: y
      at <test>:5:22
          for x in s { const y: i32 = x }
                             ^
    help: prefix with an underscore: _y
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
    warning: unused variable: m
      at <test>:1:16
        func f() { var m: [2][2]i32 = [[1,2],[3]] }
                       ^
    help: prefix with an underscore: _m
    error: expected 2 elements, found 1
      at <test>:1:38
        func f() { var m: [2][2]i32 = [[1,2],[3]] }
                                             ^~~
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
    error: initializer must be constant: g
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
    error: type mismatch
      at <test>:1:15
        func f() { if 0..5 { } }
                      ^~~~ expected bool, found i32
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
    error: cannot infer type: x
      at <test>:1:16
        func f() { var x }
                       ^
    warning: unused variable: x
      at <test>:1:16
        func f() { var x }
                       ^
    help: prefix with an underscore: _x
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
  run_src "func f() i32 { var x: i32 = undefined return x }";
  [%expect {| ok |}]

let%expect_test "typecheck: missing return on a path" =
  run_src "func f(n: i32) i32 { if n > 0 { return 1 } }";
  [%expect
    {|
    error: missing return: f
      at <test>:1:16
        func f(n: i32) i32 { if n > 0 { return 1 } }
                       ^~~
    |}]

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
    warning: unused variable: p
      at <test>:4:9
          const p = pt { z: 1 }
                ^
    help: prefix with an underscore: _p
    error: no field: z
      at <test>:4:18
          const p = pt { z: 1 }
                         ^
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
    warning: unused variable: p
      at <test>:4:9
          const p = pt { x: 1, x: 2 }
                ^
    help: prefix with an underscore: _p
    error: duplicate field: x
      at <test>:4:24
          const p = pt { x: 1, x: 2 }
                               ^
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
    warning: unused variable: p
      at <test>:4:9
          const p = pt { x: true }
                ^
    help: prefix with an underscore: _p
    error: type mismatch
      at <test>:4:21
          const p = pt { x: true }
                            ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: undefined struct literal" =
  run_src {|
func f() {
  const p = nope { x: 1 }
}
|};
  [%expect
    {|
    warning: unused variable: p
      at <test>:3:9
          const p = nope { x: 1 }
                ^
    help: prefix with an underscore: _p
    error: undefined struct: nope
      at <test>:3:13
          const p = nope { x: 1 }
                    ^~~~
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
    {|
    error: initializer must be constant: p
      at <test>:4:15
        const p: pt = pt { x: g(), y: 2 }
                      ^~~~~~~~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: duplicate struct field" =
  run_src {|
struct pt { x: i32, x: i64 }
|};
  [%expect
    {|
    error: duplicate field: x
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
    error: duplicate field: x
      at <test>:2:21
        struct pt { x: i32, x: i64, x: bool }
                            ^
    error: duplicate field: x
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
    error: type mismatch
      at <test>:4:14
        func g() { f(true) }
                     ^~~~ expected i64, found bool
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
extern func strlen(s: cstr) i64
func f() i64 { return strlen("hi") }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern variadic accepts extra args" =
  run_src
    {|
extern func printf(fmt: cstr, ...) i32
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

let%expect_test "typecheck: modifiers public inline typecheck ok" =
  run_src {|
public inline func f() i32 { return 1 }
|};
  [%expect {| ok |}]

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
    error: type mismatch
      at <test>:5:8
          take(a)
               ^ expected [4]i32, found [3]i32
    |}]

let%expect_test "typecheck: global var initialized from const" =
  run_src
    {|
const base: i32 = 10
var counter: i32 = base
func f() i32 { return counter }
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern variadic requires the fixed args" =
  run_src {|
extern func printf(fmt: cstr, ...) i32
func f() { printf() }
|};
  [%expect
    {|
    error: expected at least 1 arguments, found 0
      at <test>:3:12
        func f() { printf() }
                   ^~~~~~~~
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
    error: cannot apply `<` to bool
      at <test>:5:10
          return a < b
                 ^
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
    error: cannot apply `==` to P
      at <test>:6:10
          return a == b
                 ^
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
    error: cannot apply `==` to void
      at <test>:4:10
          return g() == g()
                 ^~~
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
