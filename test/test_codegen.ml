(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

let%expect_test "codegen: compound assign on global" =
  run_codegen {|
var n: i32 = 0
func f() { n += 1 }
|};
  [%expect
    {|
    data $n = align 4 { w 0 }

    function $f() {
    @start
        %t0 =w loadsw $n
        %t1 =w add %t0, 1
        storew %t1, $n
        ret
    }
    |}]

let%expect_test "codegen: compound assign on local" =
  run_codegen {|
func f() {
  var n: i32 = 0
  n += 1
}
|};
  [%expect
    {|
    function $f() {
    @start
        %n =l alloc4 4
        storew 0, %n
        %t0 =w loadsw %n
        %t1 =w add %t0, 1
        storew %t1, %n
        ret
    }
    |}]

let%expect_test "codegen: plain assign on global" =
  run_codegen {|
var n: i32 = 0
func f() { n = n + 1 }
|};
  [%expect
    {|
    data $n = align 4 { w 0 }

    function $f() {
    @start
        %t0 =w loadsw $n
        %t1 =w add %t0, 1
        storew %t1, $n
        ret
    }
    |}]

let%expect_test "codegen: address-of local" =
  run_codegen {|
func f() {
  var n: i32 = 0
  var p: *i32 = &n
}
|};
  [%expect
    {|
    <test>:4:18: warning: 'p' declared but never used
    function $f() {
    @start
        %n =l alloc4 4
        storew 0, %n
        %p =l alloc8 8
        %t0 =l copy %n
        storel %t0, %p
        ret
    }
    |}]

let%expect_test "codegen: address-of global" =
  run_codegen {|
var n: i32 = 0
func f() {
  var p: *i32 = &n
}
|};
  [%expect
    {|
    <test>:4:18: warning: 'p' declared but never used
    data $n = align 4 { w 0 }

    function $f() {
    @start
        %p =l alloc8 8
        %t0 =l copy $n
        storel %t0, %p
        ret
    }
    |}]

let%expect_test "codegen: call through global fn ptr" =
  run_codegen
    {|
func add(a: i32, b: i32) i32 { return a + b }
var op: (i32, i32) i32 = add
func f() { op(1, 2) }
|};
  [%expect
    {|
    TypeError: <test>:3:26: initializer for 'op' must be a constant expression
    TypeError: <test>:4:12: undefined function 'op'
    TypeError: <test>:4:12: expected 0 arguments but got 2
    |}]

let%expect_test "codegen: simple add return" =
  run_codegen "func add(a: i32, b: i32) i32 { return a + b }";
  [%expect
    {|
    function w $add(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
        %t2 =w loadsw %a
        %t3 =w loadsw %b
        %t4 =w add %t2, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: const arithmetic local" =
  run_codegen {|
func f() i32 {
  const x: i32 = 1 + 2
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        %t0 =w add 1, 2
        storew %t0, %x
        %t1 =w loadsw %x
        ret %t1
    }
    |}]

let%expect_test "codegen: comparison return" =
  run_codegen "func f(a: i32, b: i32) bool { return a < b }";
  [%expect
    {|
    function w $f(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
        %t2 =w loadsw %a
        %t3 =w loadsw %b
        %t4 =w csltw %t2, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: bitwise and shift" =
  run_codegen "func f(a: i32) i32 { return (a & 15) << 2 }";
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w and %t1, 15
        %t3 =w shl %t2, 2
        ret %t3
    }
    |}]

let%expect_test "codegen: unary minus and not" =
  run_codegen
    {|
func f(a: i32, b: bool) i32 {
  if !b { return -a }
  return a
}
|};
  [%expect
    {|
    function w $f(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
    @if.cond2_0
        %t3 =w loadub %b
        %t4 =w ceqw %t3, 0
        jnz %t4, @if.then2_0, @if.else2
    @if.then2_0
        %t5 =w loadsw %a
        %t6 =w neg %t5
        ret %t6
        jmp @if.end2
    @if.else2
    @if.end2
        %t7 =w loadsw %a
        ret %t7
    }
    |}]

let%expect_test "codegen: if/else" =
  run_codegen "func f(a: i32) i32 { if a < 0 { return 0 } else { return a } }";
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
    @if.cond1_0
        %t2 =w loadsw %a
        %t3 =w csltw %t2, 0
        jnz %t3, @if.then1_0, @if.else1
    @if.then1_0
        ret 0
        jmp @if.end1
    @if.else1
        %t4 =w loadsw %a
        ret %t4
    @if.end1
    }
    |}]

let%expect_test "codegen: while loop" =
  run_codegen
    {|
func f() i32 {
  var i: i32 = 0
  while i < 10 { i = i + 1 }
  return i
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %i =l alloc4 4
        storew 0, %i
    @while.cond0
        %t1 =w loadsw %i
        %t2 =w csltw %t1, 10
        jnz %t2, @while.body0, @while.end0
    @while.body0
        %t3 =w loadsw %i
        %t4 =w add %t3, 1
        storew %t4, %i
        jmp @while.cond0
    @while.end0
        %t5 =w loadsw %i
        ret %t5
    }
    |}]

let%expect_test "codegen: nested if" =
  run_codegen
    "func f(a: i32, b: i32) i32 { if a > 0 { if b > 0 { return 1 } } return 0 }";
  [%expect
    {|
    function w $f(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
    @if.cond2_0
        %t3 =w loadsw %a
        %t4 =w csgtw %t3, 0
        jnz %t4, @if.then2_0, @if.else2
    @if.then2_0
    @if.cond5_0
        %t6 =w loadsw %b
        %t7 =w csgtw %t6, 0
        jnz %t7, @if.then5_0, @if.else5
    @if.then5_0
        ret 1
        jmp @if.end5
    @if.else5
    @if.end5
        jmp @if.end2
    @if.else2
    @if.end2
        ret 0
    }
    |}]

let%expect_test "codegen: call with return value" =
  run_codegen
    {|
func add(a: i32, b: i32) i32 { return a + b }
func f() i32 { return add(1, 2) }
|};
  [%expect
    {|
    function w $add(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
        %t2 =w loadsw %a
        %t3 =w loadsw %b
        %t4 =w add %t2, %t3
        ret %t4
    }

    function w $f() {
    @start
        %t0 =w call $add(w 1, w 2)
        ret %t0
    }
    |}]

let%expect_test "codegen: void call discards" =
  run_codegen {|
func g(x: i32) {}
func f() { g(42) }
|};
  [%expect
    {|
    function $g(w %t0) {
    @start
        %x =l alloc4 4
        storew %t0, %x
        ret
    }

    function $f() {
    @start
        call $g(w 42)
        ret
    }
    |}]

let%expect_test "codegen: int widen cast" =
  run_codegen "func f(a: i32) i64 { return a as i64 }";
  [%expect
    {|
    function l $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =l extsw %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: sizeof int" =
  run_codegen "func f() i64 { return sizeof(i32) as i64 }";
  [%expect
    {|
    function l $f() {
    @start
        %t0 =l copy 4
        ret %t0
    }
    |}]

let%expect_test "codegen: global const int" =
  run_codegen {|
const X: i32 = 42
func f() i32 { return X }
|};
  [%expect
    {|
    data $X = align 4 { w 42 }

    function w $f() {
    @start
        %t0 =w loadsw $X
        ret %t0
    }
    |}]

let%expect_test "codegen: global var zero init bool" =
  run_codegen {|
var flag: bool
func f() bool { return flag }
|};
  [%expect
    {|
    data $flag = align 1 { z 1 }

    function w $f() {
    @start
        %t0 =w loadub $flag
        ret %t0
    }
    |}]

let%expect_test "codegen: global var explicit init" =
  run_codegen {|
var n: i32 = 7
func f() i32 { return n }
|};
  [%expect
    {|
    data $n = align 4 { w 7 }

    function w $f() {
    @start
        %t0 =w loadsw $n
        ret %t0
    }
    |}]

let%expect_test "codegen: deref read local ptr" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 5
  var p: *i32 = &x
  return *p
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 5, %x
        %p =l alloc8 8
        %t0 =l copy %x
        storel %t0, %p
        %t1 =l loadl %p
        %t2 =w loadsw %t1
        ret %t2
    }
    |}]
