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

let%expect_test "codegen: array literal init and index read" =
  run_codegen
    {|
func f() i32 {
  var a: [3]i32 = [10, 20, 30]
  return a[1]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 12
        storew 10, %a
        %t0 =l add %a, 4
        storew 20, %t0
        %t1 =l add %a, 8
        storew 30, %t1
        %t2 =l extsw 1
        %t3 =l mul %t2, 4
        %t4 =l add %a, %t3
        %t5 =w loadsw %t4
        ret %t5
    }
    |}]

let%expect_test "codegen: array index store" =
  run_codegen {|
func f() {
  var a: [2]i32 = [1, 2]
  a[0] = 9
}
|};
  [%expect
    {|
    function $f() {
    @start
        %a =l alloc4 8
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l extsw 0
        %t2 =l mul %t1, 4
        %t3 =l add %a, %t2
        storew 9, %t3
        ret
    }
    |}]

let%expect_test "codegen: array len is a constant" =
  run_codegen
    {|
func f() i64 {
  var a: [4]i32 = [1, 2, 3, 4]
  return a.len
}
|};
  [%expect
    {|
    function l $f() {
    @start
        %a =l alloc4 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l add %a, 12
        storew 4, %t2
        ret 4
    }
    |}]

let%expect_test "codegen: array of i64 uses stride 8" =
  run_codegen {|
func f() i64 {
  var a: [2]i64 = [1, 2]
  return a[1]
}
|};
  [%expect
    {|
    function l $f() {
    @start
        %a =l alloc8 16
        storel 1, %a
        %t0 =l add %a, 8
        storel 2, %t0
        %t1 =l extsw 1
        %t2 =l mul %t1, 8
        %t3 =l add %a, %t2
        %t4 =l loadl %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: for over range" =
  run_codegen
    {|
func f() i32 {
  var sum: i32 = 0
  for i in 0..3 { sum += i }
  return sum
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %sum =l alloc4 4
        storew 0, %sum
        %i =l alloc4 4
        storew 0, %i
    @for.cond0
        %t1 =w loadsw %i
        %t2 =w csltw %t1, 3
        jnz %t2, @for.body0, @for.end0
    @for.body0
        %t3 =w loadsw %sum
        %t4 =w loadsw %i
        %t5 =w add %t3, %t4
        storew %t5, %sum
    @for.cont0
        %t6 =w loadsw %i
        %t7 =w add %t6, 1
        storew %t7, %i
        jmp @for.cond0
    @for.end0
        %t8 =w loadsw %sum
        ret %t8
    }
    |}]

let%expect_test "codegen: for over inclusive range" =
  run_codegen
    {|
func f() i32 {
  var sum: i32 = 0
  for i in 0..=3 { sum += i }
  return sum
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %sum =l alloc4 4
        storew 0, %sum
        %i =l alloc4 4
        storew 0, %i
    @for.cond0
        %t1 =w loadsw %i
        %t2 =w cslew %t1, 3
        jnz %t2, @for.body0, @for.end0
    @for.body0
        %t3 =w loadsw %sum
        %t4 =w loadsw %i
        %t5 =w add %t3, %t4
        storew %t5, %sum
    @for.cont0
        %t6 =w loadsw %i
        %t7 =w add %t6, 1
        storew %t7, %i
        jmp @for.cond0
    @for.end0
        %t8 =w loadsw %sum
        ret %t8
    }
    |}]

let%expect_test "codegen: for over array" =
  run_codegen
    {|
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  var sum: i32 = 0
  for x in a { sum += x }
  return sum
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 12
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %sum =l alloc4 4
        storew 0, %sum
        %for.i2 =l alloc8 8
        storel 0, %for.i2
        %x =l alloc4 4
    @for.cond2
        %t3 =l loadl %for.i2
        %t4 =w csltl %t3, 3
        jnz %t4, @for.body2, @for.end2
    @for.body2
        %t5 =l mul %t3, 4
        %t6 =l add %a, %t5
        %t7 =w loadsw %t6
        storew %t7, %x
        %t8 =w loadsw %sum
        %t9 =w loadsw %x
        %t10 =w add %t8, %t9
        storew %t10, %sum
    @for.cont2
        %t11 =l loadl %for.i2
        %t12 =l add %t11, 1
        storel %t12, %for.i2
        jmp @for.cond2
    @for.end2
        %t13 =w loadsw %sum
        ret %t13
    }
    |}]

let%expect_test "codegen: break and continue in for" =
  run_codegen
    {|
func f() i32 {
  var sum: i32 = 0
  for i in 0..10 {
    if i == 2 { continue }
    if i == 5 { break }
    sum += i
  }
  return sum
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %sum =l alloc4 4
        storew 0, %sum
        %i =l alloc4 4
        storew 0, %i
    @for.cond0
        %t1 =w loadsw %i
        %t2 =w csltw %t1, 10
        jnz %t2, @for.body0, @for.end0
    @for.body0
    @if.cond3_0
        %t4 =w loadsw %i
        %t5 =w ceqw %t4, 2
        jnz %t5, @if.then3_0, @if.else3
    @if.then3_0
        jmp @for.cont0
    @if.else3
    @if.end3
    @if.cond6_0
        %t7 =w loadsw %i
        %t8 =w ceqw %t7, 5
        jnz %t8, @if.then6_0, @if.else6
    @if.then6_0
        jmp @for.end0
    @if.else6
    @if.end6
        %t9 =w loadsw %sum
        %t10 =w loadsw %i
        %t11 =w add %t9, %t10
        storew %t11, %sum
    @for.cont0
        %t12 =w loadsw %i
        %t13 =w add %t12, 1
        storew %t13, %i
        jmp @for.cond0
    @for.end0
        %t14 =w loadsw %sum
        ret %t14
    }
    |}]

let%expect_test "codegen: break in while" =
  run_codegen
    {|
func f() i32 {
  var i: i32 = 0
  while true {
    if i == 3 { break }
    i += 1
  }
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
        jnz 1, @while.body0, @while.end0
    @while.body0
    @if.cond1_0
        %t2 =w loadsw %i
        %t3 =w ceqw %t2, 3
        jnz %t3, @if.then1_0, @if.else1
    @if.then1_0
        jmp @while.end0
    @if.else1
    @if.end1
        %t4 =w loadsw %i
        %t5 =w add %t4, 1
        storew %t5, %i
        jmp @while.cond0
    @while.end0
        %t6 =w loadsw %i
        ret %t6
    }
    |}]

let%expect_test "codegen: array coerces to slice at call" =
  run_codegen
    {|
func sum(xs: []i32) i32 { return 0 }
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  return sum(a)
}
|};
  [%expect
    {|
    function w $sum(l %t0) {
    @start
        %xs =l alloc8 16
        %t1 =l loadl %t0
        storel %t1, %xs
        %t3 =l add %t0, 8
        %t2 =l loadl %t3
        %t4 =l add %xs, 8
        storel %t2, %t4
        ret 0
    }

    function w $f() {
    @start
        %a =l alloc4 12
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l alloc8 16
        storel %a, %t2
        %t3 =l add %t2, 8
        storel 3, %t3
        %t4 =w call $sum(l %t2)
        ret %t4
    }
    |}]

let%expect_test "codegen: sub-slice construction" =
  run_codegen
    {|
func f() i32 {
  var a: [4]i32 = [1, 2, 3, 4]
  const s: []i32 = a[1..3]
  return s[0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l add %a, 12
        storew 4, %t2
        %s =l alloc8 16
        %t3 =l extsw 1
        %t4 =l extsw 3
        %t5 =l mul %t3, 4
        %t6 =l add %a, %t5
        %t7 =l sub %t4, %t3
        %t8 =l alloc8 16
        storel %t6, %t8
        %t9 =l add %t8, 8
        storel %t7, %t9
        %t10 =l loadl %t8
        storel %t10, %s
        %t12 =l add %t8, 8
        %t11 =l loadl %t12
        %t13 =l add %s, 8
        storel %t11, %t13
        %t14 =l loadl %s
        %t15 =l extsw 0
        %t16 =l mul %t15, 4
        %t17 =l add %t14, %t16
        %t18 =w loadsw %t17
        ret %t18
    }
    |}]

let%expect_test "codegen: inclusive sub-slice construction" =
  run_codegen
    {|
func f() i32 {
  var a: [4]i32 = [1, 2, 3, 4]
  const s: []i32 = a[1..=3]
  return s[0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l add %a, 12
        storew 4, %t2
        %s =l alloc8 16
        %t3 =l extsw 1
        %t4 =w add 3, 1
        %t5 =l extsw %t4
        %t6 =l mul %t3, 4
        %t7 =l add %a, %t6
        %t8 =l sub %t5, %t3
        %t9 =l alloc8 16
        storel %t7, %t9
        %t10 =l add %t9, 8
        storel %t8, %t10
        %t11 =l loadl %t9
        storel %t11, %s
        %t13 =l add %t9, 8
        %t12 =l loadl %t13
        %t14 =l add %s, 8
        storel %t12, %t14
        %t15 =l loadl %s
        %t16 =l extsw 0
        %t17 =l mul %t16, 4
        %t18 =l add %t15, %t17
        %t19 =w loadsw %t18
        ret %t19
    }
    |}]

let%expect_test "codegen: slice len loads from fat pointer" =
  run_codegen
    {|
func f() i64 {
  var a: [3]i32 = [1, 2, 3]
  const s: []i32 = a[0..3]
  return s.len
}
|};
  [%expect
    {|
    function l $f() {
    @start
        %a =l alloc4 12
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %s =l alloc8 16
        %t2 =l extsw 0
        %t3 =l extsw 3
        %t4 =l mul %t2, 4
        %t5 =l add %a, %t4
        %t6 =l sub %t3, %t2
        %t7 =l alloc8 16
        storel %t5, %t7
        %t8 =l add %t7, 8
        storel %t6, %t8
        %t9 =l loadl %t7
        storel %t9, %s
        %t11 =l add %t7, 8
        %t10 =l loadl %t11
        %t12 =l add %s, 8
        storel %t10, %t12
        %t13 =l add %s, 8
        %t14 =l loadl %t13
        ret %t14
    }
    |}]

let%expect_test "codegen: slice param and for iteration" =
  run_codegen
    {|
func sum(xs: []i32) i32 {
  var t: i32 = 0
  for x in xs { t += x }
  return t
}
|};
  [%expect
    {|
    function w $sum(l %t0) {
    @start
        %xs =l alloc8 16
        %t1 =l loadl %t0
        storel %t1, %xs
        %t3 =l add %t0, 8
        %t2 =l loadl %t3
        %t4 =l add %xs, 8
        storel %t2, %t4
        %t =l alloc4 4
        storew 0, %t
        %t6 =l loadl %xs
        %t7 =l add %xs, 8
        %t8 =l loadl %t7
        %for.i5 =l alloc8 8
        storel 0, %for.i5
        %x =l alloc4 4
    @for.cond5
        %t9 =l loadl %for.i5
        %t10 =w csltl %t9, %t8
        jnz %t10, @for.body5, @for.end5
    @for.body5
        %t11 =l mul %t9, 4
        %t12 =l add %t6, %t11
        %t13 =w loadsw %t12
        storew %t13, %x
        %t14 =w loadsw %t
        %t15 =w loadsw %x
        %t16 =w add %t14, %t15
        storew %t16, %t
    @for.cont5
        %t17 =l loadl %for.i5
        %t18 =l add %t17, 1
        storel %t18, %for.i5
        jmp @for.cond5
    @for.end5
        %t19 =w loadsw %t
        ret %t19
    }
    |}]

let%expect_test "codegen: slice element store" =
  run_codegen
    {|
func f() {
  var a: [3]i32 = [1, 2, 3]
  var s: []i32 = a[0..3]
  s[1] = 9
}
|};
  [%expect
    {|
    function $f() {
    @start
        %a =l alloc4 12
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %s =l alloc8 16
        %t2 =l extsw 0
        %t3 =l extsw 3
        %t4 =l mul %t2, 4
        %t5 =l add %a, %t4
        %t6 =l sub %t3, %t2
        %t7 =l alloc8 16
        storel %t5, %t7
        %t8 =l add %t7, 8
        storel %t6, %t8
        %t9 =l loadl %t7
        storel %t9, %s
        %t11 =l add %t7, 8
        %t10 =l loadl %t11
        %t12 =l add %s, 8
        storel %t10, %t12
        %t13 =l loadl %s
        %t14 =l extsw 1
        %t15 =l mul %t14, 4
        %t16 =l add %t13, %t15
        storew 9, %t16
        ret
    }
    |}]

let%expect_test "codegen: compound assign to array element" =
  run_codegen
    {|
func f() i32 {
  var a: [3]i32 = [1, 2, 3]
  a[1] += 5
  return a[1]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 12
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l extsw 1
        %t3 =l mul %t2, 4
        %t4 =l add %a, %t3
        %t5 =w loadsw %t4
        %t6 =w add %t5, 5
        storew %t6, %t4
        %t7 =l extsw 1
        %t8 =l mul %t7, 4
        %t9 =l add %a, %t8
        %t10 =w loadsw %t9
        ret %t10
    }
    |}]

let%expect_test "codegen: multidimensional array literal and index" =
  run_codegen
    {|
func f() i32 {
  var m: [2][2]i32 = [[1, 2], [3, 4]]
  return m[1][0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %m =l alloc4 16
        storew 1, %m
        %t0 =l add %m, 4
        storew 2, %t0
        %t1 =l add %m, 8
        storew 3, %t1
        %t2 =l add %t1, 4
        storew 4, %t2
        %t3 =l extsw 1
        %t4 =l mul %t3, 8
        %t5 =l add %m, %t4
        %t6 =l extsw 0
        %t7 =l mul %t6, 4
        %t8 =l add %t5, %t7
        %t9 =w loadsw %t8
        ret %t9
    }
    |}]

let%expect_test "codegen: global array data" =
  run_codegen {|
var g: [3]i32 = [7, 8, 9]
func f() i32 { return g[1] }
|};
  [%expect
    {|
    data $g = align 4 { w 7, w 8, w 9 }

    function w $f() {
    @start
        %t0 =l extsw 1
        %t1 =l mul %t0, 4
        %t2 =l add $g, %t1
        %t3 =w loadsw %t2
        ret %t3
    }
    |}]

let%expect_test "codegen: iterate array of arrays copies element" =
  run_codegen
    {|
func f() i32 {
  var m: [2][2]i32 = [[1, 2], [3, 4]]
  var s: i32 = 0
  for row in m { s += row[0] }
  return s
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %m =l alloc4 16
        storew 1, %m
        %t0 =l add %m, 4
        storew 2, %t0
        %t1 =l add %m, 8
        storew 3, %t1
        %t2 =l add %t1, 4
        storew 4, %t2
        %s =l alloc4 4
        storew 0, %s
        %for.i3 =l alloc8 8
        storel 0, %for.i3
        %row =l alloc4 8
    @for.cond3
        %t4 =l loadl %for.i3
        %t5 =w csltl %t4, 2
        jnz %t5, @for.body3, @for.end3
    @for.body3
        %t6 =l mul %t4, 8
        %t7 =l add %m, %t6
        %t8 =l loadl %t7
        storel %t8, %row
        %t9 =w loadsw %s
        %t10 =l extsw 0
        %t11 =l mul %t10, 4
        %t12 =l add %row, %t11
        %t13 =w loadsw %t12
        %t14 =w add %t9, %t13
        storew %t14, %s
    @for.cont3
        %t15 =l loadl %for.i3
        %t16 =l add %t15, 1
        storel %t16, %for.i3
        jmp @for.cond3
    @for.end3
        %t17 =w loadsw %s
        ret %t17
    }
    |}]

let%expect_test "codegen: array zero initialization" =
  run_codegen {|
func f() i32 {
  var a: [3]i32
  return a[0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 12
        storel 0, %a
        %t0 =l add %a, 8
        storew 0, %t0
        %t1 =l extsw 0
        %t2 =l mul %t1, 4
        %t3 =l add %a, %t2
        %t4 =w loadsw %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: for over array literal materializes it" =
  run_codegen
    {|
func f() i32 {
  var s: i32 = 0
  for x in [1, 2, 3] { s += x }
  return s
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %s =l alloc4 4
        storew 0, %s
        %t1 =l alloc4 12
        storew 1, %t1
        %t2 =l add %t1, 4
        storew 2, %t2
        %t3 =l add %t1, 8
        storew 3, %t3
        %for.i0 =l alloc8 8
        storel 0, %for.i0
        %x =l alloc4 4
    @for.cond0
        %t4 =l loadl %for.i0
        %t5 =w csltl %t4, 3
        jnz %t5, @for.body0, @for.end0
    @for.body0
        %t6 =l mul %t4, 4
        %t7 =l add %t1, %t6
        %t8 =w loadsw %t7
        storew %t8, %x
        %t9 =w loadsw %s
        %t10 =w loadsw %x
        %t11 =w add %t9, %t10
        storew %t11, %s
    @for.cont0
        %t12 =l loadl %for.i0
        %t13 =l add %t12, 1
        storel %t13, %for.i0
        jmp @for.cond0
    @for.end0
        %t14 =w loadsw %s
        ret %t14
    }
    |}]

let%expect_test "codegen: undefined skips zero-init" =
  run_codegen {|
func f() {
  var a: [4]u8 = undefined
  var b: [4]u8
}
|};
  [%expect {|
    <test>:4:3: warning: 'b' declared but never used
    <test>:3:18: warning: 'a' declared but never used
    function $f() {
    @start
        %a =l alloc4 4
        %b =l alloc4 4
        storew 0, %b
        ret
    }
    |}]
