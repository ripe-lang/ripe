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

let%expect_test
    "codegen: array literal assigned into an element copies its cells" =
  run_codegen
    {|
func main() i32 {
  var m: [2][2]i32 = [[1, 2], [3, 4]]
  m[0] = [9, 8]
  return m[0][0] + m[0][1]
}
|};
  [%expect
    {|
    export function w $main() {
    @start
        %m =l alloc4 16
        storew 1, %m
        %t0 =l add %m, 4
        storew 2, %t0
        %t1 =l add %m, 8
        storew 3, %t1
        %t2 =l add %t1, 4
        storew 4, %t2
        %t3 =l extsw 0
        %t4 =l mul %t3, 8
        %t5 =l add %m, %t4
        storew 9, %t5
        %t6 =l add %t5, 4
        storew 8, %t6
        %t7 =l extsw 0
        %t8 =l mul %t7, 8
        %t9 =l add %m, %t8
        %t10 =l extsw 0
        %t11 =l mul %t10, 4
        %t12 =l add %t9, %t11
        %t13 =w loadsw %t12
        %t14 =l extsw 0
        %t15 =l mul %t14, 8
        %t16 =l add %m, %t15
        %t17 =l extsw 1
        %t18 =l mul %t17, 4
        %t19 =l add %t16, %t18
        %t20 =w loadsw %t19
        %t21 =w add %t13, %t20
        ret %t21
    }
    |}]

let%expect_test
    "codegen: aggregate element assigned from another element copies bytes" =
  run_codegen
    {|
func main() i32 {
  var m: [2][2]i32 = [[1, 2], [3, 4]]
  m[0] = m[1]
  return m[0][0] + m[0][1]
}
|};
  [%expect
    {|
    export function w $main() {
    @start
        %m =l alloc4 16
        storew 1, %m
        %t0 =l add %m, 4
        storew 2, %t0
        %t1 =l add %m, 8
        storew 3, %t1
        %t2 =l add %t1, 4
        storew 4, %t2
        %t3 =l extsw 0
        %t4 =l mul %t3, 8
        %t5 =l add %m, %t4
        %t6 =l extsw 1
        %t7 =l mul %t6, 8
        %t8 =l add %m, %t7
        %t9 =l loadl %t8
        storel %t9, %t5
        %t10 =l extsw 0
        %t11 =l mul %t10, 8
        %t12 =l add %m, %t11
        %t13 =l extsw 0
        %t14 =l mul %t13, 4
        %t15 =l add %t12, %t14
        %t16 =w loadsw %t15
        %t17 =l extsw 0
        %t18 =l mul %t17, 8
        %t19 =l add %m, %t18
        %t20 =l extsw 1
        %t21 =l mul %t20, 4
        %t22 =l add %t19, %t21
        %t23 =w loadsw %t22
        %t24 =w add %t16, %t23
        ret %t24
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
    data $op = align 8 { l $add }

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

    function $f() {
    @start
        %t0 =l loadl $op
        %t1 =w call %t0(w 1, w 2)
        ret
    }
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
        jnz %t3, @if.else2, @if.then2_0
    @if.then2_0
        %t4 =w loadsw %a
        %t5 =w neg %t4
        ret %t5
    @if.else2
    @if.end2
        %t6 =w loadsw %a
        ret %t6
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
        hlt
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

let%expect_test "codegen: unsigned narrowing cast masks" =
  run_codegen "func f() i64 { return (1000 as u8) as i64 }";
  [%expect
    {|
    function l $f() {
    @start
        %t0 =w extub 1000
        %t1 =l extuw %t0
        ret %t1
    }
    |}]

let%expect_test "codegen: signed narrowing cast sign extends" =
  run_codegen "func f(a: i32) i8 { return a as i8 }";
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w extsb %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: u16 narrowing cast masks" =
  run_codegen "func f(a: i32) u16 { return a as u16 }";
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w extuh %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: newtype widening cast keeps its base signedness" =
  run_codegen {|
newtype Id = u16
func f(a: Id) i64 { return a as i64 }
|};
  [%expect
    {|
    function l $f(w %t0) {
    @start
        %a =l alloc4 2
        storeh %t0, %a
        %t1 =w loaduh %a
        %t2 =l extuw %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: global const narrowing cast folds" =
  run_codegen {|
const A: u8 = 1000 as u8
func f() u8 { return A }
|};
  [%expect
    {|
    data $A = align 1 { b 232 }

    function w $f() {
    @start
        %t0 =w loadub $A
        ret %t0
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

let%expect_test "codegen: global cstr init" =
  run_codegen {|
var msg: cstr = "hello"
func f() cstr { return msg }
|};
  [%expect
    {|
    data $msg = align 8 { l $str.0 }

    function l $f() {
    @start
        %t0 =l loadl $msg
        ret %t0
    }

    data $str.0 = { b "hello", b 0 }
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
func f() usize {
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
        %t2 =l alloc8 16
        %a =l alloc4 12
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
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
        %t8 =l alloc8 16
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
        %t9 =l alloc8 16
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
func f() usize {
  var a: [3]i32 = [1, 2, 3]
  const s: []i32 = a[0..3]
  return s.len
}
|};
  [%expect
    {|
    function l $f() {
    @start
        %t7 =l alloc8 16
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
        %t7 =l alloc8 16
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
        %t1 =l alloc4 12
        %s =l alloc4 4
        storew 0, %s
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
  [%expect
    {|
    function $f() {
    @start
        %a =l alloc4 4
        %b =l alloc4 4
        storew 0, %b
        ret
    }
    |}]

(* every branch returns, so the merge block is empty and needs a terminator *)
let%expect_test "codegen: if where all paths return" =
  run_codegen
    {|
func sign(n: i32) i32 {
  if n < 0 { return -1 }
  elseif n > 0 { return 1 }
  else { return 0 }
}
|};
  [%expect
    {|
    function w $sign(w %t0) {
    @start
        %n =l alloc4 4
        storew %t0, %n
    @if.cond1_0
        %t2 =w loadsw %n
        %t3 =w csltw %t2, 0
        jnz %t3, @if.then1_0, @if.cond1_1
    @if.then1_0
        ret -1
    @if.cond1_1
        %t4 =w loadsw %n
        %t5 =w csgtw %t4, 0
        jnz %t5, @if.then1_1, @if.else1
    @if.then1_1
        ret 1
    @if.else1
        ret 0
    @if.end1
        hlt
    }
    |}]

let%expect_test "codegen: struct zero initialization covers full size" =
  run_codegen
    {|
struct pt { x: i64, y: i64, z: i32 }
func f() i64 {
  var p: pt
  return p.x
}
|};
  [%expect
    {|
    type :pt = { l, l, w }

    function l $f() {
    @start
        %p =l alloc8 24
        storel 0, %p
        %t0 =l add %p, 8
        storel 0, %t0
        %t1 =l add %p, 16
        storel 0, %t1
        %t2 =l loadl %p
        ret %t2
    }
    |}]

let%expect_test "codegen: struct param field read" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f(p: pt) i32 { return p.y }
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f(l %t0) {
    @start
        %p =l alloc8 8
        %t1 =l loadl %t0
        storel %t1, %p
        %t2 =l add %p, 4
        %t3 =w loadsw %t2
        ret %t3
    }
    |}]

let%expect_test "codegen: struct var init copies bytes" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f(a: pt) i32 {
  var b: pt = a
  return b.x
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f(l %t0) {
    @start
        %a =l alloc8 8
        %t1 =l loadl %t0
        storel %t1, %a
        %b =l alloc8 8
        %t2 =l loadl %a
        storel %t2, %b
        %t3 =w loadsw %b
        ret %t3
    }
    |}]

let%expect_test "codegen: array field of struct indexes in place" =
  run_codegen
    {|
struct buf { data: [4]i32, n: i32 }
func f() i32 {
  var b: buf
  return b.data[2]
}
|};
  [%expect
    {|
    type :buf = { w 4, w }

    function w $f() {
    @start
        %b =l alloc8 20
        storel 0, %b
        %t0 =l add %b, 8
        storel 0, %t0
        %t1 =l add %b, 16
        storew 0, %t1
        %t2 =l extsw 2
        %t3 =l mul %t2, 4
        %t4 =l add %b, %t3
        %t5 =w loadsw %t4
        ret %t5
    }
    |}]

let%expect_test "codegen: struct literal" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  const p = pt { x: 3, y: 4 }
  return p.y
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 3, %p
        %t0 =l add %p, 4
        storew 4, %t0
        %t1 =l add %p, 4
        %t2 =w loadsw %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: partial struct literal zeroes omitted fields" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  const p = pt { x: 3 }
  return p.y
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 3, %p
        %t0 =l add %p, 4
        storew 0, %t0
        %t1 =l add %p, 4
        %t2 =w loadsw %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: nested struct literal" =
  run_codegen
    {|
struct inner { a: i32 }
struct outer { i: inner, b: i32 }
func f() i32 {
  const o = outer { i: inner { a: 1 }, b: 2 }
  return o.i.a + o.b
}
|};
  [%expect
    {|
    type :inner = { w }
    type :outer = { :inner, w }

    function w $f() {
    @start
        %o =l alloc8 8
        storew 1, %o
        %t0 =l add %o, 4
        storew 2, %t0
        %t1 =w loadsw %o
        %t2 =l add %o, 4
        %t3 =w loadsw %t2
        %t4 =w add %t1, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: struct literal as value" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  return (pt { x: 1, y: 2 }).y
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %t0 =l alloc8 8
        storew 1, %t0
        %t1 =l add %t0, 4
        storew 2, %t1
        %t2 =l add %t0, 4
        %t3 =w loadsw %t2
        ret %t3
    }
    |}]

let%expect_test "codegen: const global struct data with padding" =
  run_codegen
    {|
struct mix { a: i8, b: i64 }
const g: mix = mix { a: 1, b: 2 }
func f() i64 { return g.b }
|};
  [%expect
    {|
    type :mix = { b, l }

    data $g = align 8 { b 1, z 7, l 2 }

    function l $f() {
    @start
        %t0 =l add $g, 8
        %t1 =l loadl %t0
        ret %t1
    }
    |}]

let%expect_test "codegen: nested block shadow keeps outer slot" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 1
  {
    var x: i32 = 2
    x = x + 5
  }
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 1, %x
        %x.2 =l alloc4 4
        storew 2, %x.2
        %t0 =w loadsw %x.2
        %t1 =w add %t0, 5
        storew %t1, %x.2
        %t2 =w loadsw %x
        ret %t2
    }
    |}]

let%expect_test "codegen: same scope redeclare shadows and reads outer" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 1
  var x: i32 = x + 4
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 1, %x
        %x.2 =l alloc4 4
        %t0 =w loadsw %x
        %t1 =w add %t0, 4
        storew %t1, %x.2
        %t2 =w loadsw %x.2
        ret %t2
    }
    |}]

let%expect_test
    "codegen: if-branch shadow does not leak, later global write hits global" =
  run_codegen
    {|
var g: i32 = 0
func f(c: bool) {
  if c {
    var g: i32 = 5
    g = g + 1
  }
  g = 10
}
|};
  [%expect
    {|
    data $g = align 4 { w 0 }

    function $f(w %t0) {
    @start
        %c =l alloc4 1
        storeb %t0, %c
    @if.cond1_0
        %t2 =w loadub %c
        jnz %t2, @if.then1_0, @if.else1
    @if.then1_0
        %g =l alloc4 4
        storew 5, %g
        %t3 =w loadsw %g
        %t4 =w add %t3, 1
        storew %t4, %g
        jmp @if.end1
    @if.else1
    @if.end1
        storew 10, $g
        ret
    }
    |}]

let%expect_test "codegen: local shadows global inside function" =
  run_codegen
    {|
var g: i32 = 0
func f() i32 {
  var g: i32 = 7
  g = g + 1
  return g
}
|};
  [%expect
    {|
    data $g = align 4 { w 0 }

    function w $f() {
    @start
        %g =l alloc4 4
        storew 7, %g
        %t0 =w loadsw %g
        %t1 =w add %t0, 1
        storew %t1, %g
        %t2 =w loadsw %g
        ret %t2
    }
    |}]

let%expect_test "codegen: for loop var shadows outer, outer intact after loop" =
  run_codegen
    {|
func f() i32 {
  var i: i32 = 99
  for i in 0..3 {
    var y: i32 = i
  }
  return i
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %i =l alloc4 4
        storew 99, %i
        %i.2 =l alloc4 4
        storew 0, %i.2
    @for.cond0
        %t1 =w loadsw %i.2
        %t2 =w csltw %t1, 3
        jnz %t2, @for.body0, @for.end0
    @for.body0
        %y =l alloc4 4
        %t3 =w loadsw %i.2
        storew %t3, %y
    @for.cont0
        %t4 =w loadsw %i.2
        %t5 =w add %t4, 1
        storew %t5, %i.2
        jmp @for.cond0
    @for.end0
        %t6 =w loadsw %i
        ret %t6
    }
    |}]

let%expect_test "codegen: while body shadow" =
  run_codegen
    {|
func f(n: i32) i32 {
  var x: i32 = 1
  while x < n {
    var x: i32 = 100
    x = x + 1
  }
  return x
}
|};
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %n =l alloc4 4
        storew %t0, %n
        %x =l alloc4 4
        storew 1, %x
    @while.cond1
        %t2 =w loadsw %x
        %t3 =w loadsw %n
        %t4 =w csltw %t2, %t3
        jnz %t4, @while.body1, @while.end1
    @while.body1
        %x.3 =l alloc4 4
        storew 100, %x.3
        %t5 =w loadsw %x.3
        %t6 =w add %t5, 1
        storew %t6, %x.3
        jmp @while.cond1
    @while.end1
        %t7 =w loadsw %x
        ret %t7
    }
    |}]

let%expect_test "codegen: type changing shadow" =
  run_codegen
    {|
func f() i64 {
  var x: i32 = 1
  var x: i64 = 2
  return x
}
|};
  [%expect
    {|
    function l $f() {
    @start
        %x =l alloc4 4
        storew 1, %x
        %x.2 =l alloc8 8
        storel 2, %x.2
        %t0 =l loadl %x.2
        ret %t0
    }
    |}]

let%expect_test "codegen: struct local shadow uses distinct slot" =
  run_codegen
    {|
struct P { x: i32, y: i32 }
func f() i32 {
  var p: P = P { x: 1, y: 2 }
  {
    var p: P = P { x: 3, y: 4 }
  }
  return p.x
}
|};
  [%expect
    {|
    type :P = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 1, %p
        %t0 =l add %p, 4
        storew 2, %t0
        %p.2 =l alloc8 8
        storew 3, %p.2
        %t1 =l add %p.2, 4
        storew 4, %t1
        %t2 =w loadsw %p
        ret %t2
    }
    |}]

let%expect_test "codegen: triple nested shadow gets distinct slots" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 1
  {
    var x: i32 = 2
    {
      var x: i32 = 3
      x = x + 1
    }
    x = x + 1
  }
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 1, %x
        %x.2 =l alloc4 4
        storew 2, %x.2
        %x.3 =l alloc4 4
        storew 3, %x.3
        %t0 =w loadsw %x.3
        %t1 =w add %t0, 1
        storew %t1, %x.3
        %t2 =w loadsw %x.2
        %t3 =w add %t2, 1
        storew %t3, %x.2
        %t4 =w loadsw %x
        ret %t4
    }
    |}]

let%expect_test "codegen: compound assign on shadowed local" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 1
  {
    var x: i32 = 10
    x += 5
  }
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 1, %x
        %x.2 =l alloc4 4
        storew 10, %x.2
        %t0 =w loadsw %x.2
        %t1 =w add %t0, 5
        storew %t1, %x.2
        %t2 =w loadsw %x
        ret %t2
    }
    |}]

let%expect_test "codegen: && short circuits into branches" =
  run_codegen
    {|
func check(p: *i32) i32 {
  if p != null && *p == 3 { return 1 }
  return 0
}
|};
  [%expect
    {|
    function w $check(l %t0) {
    @start
        %p =l alloc8 8
        storel %t0, %p
    @if.cond1_0
        %t3 =l loadl %p
        %t4 =w cnel %t3, 0
        jnz %t4, @and.rhs2, @if.else1
    @and.rhs2
        %t5 =l loadl %p
        %t6 =w loadsw %t5
        %t7 =w ceqw %t6, 3
        jnz %t7, @if.then1_0, @if.else1
    @if.then1_0
        ret 1
    @if.else1
    @if.end1
        ret 0
    }
    |}]

let%expect_test "codegen: || short circuits into branches" =
  run_codegen {|
func any(a: bool, b: bool) bool {
  return a || b
}
|};
  [%expect
    {|
    function w $any(w %t0, w %t1) {
    @start
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
        %t4 =w loadub %a
        jnz %t4, @bool.true2, @or.rhs3
    @or.rhs3
        %t5 =w loadub %b
        jnz %t5, @bool.true2, @bool.false2
    @bool.true2
        jmp @bool.join2
    @bool.false2
        jmp @bool.join2
    @bool.join2
        %t6 =w phi @bool.true2 1, @bool.false2 0
        ret %t6
    }
    |}]

let%expect_test "codegen: chained && threads through midpoints" =
  run_codegen
    {|
func f(a: bool, b: bool, c: bool) i32 {
  if a && b && c { return 1 }
  return 0
}
|};
  [%expect
    {|
    function w $f(w %t0, w %t1, w %t2) {
    @start
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
        %c =l alloc4 1
        storeb %t2, %c
    @if.cond3_0
        %t6 =w loadub %a
        jnz %t6, @and.rhs5, @if.else3
    @and.rhs5
        %t7 =w loadub %b
        jnz %t7, @and.rhs4, @if.else3
    @and.rhs4
        %t8 =w loadub %c
        jnz %t8, @if.then3_0, @if.else3
    @if.then3_0
        ret 1
    @if.else3
    @if.end3
        ret 0
    }
    |}]

let%expect_test "codegen: mixed || and && respects precedence" =
  run_codegen
    {|
func f(a: bool, b: bool, c: bool) bool {
  return a || b && c
}
|};
  [%expect
    {|
    function w $f(w %t0, w %t1, w %t2) {
    @start
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
        %c =l alloc4 1
        storeb %t2, %c
        %t5 =w loadub %a
        jnz %t5, @bool.true3, @or.rhs4
    @or.rhs4
        %t7 =w loadub %b
        jnz %t7, @and.rhs6, @bool.false3
    @and.rhs6
        %t8 =w loadub %c
        jnz %t8, @bool.true3, @bool.false3
    @bool.true3
        jmp @bool.join3
    @bool.false3
        jmp @bool.join3
    @bool.join3
        %t9 =w phi @bool.true3 1, @bool.false3 0
        ret %t9
    }
    |}]

let%expect_test "codegen: ! in a condition swaps branch targets" =
  run_codegen
    {|
func f(a: bool, b: bool) i32 {
  if !a && b { return 1 }
  return 0
}
|};
  [%expect
    {|
    function w $f(w %t0, w %t1) {
    @start
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
    @if.cond2_0
        %t4 =w loadub %a
        jnz %t4, @if.else2, @and.rhs3
    @and.rhs3
        %t5 =w loadub %b
        jnz %t5, @if.then2_0, @if.else2
    @if.then2_0
        ret 1
    @if.else2
    @if.end2
        ret 0
    }
    |}]

let%expect_test "codegen: && drives a while condition" =
  run_codegen {|
func f(a: bool, b: bool) {
  while a && b { return }
}
|};
  [%expect
    {|
    function $f(w %t0, w %t1) {
    @start
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
    @while.cond2
        %t4 =w loadub %a
        jnz %t4, @and.rhs3, @while.end2
    @and.rhs3
        %t5 =w loadub %b
        jnz %t5, @while.body2, @while.end2
    @while.body2
        ret
    @while.end2
        ret
    }
    |}]

let%expect_test "codegen: short circuit result stored in a local" =
  run_codegen
    {|
func f(a: bool, b: bool) i32 {
  var ok: bool = a && b
  if ok { return 1 }
  return 0
}
|};
  [%expect
    {|
    function w $f(w %t0, w %t1) {
    @start
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
        %ok =l alloc4 1
        %t4 =w loadub %a
        jnz %t4, @and.rhs3, @bool.false2
    @and.rhs3
        %t5 =w loadub %b
        jnz %t5, @bool.true2, @bool.false2
    @bool.true2
        jmp @bool.join2
    @bool.false2
        jmp @bool.join2
    @bool.join2
        %t6 =w phi @bool.true2 1, @bool.false2 0
        storeb %t6, %ok
    @if.cond7_0
        %t8 =w loadub %ok
        jnz %t8, @if.then7_0, @if.else7
    @if.then7_0
        ret 1
    @if.else7
    @if.end7
        ret 0
    }
    |}]

let%expect_test "codegen: const references another const by value" =
  run_codegen {|
const A: i32 = 5
const B: i32 = A
func f() i32 { return B }
|};
  [%expect
    {|
    data $A = align 4 { w 5 }
    data $B = align 4 { w 5 }

    function w $f() {
    @start
        %t0 =w loadsw $B
        ret %t0
    }
    |}]

let%expect_test "codegen: const references a later const" =
  run_codegen {|
const B: i32 = A
const A: i32 = 5
func f() i32 { return B }
|};
  [%expect
    {|
    data $B = align 4 { w 5 }
    data $A = align 4 { w 5 }

    function w $f() {
    @start
        %t0 =w loadsw $B
        ret %t0
    }
    |}]

let%expect_test "codegen: var initialized from a const" =
  run_codegen {|
const A: i32 = 5
var B: i32 = A
func f() i32 { return B }
|};
  [%expect
    {|
    data $A = align 4 { w 5 }
    data $B = align 4 { w 5 }

    function w $f() {
    @start
        %t0 =w loadsw $B
        ret %t0
    }
    |}]

let%expect_test "codegen: self referential const is a cycle" =
  run_codegen {|
const X: i32 = X
func f() i32 { return X }
|};
  [%expect
    {|
    error: cyclic constant: X
      at <test>:2:16
        const X: i32 = X
                       ^
    |}]

let%expect_test "codegen: mutually referential consts are a cycle" =
  run_codegen {|
const A: i32 = B
const B: i32 = A
func f() i32 { return A }
|};
  [%expect
    {|
    error: cyclic constant: B
      at <test>:2:16
        const A: i32 = B
                       ^
    |}]

let%expect_test "codegen: global const arithmetic" =
  run_codegen {|
const A: i32 = 2 + 3
func f() i32 { return A }
|};
  [%expect
    {|
    data $A = align 4 { w 5 }

    function w $f() {
    @start
        %t0 =w loadsw $A
        ret %t0
    }
    |}]

let%expect_test "codegen: global const arithmetic referencing another const" =
  run_codegen
    {|
const A: i32 = 5
const B: i32 = A * 2 + 1
func f() i32 { return B }
|};
  [%expect
    {|
    data $A = align 4 { w 5 }
    data $B = align 4 { w 11 }

    function w $f() {
    @start
        %t0 =w loadsw $B
        ret %t0
    }
    |}]

let%expect_test "codegen: global const float arithmetic" =
  run_codegen {|
const A: f64 = 1.5 + 2.5
func f() f64 { return A }
|};
  [%expect
    {|
    data $A = align 8 { d d_4 }

    function d $f() {
    @start
        %t0 =d loadd $A
        ret %t0
    }
    |}]

let%expect_test "codegen: global const cast arithmetic" =
  run_codegen {|
const A: f32 = (1 + 2) as f32
func f() f32 { return A }
|};
  [%expect
    {|
    data $A = align 4 { s s_3 }

    function s $f() {
    @start
        %t0 =s loads $A
        ret %t0
    }
    |}]

let%expect_test "codegen: assign through struct pointer deref" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  var p: pt = pt { x: 34, y: 56 }
  var q: *pt = &p
  *q.x = 8
  return p.x
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 34, %p
        %t0 =l add %p, 4
        storew 56, %t0
        %q =l alloc8 8
        %t1 =l copy %p
        storel %t1, %q
        %t2 =l loadl %q
        storew 8, %t2
        %t3 =w loadsw %p
        ret %t3
    }
    |}]

let%expect_test "codegen: assign through scalar pointer deref" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 10
  var q: *i32 = &x
  *q = 42
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 10, %x
        %q =l alloc8 8
        %t0 =l copy %x
        storel %t0, %q
        %t1 =l loadl %q
        storew 42, %t1
        %t2 =w loadsw %x
        ret %t2
    }
    |}]

let%expect_test "codegen: assign to struct field directly" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  var p: pt = pt { x: 1, y: 2 }
  p.x = 7
  return p.x
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 1, %p
        %t0 =l add %p, 4
        storew 2, %t0
        storew 7, %p
        %t1 =w loadsw %p
        ret %t1
    }
    |}]

let%expect_test "codegen: compound assign to struct field" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  var p: pt = pt { x: 1, y: 2 }
  p.x += 5
  return p.x
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 1, %p
        %t0 =l add %p, 4
        storew 2, %t0
        %t1 =w loadsw %p
        %t2 =w add %t1, 5
        storew %t2, %p
        %t3 =w loadsw %p
        ret %t3
    }
    |}]

let%expect_test "codegen: compound assign through scalar pointer deref" =
  run_codegen
    {|
func f() i32 {
  var x: i32 = 10
  var q: *i32 = &x
  *q += 5
  return x
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 10, %x
        %q =l alloc8 8
        %t0 =l copy %x
        storel %t0, %q
        %t1 =l loadl %q
        %t2 =w loadsw %t1
        %t3 =w add %t2, 5
        storew %t3, %t1
        %t4 =w loadsw %x
        ret %t4
    }
    |}]

let%expect_test "codegen: address-of struct field" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  var p: pt = pt { x: 7, y: 0 }
  var px: *i32 = &(p.x)
  return *px
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 7, %p
        %t0 =l add %p, 4
        storew 0, %t0
        %px =l alloc8 8
        %t1 =l copy %p
        storel %t1, %px
        %t2 =l loadl %px
        %t3 =w loadsw %t2
        ret %t3
    }
    |}]

let%expect_test "codegen: address-of deref" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  var p: pt = pt { x: 3, y: 0 }
  var pt1: *pt = &p
  var pt2: *pt = &(*pt1)
  return pt2.x
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 3, %p
        %t0 =l add %p, 4
        storew 0, %t0
        %pt1 =l alloc8 8
        %t1 =l copy %p
        storel %t1, %pt1
        %pt2 =l alloc8 8
        %t2 =l loadl %pt1
        %t3 =l copy %t2
        storel %t3, %pt2
        %t4 =l loadl %pt2
        %t5 =w loadsw %t4
        ret %t5
    }
    |}]

let%expect_test "codegen: address-of array index" =
  run_codegen
    {|
func f() i32 {
  var arr: [4]i32 = [1, 2, 3, 4]
  var p: *i32 = &(arr[2])
  return *p
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %arr =l alloc4 16
        storew 1, %arr
        %t0 =l add %arr, 4
        storew 2, %t0
        %t1 =l add %arr, 8
        storew 3, %t1
        %t2 =l add %arr, 12
        storew 4, %t2
        %p =l alloc8 8
        %t3 =l extsw 2
        %t4 =l mul %t3, 4
        %t5 =l add %arr, %t4
        %t6 =l copy %t5
        storel %t6, %p
        %t7 =l loadl %p
        %t8 =w loadsw %t7
        ret %t8
    }
    |}]

let%expect_test "codegen: bitwise or" =
  run_codegen "func f(a: i32, b: i32) i32 { return a | b }";
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
        %t4 =w or %t2, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: bitwise xor" =
  run_codegen "func f(a: i32, b: i32) i32 { return a ^ b }";
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
        %t4 =w xor %t2, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: right shift on signed int" =
  run_codegen "func f(a: i32, b: i32) i32 { return a >> b }";
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
        %t4 =w sar %t2, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: right shift on unsigned int" =
  run_codegen "func f(a: u32, b: u32) u32 { return a >> b }";
  [%expect
    {|
    function w $f(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
        %t2 =w loaduw %a
        %t3 =w loaduw %b
        %t4 =w shr %t2, %t3
        ret %t4
    }
    |}]

let%expect_test "codegen: cast int to float" =
  run_codegen "func f(a: i32) f64 { return a as f64 }";
  [%expect
    {|
    function d $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =d swtof %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: cast float to int" =
  run_codegen "func f(a: f64) i32 { return a as i32 }";
  [%expect
    {|
    function w $f(d %t0) {
    @start
        %a =l alloc8 8
        stored %t0, %a
        %t1 =d loadd %a
        %t2 =w dtosi %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: elseif chain" =
  run_codegen
    {|
func f(x: i32) i32 {
  if x < 0 { return 0 }
  elseif x == 0 { return 1 }
  else { return 2 }
}
|};
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %x =l alloc4 4
        storew %t0, %x
    @if.cond1_0
        %t2 =w loadsw %x
        %t3 =w csltw %t2, 0
        jnz %t3, @if.then1_0, @if.cond1_1
    @if.then1_0
        ret 0
    @if.cond1_1
        %t4 =w loadsw %x
        %t5 =w ceqw %t4, 0
        jnz %t5, @if.then1_1, @if.else1
    @if.then1_1
        ret 1
    @if.else1
        ret 2
    @if.end1
        hlt
    }
    |}]

let%expect_test "codegen: function call as argument to another call" =
  run_codegen
    {|
func inc(x: i32) i32 { return x + 1 }
func add(a: i32, b: i32) i32 { return a + b }
func f() i32 { return add(inc(1), inc(2)) }
|};
  [%expect
    {|
    function w $inc(w %t0) {
    @start
        %x =l alloc4 4
        storew %t0, %x
        %t1 =w loadsw %x
        %t2 =w add %t1, 1
        ret %t2
    }

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
        %t0 =w call $inc(w 1)
        %t1 =w call $inc(w 2)
        %t2 =w call $add(w %t0, w %t1)
        ret %t2
    }
    |}]

let%expect_test "codegen: compound multiply assign on local" =
  run_codegen {|
func f() {
  var n: i32 = 3
  n *= 4
}
|};
  [%expect
    {|
    function $f() {
    @start
        %n =l alloc4 4
        storew 3, %n
        %t0 =w loadsw %n
        %t1 =w mul %t0, 4
        storew %t1, %n
        ret
    }
    |}]

let%expect_test "codegen: compound divide assign on local" =
  run_codegen {|
func f() {
  var n: i32 = 12
  n /= 4
}
|};
  [%expect
    {|
    function $f() {
    @start
        %n =l alloc4 4
        storew 12, %n
        %t0 =w loadsw %n
        %t1 =w div %t0, 4
        storew %t1, %n
        ret
    }
    |}]

let%expect_test "codegen: struct passed by value to function" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func read_x(p: pt) i32 { return p.x }
func f() i32 {
  var p: pt = pt { x: 5, y: 6 }
  return read_x(p)
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $read_x(l %t0) {
    @start
        %p =l alloc8 8
        %t1 =l loadl %t0
        storel %t1, %p
        %t2 =w loadsw %p
        ret %t2
    }

    function w $f() {
    @start
        %p =l alloc8 8
        storew 5, %p
        %t0 =l add %p, 4
        storew 6, %t0
        %t1 =w call $read_x(l %p)
        ret %t1
    }
    |}]

let%expect_test "codegen: void function with early return" =
  run_codegen
    {|
func f(x: i32) {
  if x < 0 { return }
  var y: i32 = x + 1
}
|};
  [%expect
    {|
    function $f(w %t0) {
    @start
        %x =l alloc4 4
        storew %t0, %x
    @if.cond1_0
        %t2 =w loadsw %x
        %t3 =w csltw %t2, 0
        jnz %t3, @if.then1_0, @if.else1
    @if.then1_0
        ret
    @if.else1
    @if.end1
        %y =l alloc4 4
        %t4 =w loadsw %x
        %t5 =w add %t4, 1
        storew %t5, %y
        ret
    }
    |}]

let%expect_test "codegen: division by zero in constant" =
  run_codegen {|const X: i32 = 1 / 0|};
  [%expect
    {|
    error: division by zero in constant
      at <test>:1:16
        const X: i32 = 1 / 0
                       ^~~~~
    |}]

let%expect_test "codegen: remainder by zero in constant" =
  run_codegen {|const X: i32 = 1 % 0|};
  [%expect
    {|
    error: remainder by zero in constant
      at <test>:1:16
        const X: i32 = 1 % 0
                       ^~~~~
    |}]

let%expect_test "codegen: string interpolation" =
  run_codegen {|
func f() {
  var x: i32 = 1
  var s: cstr = "a{x}"
}
|};
  [%expect
    {|
    error: string interpolation is not yet supported
      at <test>:4:19
          var s: cstr = "a{x}"
                          ^~~~
    |}]

(* internal invariants: the typechecker keeps these unreachable from source, so
   drive the codegen helpers directly *)

let empty_structs () : (string, (string * Ripe.Types.ty) list) Hashtbl.t =
  Hashtbl.create 0

let%expect_test "codegen ICE: TVoid has no QBE base type" =
  expect_errors (fun () -> ignore (Ripe.Codegen.qbe_ty Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no QBE base type
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no alignment" =
  expect_errors (fun () ->
      ignore (Ripe.Codegen.ty_align (empty_structs ()) Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no alignment
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no size" =
  expect_errors (fun () ->
      ignore (Ripe.Codegen.ty_size (empty_structs ()) Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no size
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no alloc instruction" =
  expect_errors (fun () -> ignore (Ripe.Codegen.alloc_instr Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no alloc instruction
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no load instruction" =
  expect_errors (fun () -> ignore (Ripe.Codegen.qbe_load Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no load instruction
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no store instruction" =
  expect_errors (fun () -> ignore (Ripe.Codegen.qbe_store Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no store instruction
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no extended type" =
  expect_errors (fun () -> ignore (Ripe.Codegen.qbe_ext_ty Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no extended type
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: missing struct layout in alignment" =
  expect_errors (fun () ->
      ignore
        (Ripe.Codegen.ty_align (empty_structs ()) (Ripe.Types.TStruct "Foo")));
  [%expect
    {|
    error: internal compiler error
    no layout recorded for struct Foo
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: missing struct layout in size" =
  expect_errors (fun () ->
      ignore
        (Ripe.Codegen.ty_size (empty_structs ()) (Ripe.Types.TStruct "Foo")));
  [%expect
    {|
    error: internal compiler error
    no layout recorded for struct Foo
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen: assignment operator in constant" =
  expect_errors (fun () ->
      ignore
        (Ripe.Codegen.fold_const_binop Ripe.Ast.dummy_span Ripe.Ast.Assign
           ~result_ty:(Ripe.Types.TInt Ripe.Types.I32)
           ~operand_ty:(Ripe.Types.TInt Ripe.Types.I32) (Ripe.Codegen.Ni32 1l)
           (Ripe.Codegen.Ni32 2l)));
  [%expect
    {|
    error: unsupported constant expression
      at <test>:1:1

        ^
    help: constant initializers must fold to a compile-time value
    |}]

let%expect_test "codegen: unsupported float operation in constant" =
  expect_errors (fun () ->
      ignore
        (Ripe.Codegen.fold_const_binop Ripe.Ast.dummy_span Ripe.Ast.Lshift
           ~result_ty:(Ripe.Types.TFloat Ripe.Types.F64)
           ~operand_ty:(Ripe.Types.TFloat Ripe.Types.F64) (Ripe.Codegen.Nf 1.0)
           (Ripe.Codegen.Nf 2.0)));
  [%expect
    {|
    error: unsupported constant expression
      at <test>:1:1

        ^
    help: constant initializers must fold to a compile-time value
    |}]

let%expect_test
    "codegen: variadic call emits ... marker between fixed and variadic args" =
  run_codegen
    {|
extern func printf(fmt: cstr, ...) i32
func main() {
  printf("pi = %f\n", 3.14)
}
|};
  [%expect
    {|
    export function w $main() {
    @start
        %t0 =w call $printf(l $str.0, ..., d d_3.1400000000000001)
        ret 0
    }

    data $str.0 = { b "pi = %f\n", b 0 }
    |}]

let%expect_test "codegen: binder spelled like a temp gets a suffix" =
  run_codegen {|
func main() i32 {
  var t0: i32 = 5
  t0 = 9
  return t0
}
|};
  [%expect
    {|
    export function w $main() {
    @start
        %t0.1 =l alloc4 4
        storew 5, %t0.1
        storew 9, %t0.1
        %t0 =w loadsw %t0.1
        ret %t0
    }
    |}]

let%expect_test "codegen: global sharing a string label name" =
  run_codegen
    {|
var str0: i32 = 7
func main() i32 {
  const _s: cstr = "hi"
  return str0
}
|};
  [%expect
    {|
    data $str0 = align 4 { w 7 }

    export function w $main() {
    @start
        %_s =l alloc8 8
        storel $str.0, %_s
        %t0 =w loadsw $str0
        ret %t0
    }

    data $str.0 = { b "hi", b 0 }
    |}]

let%expect_test "codegen: sizeof struct" =
  run_codegen
    {|
struct S { a: i32, b: i32 }
func f() i64 { return sizeof(S) as i64 }
|};
  [%expect
    {|
    type :S = { w, w }

    function l $f() {
    @start
        %t0 =l copy 8
        ret %t0
    }
    |}]

let%expect_test "codegen: sizeof array" =
  run_codegen "func f() i64 { return sizeof([4]i32) as i64 }";
  [%expect
    {|
    function l $f() {
    @start
        %t0 =l copy 16
        ret %t0
    }
    |}]

let%expect_test "codegen: newtype local round-trips through its base" =
  run_codegen
    {|
newtype Id = i32
func main() i32 {
  var a: Id = 5 as Id
  return a as i32
}
|};
  [%expect
    {|
    export function w $main() {
    @start
        %a =l alloc4 4
        %t0 =w copy 5
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w copy %t1
        ret %t2
    }
    |}]

let%expect_test "codegen: call result through a local fn ptr stored" =
  run_codegen
    {|
func add(a: i32, b: i32) i32 { return a + b }
func main() i32 {
  var op = add
  var r = op(2, 3)
  return r
}
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

    export function w $main() {
    @start
        %op =l alloc8 8
        storel $add, %op
        %r =l alloc4 4
        %t0 =l loadl %op
        %t1 =w call %t0(w 2, w 3)
        storew %t1, %r
        %t2 =w loadsw %r
        ret %t2
    }
    |}]
