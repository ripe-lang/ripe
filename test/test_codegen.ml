(* SPDX-License-Identifier: GPL-2.0-only *)

open Diag
open Pipeline

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
        %compound.p.0 =l alloc8 8
        %t0 =l copy $n
        storel %t0, %compound.p.0
        %t1 =l loadl %compound.p.0
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        %t4 =l loadl %compound.p.0
        %t6 =w ceql %t4, 0
        jnz %t6, @null.fail.7, @null.ok.7
    @null.fail.7
        call $ripe_panic_null()
        hlt
    @null.ok.7
        %t5 =w loadsw %t4
        %t8 =w add %t5, 1
        storew %t8, %t1
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
        %compound.p.0 =l alloc8 8
        storew 0, %n
        %t0 =l copy %n
        storel %t0, %compound.p.0
        %t1 =l loadl %compound.p.0
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        %t4 =l loadl %compound.p.0
        %t6 =w ceql %t4, 0
        jnz %t6, @null.fail.7, @null.ok.7
    @null.fail.7
        call $ripe_panic_null()
        hlt
    @null.ok.7
        %t5 =w loadsw %t4
        %t8 =w add %t5, 1
        storew %t8, %t1
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
        %t4 =w cugel %t3, 2
        jnz %t4, @bounds.fail.5, @bounds.ok.5
    @bounds.fail.5
        call $ripe_panic_bounds(l %t3, l 2)
        hlt
    @bounds.ok.5
        %t6 =l mul %t3, 8
        %t7 =l add %m, %t6
        storew 9, %t7
        %t8 =l add %t7, 4
        storew 8, %t8
        %t9 =l extsw 0
        %t10 =w cugel %t9, 2
        jnz %t10, @bounds.fail.11, @bounds.ok.11
    @bounds.fail.11
        call $ripe_panic_bounds(l %t9, l 2)
        hlt
    @bounds.ok.11
        %t12 =l mul %t9, 8
        %t13 =l add %m, %t12
        %t14 =l extsw 0
        %t15 =w cugel %t14, 2
        jnz %t15, @bounds.fail.16, @bounds.ok.16
    @bounds.fail.16
        call $ripe_panic_bounds(l %t14, l 2)
        hlt
    @bounds.ok.16
        %t17 =l mul %t14, 4
        %t18 =l add %t13, %t17
        %t19 =w loadsw %t18
        %t20 =l extsw 0
        %t21 =w cugel %t20, 2
        jnz %t21, @bounds.fail.22, @bounds.ok.22
    @bounds.fail.22
        call $ripe_panic_bounds(l %t20, l 2)
        hlt
    @bounds.ok.22
        %t23 =l mul %t20, 8
        %t24 =l add %m, %t23
        %t25 =l extsw 1
        %t26 =w cugel %t25, 2
        jnz %t26, @bounds.fail.27, @bounds.ok.27
    @bounds.fail.27
        call $ripe_panic_bounds(l %t25, l 2)
        hlt
    @bounds.ok.27
        %t28 =l mul %t25, 4
        %t29 =l add %t24, %t28
        %t30 =w loadsw %t29
        %t31 =w add %t19, %t30
        ret %t31
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
        %t4 =w cugel %t3, 2
        jnz %t4, @bounds.fail.5, @bounds.ok.5
    @bounds.fail.5
        call $ripe_panic_bounds(l %t3, l 2)
        hlt
    @bounds.ok.5
        %t6 =l mul %t3, 8
        %t7 =l add %m, %t6
        %t8 =l extsw 1
        %t9 =w cugel %t8, 2
        jnz %t9, @bounds.fail.10, @bounds.ok.10
    @bounds.fail.10
        call $ripe_panic_bounds(l %t8, l 2)
        hlt
    @bounds.ok.10
        %t11 =l mul %t8, 8
        %t12 =l add %m, %t11
        blit %t12, %t7, 8
        %t13 =l extsw 0
        %t14 =w cugel %t13, 2
        jnz %t14, @bounds.fail.15, @bounds.ok.15
    @bounds.fail.15
        call $ripe_panic_bounds(l %t13, l 2)
        hlt
    @bounds.ok.15
        %t16 =l mul %t13, 8
        %t17 =l add %m, %t16
        %t18 =l extsw 0
        %t19 =w cugel %t18, 2
        jnz %t19, @bounds.fail.20, @bounds.ok.20
    @bounds.fail.20
        call $ripe_panic_bounds(l %t18, l 2)
        hlt
    @bounds.ok.20
        %t21 =l mul %t18, 4
        %t22 =l add %t17, %t21
        %t23 =w loadsw %t22
        %t24 =l extsw 0
        %t25 =w cugel %t24, 2
        jnz %t25, @bounds.fail.26, @bounds.ok.26
    @bounds.fail.26
        call $ripe_panic_bounds(l %t24, l 2)
        hlt
    @bounds.ok.26
        %t27 =l mul %t24, 8
        %t28 =l add %m, %t27
        %t29 =l extsw 1
        %t30 =w cugel %t29, 2
        jnz %t30, @bounds.fail.31, @bounds.ok.31
    @bounds.fail.31
        call $ripe_panic_bounds(l %t29, l 2)
        hlt
    @bounds.ok.31
        %t32 =l mul %t29, 4
        %t33 =l add %t28, %t32
        %t34 =w loadsw %t33
        %t35 =w add %t23, %t34
        ret %t35
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
        %p =l alloc8 8
        storew 0, %n
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

let%expect_test "codegen: let arithmetic local" =
  run_codegen {|
func f() i32 {
  let x: i32 = 1 + 2
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
        %t3 =w cultw 2, 32
        %t4 =w shl %t2, 2
        %t6 =w sub 0, %t3
        %t5 =w and %t4, %t6
        ret %t5
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
        jmp @if.end2
    @if.end2
        %t6 =w loadsw %a
        ret %t6
    }
    |}]

let%expect_test "codegen: unary plus is an identity" =
  run_codegen {|
comptime N: i32 = +4
func f(a: i32) i32 { return +a + N }
|};
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w add %t1, 4
        ret %t2
    }
    |}]

let%expect_test "codegen: unary plus folds a comptime identifier" =
  run_codegen
    {|
comptime A: i32 = 4
comptime B: i32 = +A
func f() i32 { return B }
|};
  [%expect {|
    function w $f() {
    @start
        ret 4
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
    @loop.body0
    @if.cond1_0
        %t2 =w loadsw %i
        %t3 =w csltw %t2, 10
        jnz %t3, @if.else1, @if.then1_0
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
        %t4 =w loadsw %i
        %t5 =w add %t4, 1
        storew %t5, %i
        jmp @loop.body0
    @loop.end0
        %t6 =w loadsw %i
        ret %t6
    }
    |}]

let%expect_test "codegen: nested if" =
  run_codegen
    "func f(a: i32, b: i32) i32 {\n\
    \  if a > 0 { if b > 0 { return 1 } }\n\
    \  return 0\n\
     }";
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
        jmp @if.end5
    @if.end5
        jmp @if.end2
    @if.else2
        jmp @if.end2
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

let%expect_test "codegen: global let narrowing cast folds" =
  run_codegen {|
let A: u8 = 1000 as u8
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
  run_codegen "func f() usize { return sizeof(i32) }";
  [%expect
    {|
    function l $f() {
    @start
        ret 4
    }
    |}]

let%expect_test "codegen: global let int" =
  run_codegen {|
let X: i32 = 42
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
        %p =l alloc8 8
        storew 5, %x
        %t0 =l copy %x
        storel %t0, %p
        %t1 =l loadl %p
        %t3 =w ceql %t1, 0
        jnz %t3, @null.fail.4, @null.ok.4
    @null.fail.4
        call $ripe_panic_null()
        hlt
    @null.ok.4
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
        %t3 =w cugel %t2, 3
        jnz %t3, @bounds.fail.4, @bounds.ok.4
    @bounds.fail.4
        call $ripe_panic_bounds(l %t2, l 3)
        hlt
    @bounds.ok.4
        %t5 =l mul %t2, 4
        %t6 =l add %a, %t5
        %t7 =w loadsw %t6
        ret %t7
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
        %t2 =w cugel %t1, 2
        jnz %t2, @bounds.fail.3, @bounds.ok.3
    @bounds.fail.3
        call $ripe_panic_bounds(l %t1, l 2)
        hlt
    @bounds.ok.3
        %t4 =l mul %t1, 4
        %t5 =l add %a, %t4
        storew 9, %t5
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
        %t2 =w cugel %t1, 2
        jnz %t2, @bounds.fail.3, @bounds.ok.3
    @bounds.fail.3
        call $ripe_panic_bounds(l %t1, l 2)
        hlt
    @bounds.ok.3
        %t4 =l mul %t1, 8
        %t5 =l add %a, %t4
        %t6 =l loadl %t5
        ret %t6
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
        %i =l alloc4 4
        %for.hi.1 =l alloc4 4
        %compound.p.0 =l alloc8 8
        storew 0, %sum
        storew 0, %i
        storew 3, %for.hi.1
    @loop.body0
    @if.cond1_0
        %t2 =w loadsw %i
        %t3 =w loadsw %for.hi.1
        %t4 =w csltw %t2, %t3
        jnz %t4, @if.else1, @if.then1_0
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
        %t5 =l copy %sum
        storel %t5, %compound.p.0
        %t6 =l loadl %compound.p.0
        %t7 =w ceql %t6, 0
        jnz %t7, @null.fail.8, @null.ok.8
    @null.fail.8
        call $ripe_panic_null()
        hlt
    @null.ok.8
        %t9 =l loadl %compound.p.0
        %t11 =w ceql %t9, 0
        jnz %t11, @null.fail.12, @null.ok.12
    @null.fail.12
        call $ripe_panic_null()
        hlt
    @null.ok.12
        %t10 =w loadsw %t9
        %t13 =w loadsw %i
        %t14 =w add %t10, %t13
        storew %t14, %t6
        %t15 =w loadsw %i
        %t16 =w add %t15, 1
        storew %t16, %i
        jmp @loop.body0
    @loop.end0
        %t17 =w loadsw %sum
        ret %t17
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
        %i =l alloc4 4
        %for.hi.1 =l alloc4 4
        %compound.p.0 =l alloc8 8
        storew 0, %sum
        storew 0, %i
        storew 3, %for.hi.1
    @loop.body0
    @if.cond1_0
        %t2 =w loadsw %i
        %t3 =w loadsw %for.hi.1
        %t4 =w cslew %t2, %t3
        jnz %t4, @if.else1, @if.then1_0
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
        %t5 =l copy %sum
        storel %t5, %compound.p.0
        %t6 =l loadl %compound.p.0
        %t7 =w ceql %t6, 0
        jnz %t7, @null.fail.8, @null.ok.8
    @null.fail.8
        call $ripe_panic_null()
        hlt
    @null.ok.8
        %t9 =l loadl %compound.p.0
        %t11 =w ceql %t9, 0
        jnz %t11, @null.fail.12, @null.ok.12
    @null.fail.12
        call $ripe_panic_null()
        hlt
    @null.ok.12
        %t10 =w loadsw %t9
        %t13 =w loadsw %i
        %t14 =w add %t10, %t13
        storew %t14, %t6
    @if.cond15_0
        %t16 =w loadsw %i
        %t17 =w loadsw %for.hi.1
        %t18 =w ceqw %t16, %t17
        jnz %t18, @if.then15_0, @if.else15
    @if.then15_0
        jmp @loop.end0
    @if.else15
        jmp @if.end15
    @if.end15
        %t19 =w loadsw %i
        %t20 =w add %t19, 1
        storew %t20, %i
        jmp @loop.body0
    @loop.end0
        %t21 =w loadsw %sum
        ret %t21
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
        %sum =l alloc4 4
        %for.p.1 =l alloc8 8
        %for.n.2 =l alloc8 8
        %for.i.3 =l alloc8 8
        %x =l alloc4 4
        %compound.p.0 =l alloc8 8
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        storew 0, %sum
        storel %a, %for.p.1
        storel 3, %for.n.2
        storel 0, %for.i.3
    @loop.body2
    @if.cond3_0
        %t4 =l loadl %for.i.3
        %t5 =l loadl %for.n.2
        %t6 =w cultl %t4, %t5
        jnz %t6, @if.else3, @if.then3_0
    @if.then3_0
        jmp @loop.end2
    @if.else3
        jmp @if.end3
    @if.end3
        %t7 =l loadl %for.p.1
        %t8 =l loadl %for.i.3
        %t9 =l mul %t8, 4
        %t10 =l add %t7, %t9
        %t11 =w loadsw %t10
        storew %t11, %x
        %t12 =l copy %sum
        storel %t12, %compound.p.0
        %t13 =l loadl %compound.p.0
        %t14 =w ceql %t13, 0
        jnz %t14, @null.fail.15, @null.ok.15
    @null.fail.15
        call $ripe_panic_null()
        hlt
    @null.ok.15
        %t16 =l loadl %compound.p.0
        %t18 =w ceql %t16, 0
        jnz %t18, @null.fail.19, @null.ok.19
    @null.fail.19
        call $ripe_panic_null()
        hlt
    @null.ok.19
        %t17 =w loadsw %t16
        %t20 =w loadsw %x
        %t21 =w add %t17, %t20
        storew %t21, %t13
        %t22 =l loadl %for.i.3
        %t23 =l add %t22, 1
        storel %t23, %for.i.3
        jmp @loop.body2
    @loop.end2
        %t24 =w loadsw %sum
        ret %t24
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
        %i =l alloc4 4
        %for.hi.1 =l alloc4 4
        %compound.p.0 =l alloc8 8
        storew 0, %sum
        storew 0, %i
        storew 10, %for.hi.1
    @loop.body0
    @if.cond1_0
        %t2 =w loadsw %i
        %t3 =w loadsw %for.hi.1
        %t4 =w csltw %t2, %t3
        jnz %t4, @if.else1, @if.then1_0
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
    @if.cond5_0
        %t6 =w loadsw %i
        %t7 =w ceqw %t6, 2
        jnz %t7, @if.then5_0, @if.else5
    @if.then5_0
        %t8 =w loadsw %i
        %t9 =w add %t8, 1
        storew %t9, %i
        jmp @loop.body0
    @if.else5
        jmp @if.end5
    @if.end5
    @if.cond10_0
        %t11 =w loadsw %i
        %t12 =w ceqw %t11, 5
        jnz %t12, @if.then10_0, @if.else10
    @if.then10_0
        jmp @loop.end0
    @if.else10
        jmp @if.end10
    @if.end10
        %t13 =l copy %sum
        storel %t13, %compound.p.0
        %t14 =l loadl %compound.p.0
        %t15 =w ceql %t14, 0
        jnz %t15, @null.fail.16, @null.ok.16
    @null.fail.16
        call $ripe_panic_null()
        hlt
    @null.ok.16
        %t17 =l loadl %compound.p.0
        %t19 =w ceql %t17, 0
        jnz %t19, @null.fail.20, @null.ok.20
    @null.fail.20
        call $ripe_panic_null()
        hlt
    @null.ok.20
        %t18 =w loadsw %t17
        %t21 =w loadsw %i
        %t22 =w add %t18, %t21
        storew %t22, %t14
        %t23 =w loadsw %i
        %t24 =w add %t23, 1
        storew %t24, %i
        jmp @loop.body0
    @loop.end0
        %t25 =w loadsw %sum
        ret %t25
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
        %compound.p.0 =l alloc8 8
        storew 0, %i
    @loop.body0
    @if.cond1_0
        jmp @if.else1
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
    @if.cond2_0
        %t3 =w loadsw %i
        %t4 =w ceqw %t3, 3
        jnz %t4, @if.then2_0, @if.else2
    @if.then2_0
        jmp @loop.end0
    @if.else2
        jmp @if.end2
    @if.end2
        %t5 =l copy %i
        storel %t5, %compound.p.0
        %t6 =l loadl %compound.p.0
        %t7 =w ceql %t6, 0
        jnz %t7, @null.fail.8, @null.ok.8
    @null.fail.8
        call $ripe_panic_null()
        hlt
    @null.ok.8
        %t9 =l loadl %compound.p.0
        %t11 =w ceql %t9, 0
        jnz %t11, @null.fail.12, @null.ok.12
    @null.fail.12
        call $ripe_panic_null()
        hlt
    @null.ok.12
        %t10 =w loadsw %t9
        %t13 =w add %t10, 1
        storew %t13, %t6
        jmp @loop.body0
    @loop.end0
        %t14 =w loadsw %i
        ret %t14
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
        blit %t0, %xs, 16
        ret 0
    }

    function w $f() {
    @start
        %a =l alloc4 12
        %t2 =l alloc8 16
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
  let s: []i32 = a[1..3]
  return s[0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 16
        %s =l alloc8 16
        %t12 =l alloc8 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l add %a, 12
        storew 4, %t2
        %t3 =l extsw 1
        %t4 =l extsw 3
        %t5 =w cugtl %t4, 4
        %t6 =w cugtl %t3, %t4
        %t7 =w or %t5, %t6
        jnz %t7, @slice.fail.8, @slice.ok.8
    @slice.fail.8
        call $ripe_panic_slice_bounds(l %t3, l %t4, l 4)
        hlt
    @slice.ok.8
        %t9 =l mul %t3, 4
        %t10 =l add %a, %t9
        %t11 =l sub %t4, %t3
        storel %t10, %t12
        %t13 =l add %t12, 8
        storel %t11, %t13
        blit %t12, %s, 16
        %t14 =l loadl %s
        %t15 =l add %s, 8
        %t16 =l loadl %t15
        %t17 =l extsw 0
        %t18 =w cugel %t17, %t16
        jnz %t18, @bounds.fail.19, @bounds.ok.19
    @bounds.fail.19
        call $ripe_panic_bounds(l %t17, l %t16)
        hlt
    @bounds.ok.19
        %t20 =l mul %t17, 4
        %t21 =l add %t14, %t20
        %t22 =w loadsw %t21
        ret %t22
    }
    |}]

let%expect_test "codegen: inclusive sub-slice construction" =
  run_codegen
    {|
func f() i32 {
  var a: [4]i32 = [1, 2, 3, 4]
  let s: []i32 = a[1..=3]
  return s[0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 16
        %s =l alloc8 16
        %t13 =l alloc8 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l add %a, 12
        storew 4, %t2
        %t3 =l extsw 1
        %t4 =w add 3, 1
        %t5 =l extsw %t4
        %t6 =w cugtl %t5, 4
        %t7 =w cugtl %t3, %t5
        %t8 =w or %t6, %t7
        jnz %t8, @slice.fail.9, @slice.ok.9
    @slice.fail.9
        call $ripe_panic_slice_bounds(l %t3, l %t5, l 4)
        hlt
    @slice.ok.9
        %t10 =l mul %t3, 4
        %t11 =l add %a, %t10
        %t12 =l sub %t5, %t3
        storel %t11, %t13
        %t14 =l add %t13, 8
        storel %t12, %t14
        blit %t13, %s, 16
        %t15 =l loadl %s
        %t16 =l add %s, 8
        %t17 =l loadl %t16
        %t18 =l extsw 0
        %t19 =w cugel %t18, %t17
        jnz %t19, @bounds.fail.20, @bounds.ok.20
    @bounds.fail.20
        call $ripe_panic_bounds(l %t18, l %t17)
        hlt
    @bounds.ok.20
        %t21 =l mul %t18, 4
        %t22 =l add %t15, %t21
        %t23 =w loadsw %t22
        ret %t23
    }
    |}]

let%expect_test "codegen: slice len loads from fat pointer" =
  run_codegen
    {|
func f() usize {
  var a: [3]i32 = [1, 2, 3]
  let s: []i32 = a[0..3]
  return s.len
}
|};
  [%expect
    {|
    function l $f() {
    @start
        %a =l alloc4 12
        %s =l alloc8 16
        %t11 =l alloc8 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l extsw 0
        %t3 =l extsw 3
        %t4 =w cugtl %t3, 3
        %t5 =w cugtl %t2, %t3
        %t6 =w or %t4, %t5
        jnz %t6, @slice.fail.7, @slice.ok.7
    @slice.fail.7
        call $ripe_panic_slice_bounds(l %t2, l %t3, l 3)
        hlt
    @slice.ok.7
        %t8 =l mul %t2, 4
        %t9 =l add %a, %t8
        %t10 =l sub %t3, %t2
        storel %t9, %t11
        %t12 =l add %t11, 8
        storel %t10, %t12
        blit %t11, %s, 16
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
        %t =l alloc4 4
        %for.it.1 =l alloc8 16
        %for.p.2 =l alloc8 8
        %for.n.3 =l alloc8 8
        %for.i.4 =l alloc8 8
        %x =l alloc4 4
        %compound.p.0 =l alloc8 8
        %xs =l alloc8 16
        blit %t0, %xs, 16
        storew 0, %t
        blit %xs, %for.it.1, 16
        %t1 =l loadl %for.it.1
        storel %t1, %for.p.2
        %t2 =l add %for.it.1, 8
        %t3 =l loadl %t2
        storel %t3, %for.n.3
        storel 0, %for.i.4
    @loop.body4
    @if.cond5_0
        %t6 =l loadl %for.i.4
        %t7 =l loadl %for.n.3
        %t8 =w cultl %t6, %t7
        jnz %t8, @if.else5, @if.then5_0
    @if.then5_0
        jmp @loop.end4
    @if.else5
        jmp @if.end5
    @if.end5
        %t9 =l loadl %for.p.2
        %t10 =l loadl %for.i.4
        %t11 =l mul %t10, 4
        %t12 =l add %t9, %t11
        %t13 =w loadsw %t12
        storew %t13, %x
        %t14 =l copy %t
        storel %t14, %compound.p.0
        %t15 =l loadl %compound.p.0
        %t16 =w ceql %t15, 0
        jnz %t16, @null.fail.17, @null.ok.17
    @null.fail.17
        call $ripe_panic_null()
        hlt
    @null.ok.17
        %t18 =l loadl %compound.p.0
        %t20 =w ceql %t18, 0
        jnz %t20, @null.fail.21, @null.ok.21
    @null.fail.21
        call $ripe_panic_null()
        hlt
    @null.ok.21
        %t19 =w loadsw %t18
        %t22 =w loadsw %x
        %t23 =w add %t19, %t22
        storew %t23, %t15
        %t24 =l loadl %for.i.4
        %t25 =l add %t24, 1
        storel %t25, %for.i.4
        jmp @loop.body4
    @loop.end4
        %t26 =w loadsw %t
        ret %t26
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
        %s =l alloc8 16
        %t11 =l alloc8 16
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l extsw 0
        %t3 =l extsw 3
        %t4 =w cugtl %t3, 3
        %t5 =w cugtl %t2, %t3
        %t6 =w or %t4, %t5
        jnz %t6, @slice.fail.7, @slice.ok.7
    @slice.fail.7
        call $ripe_panic_slice_bounds(l %t2, l %t3, l 3)
        hlt
    @slice.ok.7
        %t8 =l mul %t2, 4
        %t9 =l add %a, %t8
        %t10 =l sub %t3, %t2
        storel %t9, %t11
        %t12 =l add %t11, 8
        storel %t10, %t12
        blit %t11, %s, 16
        %t13 =l loadl %s
        %t14 =l add %s, 8
        %t15 =l loadl %t14
        %t16 =l extsw 1
        %t17 =w cugel %t16, %t15
        jnz %t17, @bounds.fail.18, @bounds.ok.18
    @bounds.fail.18
        call $ripe_panic_bounds(l %t16, l %t15)
        hlt
    @bounds.ok.18
        %t19 =l mul %t16, 4
        %t20 =l add %t13, %t19
        storew 9, %t20
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
        %compound.p.0 =l alloc8 8
        storew 1, %a
        %t0 =l add %a, 4
        storew 2, %t0
        %t1 =l add %a, 8
        storew 3, %t1
        %t2 =l extsw 1
        %t3 =w cugel %t2, 3
        jnz %t3, @bounds.fail.4, @bounds.ok.4
    @bounds.fail.4
        call $ripe_panic_bounds(l %t2, l 3)
        hlt
    @bounds.ok.4
        %t5 =l mul %t2, 4
        %t6 =l add %a, %t5
        %t7 =l copy %t6
        storel %t7, %compound.p.0
        %t8 =l loadl %compound.p.0
        %t9 =w ceql %t8, 0
        jnz %t9, @null.fail.10, @null.ok.10
    @null.fail.10
        call $ripe_panic_null()
        hlt
    @null.ok.10
        %t11 =l loadl %compound.p.0
        %t13 =w ceql %t11, 0
        jnz %t13, @null.fail.14, @null.ok.14
    @null.fail.14
        call $ripe_panic_null()
        hlt
    @null.ok.14
        %t12 =w loadsw %t11
        %t15 =w add %t12, 5
        storew %t15, %t8
        %t16 =l extsw 1
        %t17 =w cugel %t16, 3
        jnz %t17, @bounds.fail.18, @bounds.ok.18
    @bounds.fail.18
        call $ripe_panic_bounds(l %t16, l 3)
        hlt
    @bounds.ok.18
        %t19 =l mul %t16, 4
        %t20 =l add %a, %t19
        %t21 =w loadsw %t20
        ret %t21
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
        %t4 =w cugel %t3, 2
        jnz %t4, @bounds.fail.5, @bounds.ok.5
    @bounds.fail.5
        call $ripe_panic_bounds(l %t3, l 2)
        hlt
    @bounds.ok.5
        %t6 =l mul %t3, 8
        %t7 =l add %m, %t6
        %t8 =l extsw 0
        %t9 =w cugel %t8, 2
        jnz %t9, @bounds.fail.10, @bounds.ok.10
    @bounds.fail.10
        call $ripe_panic_bounds(l %t8, l 2)
        hlt
    @bounds.ok.10
        %t11 =l mul %t8, 4
        %t12 =l add %t7, %t11
        %t13 =w loadsw %t12
        ret %t13
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
        %t1 =w cugel %t0, 3
        jnz %t1, @bounds.fail.2, @bounds.ok.2
    @bounds.fail.2
        call $ripe_panic_bounds(l %t0, l 3)
        hlt
    @bounds.ok.2
        %t3 =l mul %t0, 4
        %t4 =l add $g, %t3
        %t5 =w loadsw %t4
        ret %t5
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
        %s =l alloc4 4
        %for.p.1 =l alloc8 8
        %for.n.2 =l alloc8 8
        %for.i.3 =l alloc8 8
        %row =l alloc4 8
        %compound.p.0 =l alloc8 8
        storew 1, %m
        %t0 =l add %m, 4
        storew 2, %t0
        %t1 =l add %m, 8
        storew 3, %t1
        %t2 =l add %t1, 4
        storew 4, %t2
        storew 0, %s
        storel %m, %for.p.1
        storel 2, %for.n.2
        storel 0, %for.i.3
    @loop.body3
    @if.cond4_0
        %t5 =l loadl %for.i.3
        %t6 =l loadl %for.n.2
        %t7 =w cultl %t5, %t6
        jnz %t7, @if.else4, @if.then4_0
    @if.then4_0
        jmp @loop.end3
    @if.else4
        jmp @if.end4
    @if.end4
        %t8 =l loadl %for.p.1
        %t9 =l loadl %for.i.3
        %t10 =l mul %t9, 8
        %t11 =l add %t8, %t10
        blit %t11, %row, 8
        %t12 =l copy %s
        storel %t12, %compound.p.0
        %t13 =l loadl %compound.p.0
        %t14 =w ceql %t13, 0
        jnz %t14, @null.fail.15, @null.ok.15
    @null.fail.15
        call $ripe_panic_null()
        hlt
    @null.ok.15
        %t16 =l loadl %compound.p.0
        %t18 =w ceql %t16, 0
        jnz %t18, @null.fail.19, @null.ok.19
    @null.fail.19
        call $ripe_panic_null()
        hlt
    @null.ok.19
        %t17 =w loadsw %t16
        %t20 =l extsw 0
        %t21 =w cugel %t20, 2
        jnz %t21, @bounds.fail.22, @bounds.ok.22
    @bounds.fail.22
        call $ripe_panic_bounds(l %t20, l 2)
        hlt
    @bounds.ok.22
        %t23 =l mul %t20, 4
        %t24 =l add %row, %t23
        %t25 =w loadsw %t24
        %t26 =w add %t17, %t25
        storew %t26, %t13
        %t27 =l loadl %for.i.3
        %t28 =l add %t27, 1
        storel %t28, %for.i.3
        jmp @loop.body3
    @loop.end3
        %t29 =w loadsw %s
        ret %t29
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
        %t2 =w cugel %t1, 3
        jnz %t2, @bounds.fail.3, @bounds.ok.3
    @bounds.fail.3
        call $ripe_panic_bounds(l %t1, l 3)
        hlt
    @bounds.ok.3
        %t4 =l mul %t1, 4
        %t5 =l add %a, %t4
        %t6 =w loadsw %t5
        ret %t6
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
        %for.p.1 =l alloc8 8
        %t0 =l alloc4 12
        %for.n.2 =l alloc8 8
        %for.i.3 =l alloc8 8
        %x =l alloc4 4
        %compound.p.0 =l alloc8 8
        storew 0, %s
        storew 1, %t0
        %t1 =l add %t0, 4
        storew 2, %t1
        %t2 =l add %t0, 8
        storew 3, %t2
        storel %t0, %for.p.1
        storel 3, %for.n.2
        storel 0, %for.i.3
    @loop.body3
    @if.cond4_0
        %t5 =l loadl %for.i.3
        %t6 =l loadl %for.n.2
        %t7 =w cultl %t5, %t6
        jnz %t7, @if.else4, @if.then4_0
    @if.then4_0
        jmp @loop.end3
    @if.else4
        jmp @if.end4
    @if.end4
        %t8 =l loadl %for.p.1
        %t9 =l loadl %for.i.3
        %t10 =l mul %t9, 4
        %t11 =l add %t8, %t10
        %t12 =w loadsw %t11
        storew %t12, %x
        %t13 =l copy %s
        storel %t13, %compound.p.0
        %t14 =l loadl %compound.p.0
        %t15 =w ceql %t14, 0
        jnz %t15, @null.fail.16, @null.ok.16
    @null.fail.16
        call $ripe_panic_null()
        hlt
    @null.ok.16
        %t17 =l loadl %compound.p.0
        %t19 =w ceql %t17, 0
        jnz %t19, @null.fail.20, @null.ok.20
    @null.fail.20
        call $ripe_panic_null()
        hlt
    @null.ok.20
        %t18 =w loadsw %t17
        %t21 =w loadsw %x
        %t22 =w add %t18, %t21
        storew %t22, %t14
        %t23 =l loadl %for.i.3
        %t24 =l add %t23, 1
        storel %t24, %for.i.3
        jmp @loop.body3
    @loop.end3
        %t25 =w loadsw %s
        ret %t25
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
  if n < 0 { return -1 } else if n > 0 { return 1 } else { return 0 }
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

let%expect_test "codegen: nested array field flattens to one subtype" =
  run_codegen
    {|
struct grid { cells: [2][2]i32 }
func f() i32 {
  var g: grid
  return 0
}
|};
  [%expect
    {|
    type :grid = { w 4 }

    function w $f() {
    @start
        %g =l alloc8 16
        storel 0, %g
        %t0 =l add %g, 8
        storel 0, %t0
        ret 0
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
        blit %t0, %p, 8
        %t1 =l add %p, 4
        %t2 =w loadsw %t1
        ret %t2
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
        %b =l alloc8 8
        %a =l alloc8 8
        blit %t0, %a, 8
        blit %a, %b, 8
        %t1 =w loadsw %b
        ret %t1
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
        %t3 =w cugel %t2, 4
        jnz %t3, @bounds.fail.4, @bounds.ok.4
    @bounds.fail.4
        call $ripe_panic_bounds(l %t2, l 4)
        hlt
    @bounds.ok.4
        %t5 =l mul %t2, 4
        %t6 =l add %b, %t5
        %t7 =w loadsw %t6
        ret %t7
    }
    |}]

let%expect_test "codegen: struct literal" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
func f() i32 {
  let p = pt { x: 3, y: 4 }
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
  let p = pt { x: 3 }
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
  let o = outer { i: inner { a: 1 }, b: 2 }
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

let%expect_test "codegen: let global struct data with padding" =
  run_codegen
    {|
struct mix { a: i8, b: i64 }
let g: mix = mix { a: 1, b: 2 }
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
        %x.2 =l alloc4 4
        storew 1, %x
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
        %x.2 =l alloc4 4
        storew 1, %x
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
        %g =l alloc4 4
        %c =l alloc4 1
        storeb %t0, %c
    @if.cond1_0
        %t2 =w loadub %c
        jnz %t2, @if.then1_0, @if.else1
    @if.then1_0
        storew 5, %g
        %t3 =w loadsw %g
        %t4 =w add %t3, 1
        storew %t4, %g
        jmp @if.end1
    @if.else1
        jmp @if.end1
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
        %i.2 =l alloc4 4
        %for.hi.0 =l alloc4 4
        %y =l alloc4 4
        storew 99, %i
        storew 0, %i.2
        storew 3, %for.hi.0
    @loop.body0
    @if.cond1_0
        %t2 =w loadsw %i.2
        %t3 =w loadsw %for.hi.0
        %t4 =w csltw %t2, %t3
        jnz %t4, @if.else1, @if.then1_0
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
        %t5 =w loadsw %i.2
        storew %t5, %y
        %t6 =w loadsw %i.2
        %t7 =w add %t6, 1
        storew %t7, %i.2
        jmp @loop.body0
    @loop.end0
        %t8 =w loadsw %i
        ret %t8
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
        %x =l alloc4 4
        %x.3 =l alloc4 4
        %n =l alloc4 4
        storew %t0, %n
        storew 1, %x
    @loop.body1
    @if.cond2_0
        %t3 =w loadsw %x
        %t4 =w loadsw %n
        %t5 =w csltw %t3, %t4
        jnz %t5, @if.else2, @if.then2_0
    @if.then2_0
        jmp @loop.end1
    @if.else2
        jmp @if.end2
    @if.end2
        storew 100, %x.3
        %t6 =w loadsw %x.3
        %t7 =w add %t6, 1
        storew %t7, %x.3
        jmp @loop.body1
    @loop.end1
        %t8 =w loadsw %x
        ret %t8
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
        %x.2 =l alloc8 8
        storew 1, %x
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
        %p.3 =l alloc8 8
        storew 1, %p
        %t0 =l add %p, 4
        storew 2, %t0
        storew 3, %p.3
        %t1 =l add %p.3, 4
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
        %x.2 =l alloc4 4
        %x.3 =l alloc4 4
        storew 1, %x
        storew 2, %x.2
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
        %x.2 =l alloc4 4
        %compound.p.0 =l alloc8 8
        storew 1, %x
        storew 10, %x.2
        %t0 =l copy %x.2
        storel %t0, %compound.p.0
        %t1 =l loadl %compound.p.0
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        %t4 =l loadl %compound.p.0
        %t6 =w ceql %t4, 0
        jnz %t6, @null.fail.7, @null.ok.7
    @null.fail.7
        call $ripe_panic_null()
        hlt
    @null.ok.7
        %t5 =w loadsw %t4
        %t8 =w add %t5, 5
        storew %t8, %t1
        %t9 =w loadsw %x
        ret %t9
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
        jnz %t4, @sel.then2, @sel.else2
    @sel.then2
        %t5 =l loadl %p
        %t7 =w ceql %t5, 0
        jnz %t7, @null.fail.8, @null.ok.8
    @null.fail.8
        call $ripe_panic_null()
        hlt
    @null.ok.8
        %t6 =w loadsw %t5
        %t9 =w ceqw %t6, 3
        jnz %t9, @if.then1_0, @if.else1
    @sel.else2
        jmp @if.else1
    @if.then1_0
        ret 1
    @if.else1
        jmp @if.end1
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
    @if.cond2_0
        %t4 =w loadub %a
        jnz %t4, @if.then2_0, @if.else2
    @if.then2_0
        %t3 =w copy 1
        jmp @if.end2
    @if.else2
        %t5 =w loadub %b
        %t3 =w copy %t5
        jmp @if.end2
    @if.end2
        ret %t3
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
        jnz %t6, @sel.then5, @sel.else5
    @sel.then5
        %t7 =w loadub %b
        jnz %t7, @sel.then4, @sel.else4
    @sel.else5
        jmp @sel.else4
    @sel.then4
        %t8 =w loadub %c
        jnz %t8, @if.then3_0, @if.else3
    @sel.else4
        jmp @if.else3
    @if.then3_0
        ret 1
    @if.else3
        jmp @if.end3
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
    @if.cond3_0
        %t5 =w loadub %a
        jnz %t5, @if.then3_0, @if.else3
    @if.then3_0
        %t4 =w copy 1
        jmp @if.end3
    @if.else3
    @if.cond6_0
        %t8 =w loadub %b
        jnz %t8, @if.then6_0, @if.else6
    @if.then6_0
        %t9 =w loadub %c
        %t7 =w copy %t9
        jmp @if.end6
    @if.else6
        %t7 =w copy 0
        jmp @if.end6
    @if.end6
        %t4 =w copy %t7
        jmp @if.end3
    @if.end3
        ret %t4
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
        jnz %t4, @sel.else3, @sel.then3
    @sel.then3
        %t5 =w loadub %b
        jnz %t5, @if.then2_0, @if.else2
    @sel.else3
        jmp @if.else2
    @if.then2_0
        ret 1
    @if.else2
        jmp @if.end2
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
    @loop.body2
    @if.cond3_0
        %t5 =w loadub %a
        jnz %t5, @sel.then4, @sel.else4
    @sel.then4
        %t6 =w loadub %b
        jnz %t6, @if.else3, @if.then3_0
    @sel.else4
        jmp @if.then3_0
    @if.then3_0
        jmp @loop.end2
    @if.else3
        jmp @if.end3
    @if.end3
        ret
    @loop.end2
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
        %ok =l alloc4 1
        %a =l alloc4 1
        storeb %t0, %a
        %b =l alloc4 1
        storeb %t1, %b
    @if.cond2_0
        %t4 =w loadub %a
        jnz %t4, @if.then2_0, @if.else2
    @if.then2_0
        %t5 =w loadub %b
        %t3 =w copy %t5
        jmp @if.end2
    @if.else2
        %t3 =w copy 0
        jmp @if.end2
    @if.end2
        storeb %t3, %ok
    @if.cond6_0
        %t7 =w loadub %ok
        jnz %t7, @if.then6_0, @if.else6
    @if.then6_0
        ret 1
    @if.else6
        jmp @if.end6
    @if.end6
        ret 0
    }
    |}]

let%expect_test "codegen: let references another let by value" =
  run_codegen {|
let A: i32 = 5
let B: i32 = A
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

let%expect_test "codegen: let references a later let" =
  run_codegen {|
let B: i32 = A
let A: i32 = 5
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

let%expect_test "codegen: var initialized from a let" =
  run_codegen {|
let A: i32 = 5
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

let%expect_test "codegen: self referential let is a cycle" =
  run_codegen {|
let X: i32 = X
func f() i32 { return X }
|};
  [%expect
    {|
    error: cyclic constant: X
      at <test>:2:14
        let X: i32 = X
                     ^
    |}]

let%expect_test "codegen: mutually referential consts are a cycle" =
  run_codegen {|
let A: i32 = B
let B: i32 = A
func f() i32 { return A }
|};
  [%expect
    {|
    error: cyclic constant: B
      at <test>:2:14
        let A: i32 = B
                     ^
    |}]

let%expect_test "codegen: comptime global inlines with no data" =
  run_codegen {|
comptime N: i32 = 4
func f() i32 { return N }
|};
  [%expect {|
    function w $f() {
    @start
        ret 4
    }
    |}]

let%expect_test "codegen: comptime expression folds at the use site" =
  run_codegen
    {|
comptime N: i32 = 4
comptime M: i32 = N * 2 + 1
func f() i32 { return M }
|};
  [%expect {|
    function w $f() {
    @start
        ret 9
    }
    |}]

let%expect_test "codegen: local comptime gets no slot" =
  run_codegen {|
func f() i32 {
  comptime c: i32 = 2
  return c
}
|};
  [%expect {|
    function w $f() {
    @start
        ret 2
    }
    |}]

let%expect_test "codegen: comptime forward reference" =
  run_codegen
    {|
comptime B: i32 = A + 1
comptime A: i32 = 4
func f() i32 { return B }
|};
  [%expect {|
    function w $f() {
    @start
        ret 5
    }
    |}]

let%expect_test "codegen: comptime reads a let global" =
  run_codegen
    {|
let L: i32 = 10
comptime C: i32 = L * 2
func f() i32 { return C }
|};
  [%expect
    {|
    data $L = align 4 { w 10 }

    function w $f() {
    @start
        ret 20
    }
    |}]

let%expect_test "codegen: let initialized from a comptime" =
  run_codegen
    {|
comptime N: i32 = 4
let L: i32 = N + 1
func f() i32 { return L }
|};
  [%expect
    {|
    data $L = align 4 { w 5 }

    function w $f() {
    @start
        %t0 =w loadsw $L
        ret %t0
    }
    |}]

let%expect_test "codegen: sizeof folds in a comptime" =
  run_codegen
    {|
struct pt { x: i32, y: i32 }
comptime S: usize = sizeof(pt)
func f() usize { return S }
|};
  [%expect
    {|
    type :pt = { w, w }

    function l $f() {
    @start
        ret 8
    }
    |}]

let%expect_test "codegen: comptime sizes a global array" =
  run_codegen
    {|
comptime N: i32 = 3
let A: [N]i32 = [1, 2, 3]
func f() i32 { return A[1] }
|};
  [%expect
    {|
    data $A = align 4 { w 1, w 2, w 3 }

    function w $f() {
    @start
        %t0 =l extsw 1
        %t1 =w cugel %t0, 3
        jnz %t1, @bounds.fail.2, @bounds.ok.2
    @bounds.fail.2
        call $ripe_panic_bounds(l %t0, l 3)
        hlt
    @bounds.ok.2
        %t3 =l mul %t0, 4
        %t4 =l add $A, %t3
        %t5 =w loadsw %t4
        ret %t5
    }
    |}]

let%expect_test "codegen: comptime sizes a local array" =
  run_codegen
    {|
comptime N: i32 = 2
func f() i32 {
  var a: [N]i32 = [7, 8]
  return a[0]
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %a =l alloc4 8
        storew 7, %a
        %t0 =l add %a, 4
        storew 8, %t0
        %t1 =l extsw 0
        %t2 =w cugel %t1, 2
        jnz %t2, @bounds.fail.3, @bounds.ok.3
    @bounds.fail.3
        call $ripe_panic_bounds(l %t1, l 2)
        hlt
    @bounds.ok.3
        %t4 =l mul %t1, 4
        %t5 =l add %a, %t4
        %t6 =w loadsw %t5
        ret %t6
    }
    |}]

let%expect_test "codegen: expression array size" =
  run_codegen
    {|
comptime N: i32 = 3
func f() usize { return sizeof([N * 2 + 1]i32) }
|};
  [%expect {|
    function l $f() {
    @start
        ret 28
    }
    |}]

let%expect_test "codegen: suffixed array size" =
  run_codegen {|
func f() usize { return sizeof([2u8]i32) }
|};
  [%expect {|
    function l $f() {
    @start
        ret 8
    }
    |}]

let%expect_test "codegen: sizeof of a comptime sized array folds" =
  run_codegen
    {|
comptime N: i32 = 4
comptime S: usize = sizeof([N]i32)
func f() usize { return S }
|};
  [%expect {|
    function l $f() {
    @start
        ret 16
    }
    |}]

let%expect_test "codegen: struct field sized by a later comptime" =
  run_codegen
    {|
struct S { buf: [N]i32, tail: i32 }
comptime N: i32 = 2
func f(s: *S) i32 { return s.tail }
|};
  [%expect
    {|
    type :S = { w 2, w }

    function w $f(l %t0) {
    @start
        %s =l alloc8 8
        storel %t0, %s
        %t1 =l loadl %s
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        %t4 =l add %t1, 8
        %t5 =w loadsw %t4
        ret %t5
    }
    |}]

let%expect_test "codegen: global let arithmetic" =
  run_codegen {|
let A: i32 = 2 + 3
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

let%expect_test "codegen: global let arithmetic referencing another let" =
  run_codegen
    {|
let A: i32 = 5
let B: i32 = A * 2 + 1
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

let%expect_test "codegen: global let float arithmetic" =
  run_codegen {|
let A: f64 = 1.5 + 2.5
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

let%expect_test "codegen: global let cast arithmetic" =
  run_codegen {|
let A: f32 = (1 + 2) as f32
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
  (*q).x = 8
  return p.x
}
|};
  [%expect
    {|
    type :pt = { w, w }

    function w $f() {
    @start
        %p =l alloc8 8
        %q =l alloc8 8
        storew 34, %p
        %t0 =l add %p, 4
        storew 56, %t0
        %t1 =l copy %p
        storel %t1, %q
        %t2 =l loadl %q
        %t3 =w ceql %t2, 0
        jnz %t3, @null.fail.4, @null.ok.4
    @null.fail.4
        call $ripe_panic_null()
        hlt
    @null.ok.4
        storew 8, %t2
        %t5 =w loadsw %p
        ret %t5
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
        %q =l alloc8 8
        storew 10, %x
        %t0 =l copy %x
        storel %t0, %q
        %t1 =l loadl %q
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        storew 42, %t1
        %t4 =w loadsw %x
        ret %t4
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
        %compound.p.0 =l alloc8 8
        storew 1, %p
        %t0 =l add %p, 4
        storew 2, %t0
        %t1 =l copy %p
        storel %t1, %compound.p.0
        %t2 =l loadl %compound.p.0
        %t3 =w ceql %t2, 0
        jnz %t3, @null.fail.4, @null.ok.4
    @null.fail.4
        call $ripe_panic_null()
        hlt
    @null.ok.4
        %t5 =l loadl %compound.p.0
        %t7 =w ceql %t5, 0
        jnz %t7, @null.fail.8, @null.ok.8
    @null.fail.8
        call $ripe_panic_null()
        hlt
    @null.ok.8
        %t6 =w loadsw %t5
        %t9 =w add %t6, 5
        storew %t9, %t2
        %t10 =w loadsw %p
        ret %t10
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
        %q =l alloc8 8
        %compound.p.0 =l alloc8 8
        storew 10, %x
        %t0 =l copy %x
        storel %t0, %q
        %t1 =l loadl %q
        %t2 =l copy %t1
        storel %t2, %compound.p.0
        %t3 =l loadl %compound.p.0
        %t4 =w ceql %t3, 0
        jnz %t4, @null.fail.5, @null.ok.5
    @null.fail.5
        call $ripe_panic_null()
        hlt
    @null.ok.5
        %t6 =l loadl %compound.p.0
        %t8 =w ceql %t6, 0
        jnz %t8, @null.fail.9, @null.ok.9
    @null.fail.9
        call $ripe_panic_null()
        hlt
    @null.ok.9
        %t7 =w loadsw %t6
        %t10 =w add %t7, 5
        storew %t10, %t3
        %t11 =w loadsw %x
        ret %t11
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
        %px =l alloc8 8
        storew 7, %p
        %t0 =l add %p, 4
        storew 0, %t0
        %t1 =l copy %p
        storel %t1, %px
        %t2 =l loadl %px
        %t4 =w ceql %t2, 0
        jnz %t4, @null.fail.5, @null.ok.5
    @null.fail.5
        call $ripe_panic_null()
        hlt
    @null.ok.5
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
        %pt1 =l alloc8 8
        %pt2 =l alloc8 8
        storew 3, %p
        %t0 =l add %p, 4
        storew 0, %t0
        %t1 =l copy %p
        storel %t1, %pt1
        %t2 =l loadl %pt1
        %t3 =l copy %t2
        storel %t3, %pt2
        %t4 =l loadl %pt2
        %t5 =w ceql %t4, 0
        jnz %t5, @null.fail.6, @null.ok.6
    @null.fail.6
        call $ripe_panic_null()
        hlt
    @null.ok.6
        %t7 =w loadsw %t4
        ret %t7
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
        %p =l alloc8 8
        storew 1, %arr
        %t0 =l add %arr, 4
        storew 2, %t0
        %t1 =l add %arr, 8
        storew 3, %t1
        %t2 =l add %arr, 12
        storew 4, %t2
        %t3 =l extsw 2
        %t4 =w cugel %t3, 4
        jnz %t4, @bounds.fail.5, @bounds.ok.5
    @bounds.fail.5
        call $ripe_panic_bounds(l %t3, l 4)
        hlt
    @bounds.ok.5
        %t6 =l mul %t3, 4
        %t7 =l add %arr, %t6
        %t8 =l copy %t7
        storel %t8, %p
        %t9 =l loadl %p
        %t11 =w ceql %t9, 0
        jnz %t11, @null.fail.12, @null.ok.12
    @null.fail.12
        call $ripe_panic_null()
        hlt
    @null.ok.12
        %t10 =w loadsw %t9
        ret %t10
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
        %t4 =w csltw %t3, 0
        jnz %t4, @negshift.fail.5, @negshift.ok.5
    @negshift.fail.5
        call $ripe_panic_shift()
        hlt
    @negshift.ok.5
        %t6 =w cultw %t3, 32
        %t7 =w sub %t6, 1
        %t8 =w or %t3, %t7
        %t9 =w sar %t2, %t8
        ret %t9
    }
    |}]

let%expect_test "codegen: signed right shift by a constant out of range count" =
  run_codegen "func f(a: i32) i32 { return a >> 40 }";
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w sar %t1, 31
        ret %t2
    }
    |}]

let%expect_test "codegen: signed right shift by a constant in range count" =
  run_codegen "func f(a: i32) i32 { return a >> 5 }";
  [%expect
    {|
    function w $f(w %t0) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %t1 =w loadsw %a
        %t2 =w sar %t1, 5
        ret %t2
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
        %t4 =w cultw %t3, 32
        %t5 =w shr %t2, %t3
        %t7 =w sub 0, %t4
        %t6 =w and %t5, %t7
        ret %t6
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

let%expect_test "codegen: else if chain" =
  run_codegen
    {|
func f(x: i32) i32 {
  if x < 0 { return 0 } else if x == 0 { return 1 } else { return 2 }
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
        %compound.p.0 =l alloc8 8
        storew 3, %n
        %t0 =l copy %n
        storel %t0, %compound.p.0
        %t1 =l loadl %compound.p.0
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        %t4 =l loadl %compound.p.0
        %t6 =w ceql %t4, 0
        jnz %t6, @null.fail.7, @null.ok.7
    @null.fail.7
        call $ripe_panic_null()
        hlt
    @null.ok.7
        %t5 =w loadsw %t4
        %t8 =w mul %t5, 4
        storew %t8, %t1
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
        %compound.p.0 =l alloc8 8
        storew 12, %n
        %t0 =l copy %n
        storel %t0, %compound.p.0
        %t1 =l loadl %compound.p.0
        %t2 =w ceql %t1, 0
        jnz %t2, @null.fail.3, @null.ok.3
    @null.fail.3
        call $ripe_panic_null()
        hlt
    @null.ok.3
        %t4 =l loadl %compound.p.0
        %t6 =w ceql %t4, 0
        jnz %t6, @null.fail.7, @null.ok.7
    @null.fail.7
        call $ripe_panic_null()
        hlt
    @null.ok.7
        %t5 =w loadsw %t4
        %t9 =w ceqw 4, 0
        jnz %t9, @divzero.fail.10, @divzero.ok.10
    @divzero.fail.10
        call $ripe_panic_divzero()
        hlt
    @divzero.ok.10
        %t12 =w ceqw 4, -1
        jnz %t12, @div.neg1.11, @div.norm.11
    @div.neg1.11
        %t13 =w sub 0, %t5
        jmp @div.join.11
    @div.norm.11
        %t14 =w div %t5, 4
        jmp @div.join.11
    @div.join.11
        %t8 =w phi @div.neg1.11 %t13, @div.norm.11 %t14
        storew %t8, %t1
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
        blit %t0, %p, 8
        %t1 =w loadsw %p
        ret %t1
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
        %y =l alloc4 4
        %x =l alloc4 4
        storew %t0, %x
    @if.cond1_0
        %t2 =w loadsw %x
        %t3 =w csltw %t2, 0
        jnz %t3, @if.then1_0, @if.else1
    @if.then1_0
        ret
    @if.else1
        jmp @if.end1
    @if.end1
        %t4 =w loadsw %x
        %t5 =w add %t4, 1
        storew %t5, %y
        ret
    }
    |}]

let%expect_test "codegen: division by zero in constant" =
  run_codegen {|let X: i32 = 1 / 0|};
  [%expect
    {|
    error: division by zero in constant
      at <test>:1:14
        let X: i32 = 1 / 0
                     ^~~~~
    |}]

let%expect_test "codegen: remainder by zero in constant" =
  run_codegen {|let X: i32 = 1 % 0|};
  [%expect
    {|
    error: remainder by zero in constant
      at <test>:1:14
        let X: i32 = 1 % 0
                     ^~~~~
    |}]

let%expect_test "codegen: function address cast to int in constant" =
  run_codegen {|
func g() i32 { return 3 }
let A: i64 = g as i64
|};
  [%expect
    {|
    error: unsupported constant expression
      at <test>:3:14
        let A: i64 = g as i64
                     ^
    help: constant initializers must fold to a compile-time value
    |}]

let%expect_test "codegen: string in constant arithmetic" =
  run_codegen {|
let S: cstr = "x"
let N: i64 = S as i64 + 1
|};
  [%expect
    {|
    error: unsupported constant expression
      at <test>:3:14
        let N: i64 = S as i64 + 1
                     ^
    help: constant initializers must fold to a compile-time value
    |}]

(* internal invariants: the typechecker keeps these unreachable from source, so
   drive the codegen helpers directly *)

let empty_structs () : (Ripe.Symbol.key, Ripe.Types.ty list) Hashtbl.t =
  Hashtbl.create 0

let%expect_test "codegen ICE: TVoid has no QBE base type" =
  expect_errors (fun () -> ignore (Ripe.Codegen_qbe.qbe_ty Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no QBE base type
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no alignment" =
  expect_errors (fun () ->
      ignore (Ripe.Types.ty_align (empty_structs ()) Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no alignment
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no size" =
  expect_errors (fun () ->
      ignore (Ripe.Types.ty_size (empty_structs ()) Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no size
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no alloc instruction" =
  expect_errors (fun () ->
      ignore (Ripe.Codegen_qbe.alloc_instr Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no alloc instruction
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no load instruction" =
  expect_errors (fun () -> ignore (Ripe.Codegen_qbe.qbe_load Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no load instruction
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no store instruction" =
  expect_errors (fun () -> ignore (Ripe.Codegen_qbe.qbe_store Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no store instruction
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: TVoid has no extended type" =
  expect_errors (fun () ->
      ignore (Ripe.Codegen_qbe.qbe_ext_ty Ripe.Types.TVoid));
  [%expect
    {|
    error: internal compiler error
    TVoid has no extended type
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: missing struct layout in alignment" =
  expect_errors (fun () ->
      ignore
        (Ripe.Types.ty_align (empty_structs ())
           (Ripe.Types.TStruct (Ripe.Qname.unresolved "Foo", []))));
  [%expect
    {|
    error: internal compiler error
    no layout recorded for struct Foo
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen ICE: missing struct layout in size" =
  expect_errors (fun () ->
      ignore
        (Ripe.Types.ty_size (empty_structs ())
           (Ripe.Types.TStruct (Ripe.Qname.unresolved "Foo", []))));
  [%expect
    {|
    error: internal compiler error
    no layout recorded for struct Foo
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "codegen: assignment operator in constant" =
  expect_errors (fun () ->
      ignore
        (Ripe.Const_eval.fold_const_binop Ripe.Ast.dummy_span Ripe.Ast.Assign
           ~result_ty:(Ripe.Types.TInt Ripe.Types.I32)
           ~operand_ty:(Ripe.Types.TInt Ripe.Types.I32)
           (Ripe.Const_eval.Ni32 1l) (Ripe.Const_eval.Ni32 2l)));
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
        (Ripe.Const_eval.fold_const_binop Ripe.Ast.dummy_span Ripe.Ast.Lshift
           ~result_ty:(Ripe.Types.TFloat Ripe.Types.F64)
           ~operand_ty:(Ripe.Types.TFloat Ripe.Types.F64)
           (Ripe.Const_eval.Nf 1.0) (Ripe.Const_eval.Nf 2.0)));
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
  let _s: cstr = "hi"
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
        %r =l alloc4 4
        storel $add, %op
        %t0 =l loadl %op
        %t1 =w call %t0(w 2, w 3)
        storew %t1, %r
        %t2 =w loadsw %r
        ret %t2
    }
    |}]

let%expect_test "codegen: call through a struct field fn ptr" =
  run_codegen
    {|
func add(a: i32, b: i32) i32 { return a + b }
struct Ops { combine: (i32, i32) i32 }
func main() i32 {
  var c = Ops { combine: add }
  return c.combine(6, 7)
}
|};
  [%expect
    {|
    type :Ops = { l }

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
        %c =l alloc8 8
        storel $add, %c
        %t0 =l loadl %c
        %t1 =w call %t0(w 6, w 7)
        ret %t1
    }
    |}]

let%expect_test "codegen: call through a fn ptr array element" =
  run_codegen
    {|
func mul(a: i32, b: i32) i32 { return a * b }
func main() i32 {
  var h: [1](i32, i32) i32 = undefined
  h[0] = mul
  return h[0](4, 5)
}
|};
  [%expect
    {|
    function w $mul(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
        %t2 =w loadsw %a
        %t3 =w loadsw %b
        %t4 =w mul %t2, %t3
        ret %t4
    }

    export function w $main() {
    @start
        %h =l alloc8 8
        %t0 =l extsw 0
        %t1 =w cugel %t0, 1
        jnz %t1, @bounds.fail.2, @bounds.ok.2
    @bounds.fail.2
        call $ripe_panic_bounds(l %t0, l 1)
        hlt
    @bounds.ok.2
        %t3 =l mul %t0, 8
        %t4 =l add %h, %t3
        storel $mul, %t4
        %t5 =l extsw 0
        %t6 =w cugel %t5, 1
        jnz %t6, @bounds.fail.7, @bounds.ok.7
    @bounds.fail.7
        call $ripe_panic_bounds(l %t5, l 1)
        hlt
    @bounds.ok.7
        %t8 =l mul %t5, 8
        %t9 =l add %h, %t8
        %t10 =l loadl %t9
        %t11 =w call %t10(w 4, w 5)
        ret %t11
    }
    |}]

let%expect_test "codegen: call the result of a call returning a fn ptr" =
  run_codegen
    {|
func mul(a: i32, b: i32) i32 { return a * b }
func pick() (i32, i32) i32 { return mul }
func main() i32 { return pick()(5, 6) }
|};
  [%expect
    {|
    function w $mul(w %t0, w %t1) {
    @start
        %a =l alloc4 4
        storew %t0, %a
        %b =l alloc4 4
        storew %t1, %b
        %t2 =w loadsw %a
        %t3 =w loadsw %b
        %t4 =w mul %t2, %t3
        ret %t4
    }

    function l $pick() {
    @start
        ret $mul
    }

    export function w $main() {
    @start
        %t0 =l call $pick()
        %t1 =w call %t0(w 5, w 6)
        ret %t1
    }
    |}]

let%expect_test "codegen: call through a dereferenced fn ptr" =
  run_codegen
    {|
func add(a: i32, b: i32) i32 { return a + b }
func main() i32 {
  var op = add
  var p = &op
  return (*p)(4, 5)
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
        %p =l alloc8 8
        storel $add, %op
        %t0 =l copy %op
        storel %t0, %p
        %t1 =l loadl %p
        %t3 =w ceql %t1, 0
        jnz %t3, @null.fail.4, @null.ok.4
    @null.fail.4
        call $ripe_panic_null()
        hlt
    @null.ok.4
        %t2 =l loadl %t1
        %t5 =w call %t2(w 4, w 5)
        ret %t5
    }
    |}]

let%expect_test "codegen: loop var and let allocs hoisted to @start" =
  run_codegen
    {|
func f() {
  var i: i32 = 0
  while i < 3 {
    var x: i32 = i
    let y: i32 = x
    i = y
  }
}
|};
  [%expect
    {|
    function $f() {
    @start
        %i =l alloc4 4
        %x =l alloc4 4
        %y =l alloc4 4
        storew 0, %i
    @loop.body0
    @if.cond1_0
        %t2 =w loadsw %i
        %t3 =w csltw %t2, 3
        jnz %t3, @if.else1, @if.then1_0
    @if.then1_0
        jmp @loop.end0
    @if.else1
        jmp @if.end1
    @if.end1
        %t4 =w loadsw %i
        storew %t4, %x
        %t5 =w loadsw %x
        storew %t5, %y
        %t6 =w loadsw %y
        storew %t6, %i
        jmp @loop.body0
    @loop.end0
        ret
    }
    |}]

let%expect_test "codegen: checked cast range guard" =
  run_codegen {|
func f() u8 {
  var x: i32 = 300
  return x as! u8
}
|};
  [%expect
    {|
    function w $f() {
    @start
        %x =l alloc4 4
        storew 300, %x
        %t0 =w loadsw %x
        %t1 =l extsw %t0
        %t2 =w csltl %t1, 0
        %t3 =w csgtl %t1, 255
        %t4 =w or %t2, %t3
        jnz %t4, @cast.fail.5, @cast.ok.5
    @cast.fail.5
        call $ripe_panic_cast()
        hlt
    @cast.ok.5
        %t6 =w extub %t0
        ret %t6
    }
    |}]

let%expect_test "codegen: never function lowers to hlt" =
  run_codegen
    {|
extern func exit(code: i32) never
func spin() never { exit(3) }
|};
  [%expect
    {|
    function $spin() {
    @start
        call $exit(w 3)
        hlt
    }
    |}]
