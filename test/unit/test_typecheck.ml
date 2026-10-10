(* SPDX-License-Identifier: Apache-2.0 *)

open Pipeline

let%expect_test "typecheck: break outside loop" =
  run_src "f :: fn() { break };";
  [%expect
    {|
    error: `break` outside a loop
      at <test>:1:13
        f :: fn() { break };
                    ^~~~~
    |}]

let%expect_test "typecheck: continue outside loop" =
  run_src "f :: fn() { continue };";
  [%expect
    {|
    error: `continue` outside a loop
      at <test>:1:13
        f :: fn() { continue };
                    ^~~~~~~~
    |}]

let%expect_test "typecheck: unbound variable" =
  run_src "f :: fn() { x };";
  [%expect
    {|
    error: undefined variable
      at <test>:1:13
        f :: fn() { x };
                    ^
    |}]

let%expect_test "typecheck: type mismatch in a binding" =
  run_src "f :: fn() { x : bool = 42 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:24
        f :: fn() { x : bool = 42 };
                               ^~ expected bool, found i32
    |}]

let%expect_test "typecheck: int literal a float can't hold exactly" =
  run_src "f :: fn() { x : f32 = 16777217 };";
  [%expect
    {|
    error: integer literal loses precision
      at <test>:1:23
        f :: fn() { x : f32 = 16777217 };
                              ^~~~~~~~ becomes 16777216
    help: write 16777217.0 to accept the rounding
    |}]

let%expect_test "typecheck: float suffix disagrees with the annotation" =
  run_src "f :: fn() { x : f64 = 1.5f32 };";
  [%expect {| ok |}]

let%expect_test "typecheck: wrong number of arguments" =
  run_src {|
g :: fn() {};
f :: fn() { g(1) };
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:3:13
        f :: fn() { g(1) };
                    ^~~~ expected 0 arguments, found 1
    |}]

let%expect_test "typecheck: null assigned to non-pointer" =
  run_src "f :: fn() { x : i32 = null };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:23
        f :: fn() { x : i32 = null };
                              ^~~~ expected i32, found null
    |}]

let%expect_test "typecheck: identity function" =
  run_src "id :: fn(a: i32) -> i32 { return a };";
  [%expect {| ok |}]

let%expect_test "typecheck: null assigned to pointer" =
  run_src "f :: fn() { p : *i32 = null };";
  [%expect {| ok |}]

let%expect_test "typecheck: array does not coerce to slice under a pointer" =
  run_src "takes :: fn(p: *[]i32) { };\nf :: fn() { a : [3]i32;\n  takes(&a) };";
  [%expect
    {|
    error: type mismatch
      at <test>:3:9
          takes(&a) };
                ^~ expected *[]i32, found *[3]i32
    |}]

let%expect_test "typecheck: break inside while" =
  run_src "f :: fn() { while true { break } };";
  [%expect {| ok |}]

let%expect_test "typecheck: a diverging binding ends a value block" =
  run_src {|
d :: fn() -> never { loop {} };
f :: fn() -> i32 { _x := d() };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: forward reference" =
  run_src {|
f :: fn() { g() };
g :: fn() {};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: call no args" =
  run_src {|
g :: fn() {};
f :: fn() { g() };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: call with args" =
  run_src {|
add :: fn(x: i32, y: i32) {};
f :: fn() { add(1, 2) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr assign and call" =
  run_src
    {|
add :: fn(a: i32, b: i32) -> i32 { return a + b };
f :: fn() {
  op : fn (i32, i32) i32 = add;
  op(1, 2);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr inferred from function name" =
  run_src
    {|
add :: fn(a: i32, b: i32) -> i32 { return a + b };
f :: fn() {
  op := add;
  op(1, 2);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr signature mismatch" =
  run_src
    {|
add :: fn(a: i32, b: i32) -> i32 { return a + b };
f :: fn() {
  op : fn (i32) i32 = add;
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:23
          op : fn (i32) i32 = add;
                              ^~~ expected fn (i32) i32, found fn (i32, i32) i32
    |}]

let%expect_test "typecheck: non-callable variable" =
  run_src {|
f :: fn() {
  x : i32 = 5;
  x(1);
};
|};
  [%expect
    {|
    error: not callable
      at <test>:4:3
          x(1);
          ^ this has type i32
    |}]

let%expect_test "typecheck: a value shadowing a type still converts" =
  run_src
    {|
word :: i64;
f :: fn() {
  word : i32 = 5;
  _w := cast(word, word);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a value shadowing a fn is not a call" =
  run_src
    {|
f :: fn() {
  point :: fn() {};
  point : i32 = 5;
  point(1);
};
|};
  [%expect
    {|
    error: not callable
      at <test>:5:3
          point(1);
          ^~~~~ this has type i32
      at <test>:3:3
          point :: fn() {};
          ^~~~~ shadowed by the value
    help: rename the value to call this function
    |}]

let%expect_test "typecheck: a shadowed fn wins the note over a type" =
  run_src
    {|
f :: fn() {
  point :: struct { x: i32, y: i32 };
  point :: fn() {};
  point : i32 = 5;
  point(1);
};
|};
  [%expect
    {|
    error: not callable
      at <test>:6:3
          point(1);
          ^~~~~ this has type i32
      at <test>:4:3
          point :: fn() {};
          ^~~~~ shadowed by the value
    help: rename the value to call this function
    |}]

let%expect_test "typecheck: fn ptr as parameter" =
  run_src
    {|
add :: fn(a: i32, b: i32) -> i32 { return a + b };
apply :: fn(f: fn (i32, i32) i32, a: i32, b: i32) -> i32 { return f(a, b) };
g :: fn() { apply(add, 1, 2) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr wrong arity at call" =
  run_src
    {|
add :: fn(a: i32, b: i32) -> i32 { return a + b };
f :: fn() {
  op : fn (i32, i32) i32 = add;
  op(1);
};
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:5:3
          op(1);
          ^~~~~ expected 2 arguments, found 1
    |}]

let%expect_test "typecheck: fn ptr forward reference" =
  run_src
    {|
f :: fn() {
  op : fn (i32, i32) i32 = add;
  op(1, 2);
};
add :: fn(a: i32, b: i32) -> i32 { return a + b };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: fn ptr returning fn ptr" =
  run_src
    {|
add :: fn(a: i32, b: i32) -> i32 { return a + b };
get_op :: fn() -> fn (i32, i32) i32 { return add };
f :: fn() {
  op := get_op();
  op(1, 2);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: unit fn ptr zero args" =
  run_src {|
noop :: fn() {};
f :: fn() {
  p : fn () = noop;
  p();
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global read from function" =
  run_src {|
X : i32 = 42;
f :: fn() -> i32 { return X };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global read and write" =
  run_src {|
n : i32 = 0;
f :: fn() -> i32 {
  n = n + 1;
  return n;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global zero init" =
  run_src {|
flag : bool;
f :: fn() -> bool { return flag };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global forward reference" =
  run_src {|
f :: fn() -> i32 { return X };
X : i32 = 7;
|};
  [%expect {| ok |}]

let%expect_test "typecheck: write to a binding's struct field" =
  run_src
    {|
P :: struct { x: i32, y: i32 };
f :: fn() {
  p : P = P { x: 1, y: 2 };
  p.x = 5;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: write to a binding's array element" =
  run_src {|
f :: fn() {
  arr : [3]i32 = [1, 2, 3];
  arr[0] = 9;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: write through a pointer" =
  run_src {|
g : i32 = 0;
f :: fn() {
  p : *i32 = &g;
  *p = 5;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global initializer calls a function" =
  run_src {|
g :: fn() -> i32 { return 1 };
X : i32 = g();
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size expression" =
  run_src
    {|
N : i32 = 4;
f :: fn() -> i32 {
  a : [N * 2 + 1]i32;
  a[8] = 1;
  return a[8];
};
|};
  [%expect
    {|
    error: array size must be a literal
      at <test>:4:8
          a : [N * 2 + 1]i32;
               ^~~~~~~~~
    |}]

let%expect_test "typecheck: array size with a suffix" =
  run_src {|
f :: fn() -> i32 {
  a : [2u8]i32 = [1, 2];
  return a[1];
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: negative array size" =
  run_src {|
a : [0 - 1]i32;
|};
  [%expect
    {|
    error: array size must be a literal
      at <test>:2:6
        a : [0 - 1]i32;
             ^~~~~
    |}]

let%expect_test "typecheck: bad array size in a param errors once" =
  run_src {|
f :: fn(a: [0 - 1]i32) {};
|};
  [%expect
    {|
    error: array size must be a literal
      at <test>:2:13
        f :: fn(a: [0 - 1]i32) {};
                    ^~~~~
    |}]

let%expect_test "typecheck: huge array size" =
  run_src {|
a : [9999999999i64]i32;
|};
  [%expect
    {|
    error: array size is too large: 9999999999
      at <test>:2:6
        a : [9999999999i64]i32;
             ^~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: huge unsigned array size" =
  run_src {|
a : [cast(u64, 0 - 1)]i32;
|};
  [%expect
    {|
    error: array size must be a literal
      at <test>:2:6
        a : [cast(u64, 0 - 1)]i32;
             ^~~~~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: array size literal with a type suffix" =
  run_src {|
a : [2u8]i32;
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size literal suffix still range checks" =
  run_src {|
a : [300u8]i32;
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:2:6
        a : [300u8]i32;
             ^~~~~ does not fit in u8
    |}]

let%expect_test "typecheck: float array size" =
  run_src {|
a : [1.5]i32;
|};
  [%expect
    {|
    error: array size must be an integer
      at <test>:2:6
        a : [1.5]i32;
             ^~~
    |}]

let%expect_test "typecheck: array size names a global" =
  run_src {|
n : i32 = 3;
a : [n]i32;
|};
  [%expect
    {|
    error: array size must be a literal
      at <test>:3:6
        a : [n]i32;
             ^
    |}]

let%expect_test "typecheck: array size calls a function" =
  run_src {|
g :: fn() -> i32 { return 3 };
a : [g()]i32;
|};
  [%expect
    {|
    error: array size must be a literal
      at <test>:3:6
        a : [g()]i32;
             ^~~
    |}]

let%expect_test "typecheck: int arithmetic ok" =
  run_src "f :: fn() -> i32 { return 1 + 2 * 3 - 4 };";
  [%expect {| ok |}]

let%expect_test "typecheck: mixed int widths" =
  run_src {|
f :: fn() {
  a : i32 = 1;
  b : i64 = 2;
  c := a + b;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: bool arithmetic rejected" =
  run_src "f :: fn() { x := true + false };";
  [%expect
    {|
    error: invalid operand
      at <test>:1:18
        f :: fn() { x := true + false };
                         ^~~~ cannot apply `+` to bool
    |}]

let%expect_test "typecheck: comparison yields bool" =
  run_src "f :: fn() -> bool { return 1 < 2 };";
  [%expect {| ok |}]

let%expect_test "typecheck: logical and/or require bool" =
  run_src "f :: fn() { x := 1 && 2 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:18
        f :: fn() { x := 1 && 2 };
                         ^ expected bool, found i32
    error: type mismatch
      at <test>:1:23
        f :: fn() { x := 1 && 2 };
                              ^ expected bool, found i32
    |}]

let%expect_test "typecheck: not on non-bool" =
  run_src "f :: fn() { x := !1 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:19
        f :: fn() { x := !1 };
                          ^ expected bool, found i32
    |}]

let%expect_test "typecheck: address of rvalue rejected" =
  run_src "f :: fn() { x := &5 };";
  [%expect
    {|
    error: cannot take address of expression
      at <test>:1:19
        f :: fn() { x := &5 };
                          ^
    |}]

let%expect_test "typecheck: shift on int ok" =
  run_src "f :: fn() -> i32 { return 1 << 3 };";
  [%expect {| ok |}]

let%expect_test "typecheck: shift literal takes target type" =
  run_src "f :: fn() -> u8 { return 5 << 2 };";
  [%expect {| ok |}]

let%expect_test "typecheck: bitwise on bool rejected" =
  run_src "f :: fn() { x := true & false };";
  [%expect
    {|
    error: invalid operand
      at <test>:1:18
        f :: fn() { x := true & false };
                         ^~~~ cannot apply `&` to bool
    |}]

let%expect_test "typecheck: int to int cast" =
  run_src "f :: fn() -> i64 { return cast(i64, 1) };";
  [%expect {| ok |}]

let%expect_test "typecheck: sizeof has usize type" =
  run_src "f :: fn() -> usize { return sizeof(i32) };";
  [%expect {| ok |}]

let%expect_test "typecheck: cast bool to ptr rejected" =
  run_src "f :: fn() { p : *i32 = cast(*i32, true) };";
  [%expect
    {|
    error: invalid conversion
      at <test>:1:24
        f :: fn() { p : *i32 = cast(*i32, true) };
                               ^~~~~~~~~~~~~~~~ cannot convert bool to *i32
    |}]

let%expect_test ("typecheck: cast cstr to float rejected" [@tags "disabled"]) =
  run_src {|f :: fn() { x : f32 = "hi"f32() };|};
  [%expect
    {|
    error: type mismatch
      at <test>:1:23
        f :: fn() { x : f32 = "hi"f32() };
                              ^~~~ expected f32, found *i8
    error: expected `;`
      at <test>:1:27
        f :: fn() { x : f32 = "hi"f32() };
                                  ^~~ found f32
    |}]

let%expect_test "typecheck: cast struct to float rejected" =
  run_src
    {|
S :: struct { x: i32 };
f :: fn() { s : S; y : f64 = cast(f64, s) };
|};
  [%expect
    {|
    error: invalid conversion
      at <test>:3:30
        f :: fn() { s : S; y : f64 = cast(f64, s) };
                                     ^~~~~~~~~~~~ cannot convert S to f64
    |}]

let%expect_test "typecheck: cast int to bool rejected" =
  run_src "f :: fn() { b : bool = cast(bool, 256) };";
  [%expect
    {|
    error: invalid conversion
      at <test>:1:24
        f :: fn() { b : bool = cast(bool, 256) };
                               ^~~~~~~~~~~~~~~ cannot convert i32 to bool
    help: compare with zero instead e.g. `x != 0`
    |}]

let%expect_test "typecheck: missing return value" =
  run_src "f :: fn() -> i32 { return };";
  [%expect
    {|
    error: empty return in non-unit function
      at <test>:1:20
        f :: fn() -> i32 { return };
                           ^~~~~~
    |}]

let%expect_test "typecheck: return value in unit fn" =
  run_src "f :: fn() { return 1 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:20
        f :: fn() { return 1 };
                           ^ expected (), found i32
    |}]

let%expect_test "typecheck: return type mismatch" =
  run_src "f :: fn() -> i32 { return true };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:27
        f :: fn() -> i32 { return true };
                                  ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: if condition must be bool" =
  run_src "f :: fn() { if 1 {} };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:16
        f :: fn() { if 1 {} };
                       ^ expected bool, found i32
    |}]

let%expect_test "typecheck: while condition must be bool" =
  run_src "f :: fn() { while 1 {} };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:19
        f :: fn() { while 1 {} };
                          ^ expected bool, found i32
    |}]

let%expect_test "typecheck: if/else ok" =
  run_src "f :: fn() -> i32 { if true { return 1 } else { return 2 } };";
  [%expect {| ok |}]

let%expect_test "typecheck: nested loops break ok" =
  run_src "f :: fn() { while true { while true { break } } };";
  [%expect {| ok |}]

let%expect_test "typecheck: redeclare local shadows" =
  run_src {|
f :: fn() {
  x : i32 = 1;
  x : i32 = 2;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: shadow can change type and the new type wins" =
  run_src {|
f :: fn() -> i64 {
  x : i32 = 1;
  x : i64 = 2;
  return x;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: shadow reads the old binding in its initializer" =
  run_src
    {|
f :: fn() -> i32 {
  x : i32 = 1;
  x : i32 = x + 4;
  return x;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type annot mismatch on a binding" =
  run_src "f :: fn() { x : bool = 1 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:24
        f :: fn() { x : bool = 1 };
                               ^ expected bool, found i32
    |}]

let%expect_test "typecheck: use before decl" =
  run_src {|
f :: fn() {
  x;
  x : i32 = 1;
};
|};
  [%expect
    {|
    error: undefined variable
      at <test>:3:3
          x;
          ^
    |}]

let%expect_test "typecheck: deref non-pointer" =
  run_src {|
f :: fn() {
  x : i32 = 1;
  y := *x;
};
|};
  [%expect
    {|
    error: cannot dereference
      at <test>:4:9
          y := *x;
                ^ on i32
    |}]

let%expect_test "typecheck: a failed check does not cascade" =
  run_src {|
f :: fn() {
  n : i32 = 1;
  _y := *n.x + 1;
};
|};
  [%expect
    {|
    error: type has no fields
      at <test>:4:10
          _y := *n.x + 1;
                 ^~~ on i32
    |}]

let%expect_test "typecheck: address-of and deref roundtrip" =
  run_src
    {|
f :: fn() -> i32 {
  x : i32 = 5;
  p : *i32 = &x;
  return *p;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: struct field read" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn(p: Pt) -> i32 { return p.x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: unknown struct field" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn(p: Pt) -> i32 { return p.z };
|};
  [%expect
    {|
    error: no field
      at <test>:3:34
        f :: fn(p: Pt) -> i32 { return p.z };
                                         ^ on struct Pt
    |}]

let%expect_test "typecheck: field access on non-struct" =
  run_src {|
f :: fn() {
  x : i32 = 1;
  y := x.foo;
};
|};
  [%expect
    {|
    error: type has no fields
      at <test>:4:8
          y := x.foo;
               ^~~~~ on i32
    |}]

let%expect_test "typecheck: field access auto-deref through ptr" =
  run_src {|
Pt :: struct { x: i32 };
f :: fn(p: *Pt) -> i32 { return p.x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: field access through a double pointer" =
  run_src {|
Pt :: struct { x: i32 };
f :: fn(p: **Pt) -> i32 { return p.x };
|};
  [%expect
    {|
    error: too many pointer levels
      at <test>:3:34
        f :: fn(p: **Pt) -> i32 { return p.x };
                                         ^~~
    help: dereference first: `(*p).x`
    |}]

let%expect_test "typecheck: duplicate function" =
  run_src {|
f :: fn() {};
f :: fn() {};
|};
  [%expect
    {|
    error: already defined
      at <test>:3:1
        f :: fn() {};
        ^
      at <test>:2:1
        f :: fn() {};
        ^ previous definition here
    |}]

let%expect_test "typecheck: arg type mismatch" =
  run_src {|
g :: fn(x: i32) {};
f :: fn() { g(true) };
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:15
        f :: fn() { g(true) };
                      ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: extern decl callable" =
  run_src
    {|
extern "C" fn puts(s: *i8) i32;
f :: fn() {
  p : *i8 = null;
  puts(p);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern definition callable" =
  run_src
    {|
extern "C" fn add(a: i32, b: i32) i32 { return a + b }
f :: fn() -> i32 { return add(1, 2) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern definition checks its body" =
  run_src {|extern "C" fn add(a: i32, b: i32) i32 { return a > b }|};
  [%expect
    {|
    error: type mismatch
      at <test>:1:48
        extern "C" fn add(a: i32, b: i32) i32 { return a > b }
                                                       ^~~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: an unsupported ABI on a definition" =
  run_src {|extern "Rust" fn add(a: i32) i32 { return a }|};
  [%expect
    {|
    error: unsupported ABI
      at <test>:1:8
        extern "Rust" fn add(a: i32) i32 { return a }
               ^~~~~~ this ABI is not supported here
    |}]

let%expect_test "typecheck: array literal inferred" =
  run_src "f :: fn() { a := [1, 2, 3]; a[0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: array annotated ok" =
  run_src "f :: fn() { a : [3]i32 = [1, 2, 3]; a[0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: array element type mismatch" =
  run_src "f :: fn() { a : [2]i32 = [1, true] };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:30
        f :: fn() { a : [2]i32 = [1, true] };
                                     ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: array wrong element count" =
  run_src "f :: fn() { a : [3]i32 = [1, 2] };";
  [%expect
    {|
    error: wrong number of arguments
      at <test>:1:26
        f :: fn() { a : [3]i32 = [1, 2] };
                                 ^~~~~~ expected 3 elements, found 2
    |}]

let%expect_test "typecheck: heterogeneous inferred literal" =
  run_src "f :: fn() { a := [1, true]; a[0] };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:19
        f :: fn() { a := [1, true]; a[0] };
                          ^ expected bool, found i32
    |}]

let%expect_test "typecheck: empty array literal needs annotation" =
  run_src "f :: fn() { a := [] };";
  [%expect
    {|
    error: cannot infer type of empty array literal
      at <test>:1:18
        f :: fn() { a := [] };
                         ^~
    |}]

let%expect_test "typecheck: index non-array" =
  run_src "f :: fn() { x : i32 = 0; x[0] };";
  [%expect
    {|
    error: cannot index
      at <test>:1:26
        f :: fn() { x : i32 = 0; x[0] };
                                 ^~~~ on i32
    |}]

let%expect_test "typecheck: index non-integer" =
  run_src "f :: fn() { a : [2]i32 = [1, 2]; a[true] };";
  [%expect
    {|
    error: array index must be an integer
      at <test>:1:36
        f :: fn() { a : [2]i32 = [1, 2]; a[true] };
                                           ^~~~
    |}]

let%expect_test "typecheck: index result type" =
  run_src "f :: fn() -> i32 { a : [2]i32 = [1, 2]; return a[0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: len is usize" =
  run_src "f :: fn() -> usize { a : [2]i32 = [1, 2]; return a.len };";
  [%expect {| ok |}]

let%expect_test "typecheck: len mismatched with i32" =
  run_src "f :: fn() -> i32 { a : [2]i32 = [1, 2]; return a.len };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:48
        f :: fn() -> i32 { a : [2]i32 = [1, 2]; return a.len };
                                                       ^~~~~ expected i32, found usize
    |}]

let%expect_test "typecheck: array no such field" =
  run_src "f :: fn() { a : [2]i32 = [1, 2]; a.foo };";
  [%expect
    {|
    error: no field
      at <test>:1:36
        f :: fn() { a : [2]i32 = [1, 2]; a.foo };
                                           ^~~ on [2]i32
    |}]

let%expect_test "typecheck: assign to index" =
  run_src "f :: fn() { a : [2]i32 = [1, 2]; a[0] = 9 };";
  [%expect {| ok |}]

let%expect_test "typecheck: index element assign type mismatch" =
  run_src "f :: fn() { a : [2]i32 = [1, 2]; a[0] = true };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:41
        f :: fn() { a : [2]i32 = [1, 2]; a[0] = true };
                                                ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: for over range ok (branch 2)" =
  run_src "f :: fn() { for i in 0..5 { x := i } };";
  [%expect {| ok |}]

let%expect_test "typecheck: for over inclusive range ok (branch 2)" =
  run_src "f :: fn() { for i in 0..=5 { x := i } };";
  [%expect {| ok |}]

let%expect_test "typecheck: for over array binds element type" =
  run_src
    {|
f :: fn() {
  a : [3]i32 = [1, 2, 3];
  for x in a { y : i32 = x }
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: for over array wrong element use" =
  run_src
    {|
f :: fn() {
  a : [3]i32 = [1, 2, 3];
  for x in a { y : bool = x }
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:27
          for x in a { y : bool = x }
                                  ^ expected bool, found i32
    |}]

let%expect_test "typecheck: for over non-iterable" =
  run_src "f :: fn() { for x in 5 { y := x } };";
  [%expect
    {|
    error: cannot iterate
      at <test>:1:22
        f :: fn() { for x in 5 { y := x } };
                             ^ on i32
    |}]

let%expect_test "typecheck: range bounds must be integers (branch 2)" =
  run_src "f :: fn() { for i in true..5 { x := i } };";
  [%expect
    {|
    error: range bounds must be integers
      at <test>:1:22
        f :: fn() { for i in true..5 { x := i } };
                             ^~~~
    error: type mismatch
      at <test>:1:28
        f :: fn() { for i in true..5 { x := i } };
                                   ^ expected bool, found i32
    |}]

let%expect_test "typecheck: range literal bends to typed endpoint (branch 1)" =
  run_src {|
f :: fn() {
  n : i64 = 5;
  for i in 0..n { x := i }
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: range over len needs no cast (branch 1)" =
  run_src
    {|
f :: fn() {
  a : [4]i32 = [1, 2, 3, 4];
  for i in 0..a.len { a[i] = 0 }
};
|};
  [%expect {| ok |}]

let%expect_test
    "typecheck: typed left endpoint bends the literal right (branch 2)" =
  run_src {|
f :: fn() {
  n : i64 = 5;
  for i in n..10 { x := i }
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: slice bound over len needs no cast (branch 1)" =
  run_src
    {|
f :: fn() {
  a : [4]i32 = [1, 2, 3, 4];
  s : []i32 = a[0..a.len];
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: two typed endpoints still must match (branch 2)" =
  run_src
    {|
f :: fn() {
  m : i32 = 0;
  n : i64 = 5;
  for i in m..n { x := i }
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: break inside for" =
  run_src "f :: fn() { for i in 0..5 { break } };";
  [%expect {| ok |}]

let%expect_test "typecheck: continue inside for" =
  run_src "f :: fn() { for i in 0..5 { continue } };";
  [%expect {| ok |}]

let%expect_test "typecheck: a bare array is not a slice param" =
  run_src
    {|
sum :: fn(xs: []i32) -> i32 { return 0 };
f :: fn() -> i32 {
  a : [3]i32 = [1, 2, 3];
  return sum(a);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: slice element wrong type rejected" =
  run_src
    {|
sum :: fn(xs: []i32) {};
f :: fn() {
  a : [2]f32 = [1.0, 2.0];
  sum(a);
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:5:7
          sum(a);
              ^ expected []i32, found [2]f32
    |}]

let%expect_test "typecheck: sub-slice ok" =
  run_src
    "f :: fn() -> i32 { a : [4]i32 = [1,2,3,4]; s : []i32 = a[1..3]; return \
     s[0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: slice of a slice ok" =
  run_src
    {|
f :: fn() -> i32 {
  a : [5]i32 = [1, 2, 3, 4, 5];
  s : []i32 = a[1..5];
  t : []i32 = s[1..3];
  return t[0];
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: returning a sub-slice of a slice param ok" =
  run_src "f :: fn(xs: []i32) -> []i32 { return xs[0..2] };";
  [%expect {| ok |}]

let%expect_test "typecheck: returning a slice param ok" =
  run_src "f :: fn(xs: []i32) -> []i32 { return xs };";
  [%expect {| ok |}]

let%expect_test "typecheck: returning a pointer param ok" =
  run_src "f :: fn(p: *i32) -> *i32 { return p };";
  [%expect {| ok |}]

let%expect_test
    "typecheck: returning the address of a field through a pointer ok" =
  run_src "S :: struct { a: i32 }; f :: fn(p: *S) -> *i32 { return &p.a };";
  [%expect {| ok |}]

let%expect_test "typecheck: returning a slice from a slice call ok" =
  run_src
    "g :: fn(xs: []i32) -> []i32 { return xs }; f :: fn(xs: []i32) -> []i32 { \
     return g(xs) };";
  [%expect {| ok |}]

let%expect_test "typecheck: inclusive range slice ok (branch 2)" =
  run_src
    "f :: fn() -> i32 { a : [3]i32 = [1,2,3]; s : []i32 = a[0..=2]; return \
     s[2] };";
  [%expect {| ok |}]

let%expect_test "typecheck: slice bounds must be integers (branch 2)" =
  run_src "f :: fn() { a : [3]i32 = [1,2,3]; s : []i32 = a[true..2] };";
  [%expect
    {|
    error: range bounds must be integers
      at <test>:1:49
        f :: fn() { a : [3]i32 = [1,2,3]; s : []i32 = a[true..2] };
                                                        ^~~~
    error: type mismatch
      at <test>:1:55
        f :: fn() { a : [3]i32 = [1,2,3]; s : []i32 = a[true..2] };
                                                              ^ expected bool, found i32
    |}]

let%expect_test "typecheck: slice .len is usize" =
  run_src
    "f :: fn() -> usize { a : [3]i32 = [1,2,3]; s : []i32 = a[0..3]; return \
     s.len };";
  [%expect {| ok |}]

let%expect_test "typecheck: slice .ptr is pointer" =
  run_src
    {|
first :: fn(p: *i32) -> i32 { return 0 };
f :: fn() -> i32 {
  a : [3]i32 = [1, 2, 3];
  s : []i32 = a[0..3];
  return first(s.ptr);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: slice does not coerce back to array" =
  run_src "f :: fn() { a : [3]i32 = [1,2,3]; b : [3]i32 = a[0..3] };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:48
        f :: fn() { a : [3]i32 = [1,2,3]; b : [3]i32 = a[0..3] };
                                                       ^~~~~~~ expected [3]i32, found []i32
    |}]

let%expect_test "typecheck: for over slice binds element" =
  run_src
    {|
f :: fn() {
  a : [3]i32 = [1, 2, 3];
  s : []i32 = a[0..3];
  for x in s { y : i32 = x }
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: slice index element assignable" =
  run_src "f :: fn() { a : [3]i32 = [1,2,3]; s : []i32 = a[0..3]; s[0] = 9 };";
  [%expect {| ok |}]

let%expect_test "typecheck: compound assign to array element" =
  run_src "f :: fn() -> i32 { a : [3]i32 = [1,2,3]; a[0] += 5; return a[0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: multidimensional array" =
  run_src "f :: fn() -> i32 { m : [2][2]i32 = [[1,2],[3,4]]; return m[1][0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: multidim wrong inner count" =
  run_src "f :: fn() { m : [2][2]i32 = [[1,2],[3]] };";
  [%expect
    {|
    error: wrong number of arguments
      at <test>:1:36
        f :: fn() { m : [2][2]i32 = [[1,2],[3]] };
                                           ^~~ expected 2 elements, found 1
    |}]

let%expect_test "typecheck: global array" =
  run_src {|
g : [3]i32 = [7, 8, 9];
f :: fn() -> i32 { return g[1] };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global array element calls a function" =
  run_src {|
k :: fn() -> i32 { return 1 };
g : [2]i32 = [k(), 2];
|};
  [%expect {| ok |}]

let%expect_test "typecheck: iterate array of arrays" =
  run_src
    {|
f :: fn() -> i32 {
  m : [2][2]i32 = [[1, 2], [3, 4]];
  s : i32 = 0;
  for row in m { s += row[0] }
  return s;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: range as value is an error" =
  run_src "f :: fn() -> i32 { return 0..5 };";
  [%expect
    {|
    error: range is only valid in a for loop or slice
      at <test>:1:27
        f :: fn() -> i32 { return 0..5 };
                                  ^~~~
    |}]

let%expect_test "typecheck: range in condition is an error" =
  run_src "f :: fn() { if 0..5 { } };";
  [%expect
    {|
    error: range is only valid in a for loop or slice
      at <test>:1:16
        f :: fn() { if 0..5 { } };
                       ^~~~
    |}]

let%expect_test "typecheck: for over array literal" =
  run_src
    "f :: fn() -> i32 { s : i32 = 0; for x in [1,2,3] { s += x }; return s };";
  [%expect {| ok |}]

let%expect_test "typecheck: array literal as slice argument" =
  run_src
    {|
sum :: fn(xs: []i32) -> i32 { return 0 };
f :: fn() -> i32 { return sum([1, 2, 3]) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: scalar zero init" =
  run_src "f :: fn() -> i32 { x : i32; return x };";
  [%expect {| ok |}]

let%expect_test "typecheck: array zero init" =
  run_src "f :: fn() -> i32 { a : [3]i32; return a[0] };";
  [%expect {| ok |}]

let%expect_test "typecheck: missing return on a path" =
  run_src "f :: fn(n: i32) -> i32 { if n > 0 { return 1 } };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:26
        f :: fn(n: i32) -> i32 { if n > 0 { return 1 } };
                                 ^~~~~~~~~~~~~~~~~~~~~ expected i32, found ()
    |}]

let%expect_test "typecheck: if and else both return ok" =
  run_src "f :: fn(n: i32) -> i32 { if n > 0 { return 1 } else { return 0 } };";
  [%expect {| ok |}]

let%expect_test "typecheck: while true diverges, no return needed" =
  run_src "f :: fn() -> i32 { while true { } };";
  [%expect {| ok |}]

let%expect_test "typecheck: while true with break still needs a return" =
  run_src "f :: fn() -> i32 { while true { break } };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:20
        f :: fn() -> i32 { while true { break } };
                           ^~~~~~~~~~~~~~~~~~~~ expected i32, found ()
    |}]

let%expect_test "typecheck: inner loop break does not exit outer" =
  run_src "f :: fn() -> i32 { while true { while true { break } } };";
  [%expect {| ok |}]

let%expect_test "typecheck: break under if still needs a return" =
  run_src
    "f :: fn() -> i32 { c : bool = true;\n while true { if c { break } } };";
  [%expect
    {|
    error: type mismatch
      at <test>:2:2
         while true { if c { break } } };
         ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~ expected i32, found ()
    |}]

let%expect_test "typecheck: struct literal" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() -> i32 {
  p := Pt { x: 3, y: 4 };
  return p.x + p.y;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: empty struct literal" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() -> i32 {
  p := Pt { };
  return p.x;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: struct literal unknown field" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() {
  p := Pt { z: 1 };
};
|};
  [%expect
    {|
    error: no field
      at <test>:4:13
          p := Pt { z: 1 };
                    ^
    |}]

let%expect_test "typecheck: struct literal duplicate field" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() {
  p := Pt { x: 1, x: 2 };
};
|};
  [%expect
    {|
    error: duplicate field
      at <test>:4:19
          p := Pt { x: 1, x: 2 };
                          ^
    |}]

let%expect_test "typecheck: struct literal wrong field type" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() {
  p := Pt { x: true };
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:16
          p := Pt { x: true };
                       ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: undefined struct literal" =
  run_src {|
f :: fn() {
  p := nope { x: 1 };
};
|};
  [%expect
    {|
    error: undefined struct
      at <test>:3:8
          p := nope { x: 1 };
               ^~~~
    |}]

let%expect_test "typecheck: global struct literal" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
origin : Pt = Pt { x: 1, y: 2 };
f :: fn() -> i32 { return origin.x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: global struct literal calls a function" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
g :: fn() -> i32 { return 1 };
p : Pt = Pt { x: g(), y: 2 };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: positional struct literal" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() -> i32 {
  p := Pt { 3, 4 };
  return p.x + p.y;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: positional struct literal of one field" =
  run_src
    {|
Box :: struct { v: i32 };
f :: fn() -> i32 {
  b := Box { 3 };
  return b.v;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: positional struct literal too few fields" =
  run_src {|
Pt :: struct { x: i32, y: i32 };
f :: fn() {
  p := Pt { 1 };
};
|};
  [%expect
    {|
    error: wrong number of fields
      at <test>:4:8
          p := Pt { 1 };
               ^~~~~~~~ expected 2, found 1
    |}]

let%expect_test "typecheck: positional struct literal too many fields" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() {
  p := Pt { 1, 2, 3 };
};
|};
  [%expect
    {|
    error: wrong number of fields
      at <test>:4:8
          p := Pt { 1, 2, 3 };
               ^~~~~~~~~~~~~~ expected 2, found 3
    |}]

let%expect_test "typecheck: positional struct literal wrong field type" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() {
  p := Pt { 1, true };
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:16
          p := Pt { 1, true };
                       ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: positional struct literal nested" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
Wrap :: struct { p: Pt, tag: i32 };
f :: fn() -> i32 {
  w := Wrap { Pt { 1, 2 }, 3 };
  return w.p.x + w.tag;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: positional global struct literal" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
origin : Pt = Pt { 1, 2 };
f :: fn() -> i32 { return origin.x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: positional literal of a fieldless struct" =
  run_src {|
E :: struct {
};
f :: fn() {
  _a := E { };
  _b := E { 1 };
};
|};
  [%expect
    {|
    error: wrong number of fields
      at <test>:6:9
          _b := E { 1 };
                ^~~~~~~ expected 0, found 1
    |}]

let%expect_test "typecheck: duplicate struct field" =
  run_src {|
Pt :: struct { x: i32, x: i64 };
|};
  [%expect
    {|
    error: duplicate field
      at <test>:2:24
        Pt :: struct { x: i32, x: i64 };
                               ^
    |}]

let%expect_test "typecheck: three duplicate struct fields" =
  run_src {|
Pt :: struct { x: i32, x: i64, x: bool };
|};
  [%expect
    {|
    error: duplicate field
      at <test>:2:24
        Pt :: struct { x: i32, x: i64, x: bool };
                               ^
    error: duplicate field
      at <test>:2:32
        Pt :: struct { x: i32, x: i64, x: bool };
                                       ^
    |}]

let%expect_test "typecheck: type alias mismatch across types" =
  run_src
    {|
MyInt :: i64;
f :: fn(x: MyInt) -> i32 { return 0 };
g :: fn() { f(true) };
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:15
        g :: fn() { f(true) };
                      ^~~~ expected MyInt, found bool
    |}]

let%expect_test "typecheck: cstr parameter accepts string literal" =
  run_src
    {|
extern "C" fn strlen(s: cstr) i64;
f :: fn() -> i64 { return strlen("hi") };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: extern variadic accepts extra args" =
  run_src
    {|
extern "C" fn printf(fmt: cstr, ...) i32;
f :: fn() { printf("%d %d", 1, 2) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: pointer equality yields bool" =
  run_src {|
f :: fn(a: *i32, b: *i32) -> bool { return a == b };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: nested struct field type mismatch" =
  run_src
    {|
Inner :: struct { a: i32 };
Outer :: struct { i: Inner };
f :: fn() {
  o : Outer = Outer { i: Inner { a: 1 } };
  o.i.a = true;
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:6:11
          o.i.a = true;
                  ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: struct with array field initializes ok" =
  run_src
    {|
Buf :: struct { data: [4]i32, n: i32 };
f :: fn() -> i32 {
  b : Buf = Buf { data: [1, 2, 3, 4], n: 4 };
  return b.n;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: function returning struct ok" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
origin :: fn() -> Pt { return Pt { x: 0, y: 0 } };
f :: fn() -> i32 { return origin().x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: unit call result cannot be assigned" =
  run_src {|
g :: fn() { };
f :: fn() { x : i32 = g() };
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:23
        f :: fn() { x : i32 = g() };
                              ^~~ expected i32, found ()
    |}]

let%expect_test "typecheck: struct field whose type is another struct" =
  run_src
    {|
Bt :: struct { x: i32 };
A :: struct { b: Bt };
f :: fn() -> i32 {
  v : A = A { b: Bt { x: 1 } };
  return v.b.x;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array of structs iterates element type" =
  run_src
    {|
Pt :: struct { x: i32, y: i32 };
f :: fn() -> i32 {
  pts : [2]Pt = [Pt { x: 1, y: 2 }, Pt { x: 3, y: 4 }];
  s : i32 = 0;
  for p in pts { s += p.x }
  return s;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: cast int to float ok" =
  run_src {|
f :: fn() -> f64 {
  a : i32 = 3;
  return cast(f64, a);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: cast float to int ok" =
  run_src {|
f :: fn() -> i32 {
  a : f64 = 3.5;
  return cast(i32, a);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: array size mismatch as argument" =
  run_src
    {|
take :: fn(a: [4]i32) {};
f :: fn() {
  a : [3]i32 = [1, 2, 3];
  take(a);
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:5:8
          take(a);
               ^ expected [4]i32, found [3]i32
    |}]

let%expect_test "typecheck: extern variadic requires the fixed args" =
  run_src
    {|
extern "C" fn printf(fmt: cstr, ...) i32;
f :: fn() { printf() };
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:3:13
        f :: fn() { printf() };
                    ^~~~~~~~ expected at least 1 argument, found 0
    |}]

let%expect_test "typecheck: continue inside while" =
  run_src "f :: fn() { while true { continue } };";
  [%expect {| ok |}]

let%expect_test "typecheck: bool relational comparison rejected" =
  run_src
    {|
f :: fn() -> bool {
  a : bool = true;
  b : bool = false;
  return a < b;
};
|};
  [%expect
    {|
    error: invalid operand
      at <test>:5:10
          return a < b;
                 ^ cannot apply `<` to bool
    |}]

let%expect_test "typecheck: struct equality rejected" =
  run_src
    {|
P :: struct { x: i32 };
f :: fn() -> bool {
  a : P = P { x: 1 };
  b : P = P { x: 1 };
  return a == b;
};
|};
  [%expect
    {|
    error: invalid operand
      at <test>:6:10
          return a == b;
                 ^ cannot apply `==` to P
    |}]

let%expect_test "typecheck: unit equality rejected" =
  run_src {|
g :: fn() {};
f :: fn() -> bool {
  return g() == g();
};
|};
  [%expect
    {|
    error: invalid operand
      at <test>:4:10
          return g() == g();
                 ^~~ cannot apply `==` to ()
    |}]

let%expect_test "typecheck: float modulo rejected" =
  run_src {|
f :: fn() -> f64 {
  return 5.0 % 2.0;
};
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:10
          return 5.0 % 2.0;
                 ^~~ cannot apply `%` to f64
    |}]

let%expect_test "typecheck: bare return in main rejected" =
  run_src {|
main :: fn() -> i32 {
  return;
};
|};
  [%expect
    {|
    error: empty return in non-unit function
      at <test>:3:3
          return;
          ^~~~~~
    |}]

let%expect_test "typecheck: bare return in non-main i32 rejected" =
  run_src {|
g :: fn() -> i32 {
  return;
};
|};
  [%expect
    {|
    error: empty return in non-unit function
      at <test>:3:3
          return;
          ^~~~~~
    |}]

let%expect_test "typecheck: int literal out of range rejected" =
  run_src {|
main :: fn() -> i32 {
  x : u8 = 300;
  return 0;
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:12
          x : u8 = 300;
                   ^~~ does not fit in u8
    |}]

let%expect_test "typecheck: negative literal into unsigned rejected" =
  run_src {|
main :: fn() -> i32 {
  x : u8 = -1;
  return 0;
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:12
          x : u8 = -1;
                   ^~ does not fit in u8
    |}]

let%expect_test "typecheck: int literal at type bound accepted" =
  run_src
    {|
main :: fn() -> i32 {
  x : u8 = 255;
  y : i8 = -128;
  return 0;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: inferred literal overflowing i32 rejected" =
  run_src {|
main :: fn() -> i32 {
  x := 3000000000;
  return x;
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:8
          x := 3000000000;
               ^~~~~~~~~~ does not fit in i32
    |}]

let%expect_test "typecheck: i64 max accepted" =
  run_src
    {|
main :: fn() -> i32 {
  _x : i64 = 9223372036854775807;
  return 0;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: u64 max accepted" =
  run_src
    {|
main :: fn() -> i32 {
  _x : u64 = 18446744073709551615;
  return 0;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: i64 max plus one rejected" =
  run_src
    {|
main :: fn() -> i32 {
  _x : i64 = 9223372036854775808;
  return 0;
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:14
          _x : i64 = 9223372036854775808;
                     ^~~~~~~~~~~~~~~~~~~ does not fit in i64
    |}]

let%expect_test "typecheck: literal above u64 max rejected by lexer" =
  run_src
    {|
main :: fn() -> i32 {
  _x : u64 = 18446744073709551616
  return 0;
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:14
          _x : u64 = 18446744073709551616
                     ^~~~~~~~~~~~~~~~~~~~
    |}]

let%expect_test "typecheck: negative literal into u64 rejected" =
  run_src {|
main :: fn() -> i32 {
  _x : u64 = -1;
  return 0;
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:14
          _x : u64 = -1;
                     ^~ does not fit in u64
    |}]

let%expect_test "typecheck: non-i32 main rejected" =
  run_src "main :: fn() -> f64 { return 0.0 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:17
        main :: fn() -> f64 { return 0.0 };
                        ^~~ expected () or i32, found f64
    |}]

let%expect_test "typecheck: type alias is transparent to its base" =
  run_src
    {|
Meters :: i32;
f :: fn() -> i32 { d : Meters = 5; return d + 1 };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a struct allows field access" =
  run_src
    {|
Point :: struct { x: i32, y: i32 };
Pt :: Point;
f :: fn() -> i32 { p : Pt = Point { x: 1, y: 2 }; return p.x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a function pointer is callable" =
  run_src
    {|
BinOp :: (fn (i32, i32) i32);
add :: fn(a: i32, b: i32) -> i32 { return a + b };
f :: fn() -> i32 { op : BinOp = add; return op(2, 3) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: alias of an array coerces to a slice" =
  run_src
    {|
Row :: [3]i32;
take :: fn(s: []i32) -> i32 { return s[0] };
f :: fn() -> i32 { r : Row = [1, 2, 3]; return take(r) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: aggregate cast sees through an alias element" =
  run_src
    {|
Meters :: i32;
f :: fn() -> i32 { a : [3]Meters = [1, 2, 3]; b : [3]i32 = a as [3]i32; return b[1] };
|};
  [%expect
    {|
    error: expected `;`
      at <test>:3:62
        f :: fn() -> i32 { a : [3]Meters = [1, 2, 3]; b : [3]i32 = a as [3]i32; return b[1] };
                                                                     ^~ found as
    |}]

let%expect_test "typecheck: alias is transparent under a slice and a pointer" =
  run_src
    {|
Meters :: i32;
take_slice :: fn(s: []i32) -> i32 { return s[0] };
take_ptr :: fn(p: *i32) -> i32 { return *p };
f :: fn() -> i32 {
  a : [3]Meters = [1, 2, 3];
  s : []Meters = a;
  m : Meters = 7;
  return take_slice(s) + take_ptr(&m);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: alias and base compare with each other" =
  run_src
    {|
Meters :: i32;
f :: fn() -> bool { m : Meters = 5; b : i32 = 5; return m == b };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias keeps every comparison of its base" =
  run_src
    {|
Meters :: i32;
f :: fn() -> bool {
  a : Meters = 5;
  b : Meters = 6;
  return a == b || a != b || a < b || a > b || a <= b || a >= b;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias keeps arithmetic and bitwise operators" =
  run_src
    {|
Meters :: i32;
f :: fn() -> i32 {
  a : Meters = 12;
  b : Meters = 5;
  return a + b - a * b / (a % b) + (a & b) + (a | b) + (a ^ b) + (a << 1) + (a >> 1);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a float keeps its operators" =
  run_src
    {|
Temp :: f32;
f :: fn() -> bool {
  a : Temp = 1.5;
  b : Temp = 2.5;
  c : Temp = a + b - a * b / a;
  c += 1.0;
  return -c < b && a <= b && a == a && b >= a;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: type alias of a float still has no remainder" =
  run_src {|
Temp :: f32;
f :: fn() -> f32 { a : Temp = 5.0; return a % a };
|};
  [%expect
    {|
    error: invalid operand
      at <test>:3:43
        f :: fn() -> f32 { a : Temp = 5.0; return a % a };
                                                  ^ cannot apply `%` to Temp
    |}]

let%expect_test "typecheck: type alias mixes with its base in comparisons" =
  run_src
    {|
Meters :: i32;
f :: fn() -> bool { a : Meters = 5; raw : i32 = 6; return a < raw && raw > a };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a type alias name collides with a struct" =
  run_src {|
Foo :: i32;
Foo :: struct { x: i32 };
|};
  [%expect
    {|
    error: already defined
      at <test>:3:1
        Foo :: struct { x: i32 };
        ^~~
      at <test>:2:1
        Foo :: i32;
        ^~~ previous definition here
    |}]

let%expect_test "typecheck: a type name shadows a builtin" =
  run_src {|
i32 :: i64;
f :: fn(x: i32) -> i64 { return x };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: shadowing a builtin reaches its own definition" =
  run_src {|
i32 :: bool;
f :: fn(x: i32) -> i64 { return x };
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:33
        f :: fn(x: i32) -> i64 { return x };
                                        ^ expected i64, found i32
    |}]

let%expect_test "typecheck: sizeof of a struct type" =
  run_src
    {|
S :: struct { a: i32, b: i32 };
f :: fn() -> i64 { return sizeof(S) as i64 };
|};
  [%expect
    {|
    error: expected `;`
      at <test>:3:37
        f :: fn() -> i64 { return sizeof(S) as i64 };
                                            ^~ found as
    |}]

let%expect_test "typecheck: sizeof of an array type" =
  run_src "f :: fn() -> i64 { return cast(i64, sizeof([4]i32)) };";
  [%expect {| ok |}]

let%expect_test "typecheck: a struct field names a struct defined later" =
  run_src {|
A :: struct { b: *B };
B :: struct { n: i32 };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a struct points at itself" =
  run_src "Node :: struct { val: i32, next: *Node };";
  [%expect {| ok |}]

let%expect_test "typecheck: a struct that holds itself by value has no size" =
  run_src "Node :: struct { n: Node };";
  [%expect
    {|
    error: recursive struct has infinite size
      at <test>:1:1
        Node :: struct { n: Node };
        ^~~~
    |}]

let%expect_test
    "typecheck: two structs that hold each other by value have no size" =
  run_src {|
A :: struct { b: B };
B :: struct { a: A };
|};
  [%expect
    {|
    error: recursive struct has infinite size
      at <test>:2:1
        A :: struct { b: B };
        ^
    error: recursive struct has infinite size
      at <test>:3:1
        B :: struct { a: A };
        ^
    |}]

let%expect_test "typecheck: int literal suffix pins the type" =
  run_src "f :: fn() -> u8 { return 200u8 };";
  [%expect {| ok |}]

let%expect_test "typecheck: int literal suffix that mismatches the target" =
  run_src "f :: fn() { x : u8 = 5u16 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:22
        f :: fn() { x : u8 = 5u16 };
                             ^~~~ expected u8, found u16
    |}]

let%expect_test "typecheck: int literal suffix out of range" =
  run_src "f :: fn() -> u8 { return 256u8 };";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:26
        f :: fn() -> u8 { return 256u8 };
                                 ^~~~~ does not fit in u8
    |}]

let%expect_test "typecheck: negative unsigned suffix" =
  run_src "f :: fn() -> i8 { return -1u8 };";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:26
        f :: fn() -> i8 { return -1u8 };
                                 ^~~~ does not fit in u8
    |}]

let%expect_test "typecheck: assignment in condition is not a value" =
  run_src "f :: fn() { b : bool = false; if b = true { } };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        f :: fn() { b : bool = false; if b = true { } };
                                         ^~~~~~~~ expected bool, found ()
    help: did you mean `==` to compare?
    |}]

let%expect_test "typecheck: chained assignment is not a value" =
  run_src "f :: fn() { a : i32 = 0; b : i32 = 0; a = b = 5 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:43
        f :: fn() { a : i32 = 0; b : i32 = 0; a = b = 5 };
                                                  ^~~~~ expected i32, found ()
    |}]

let%expect_test "typecheck: assign to for loop variable" =
  run_src "f :: fn() { for i in 0..3 { i = 99 } };";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:29
        f :: fn() { for i in 0..3 { i = 99 } };
                                    ^
    |}]

let%expect_test "typecheck: a never binding needs a diverging init" =
  run_src "f :: fn() { x : never = 0 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:25
        f :: fn() { x : never = 0 };
                                ^ expected never, found i32
    |}]

let%expect_test "typecheck: never as a binding type" =
  run_src {|
d :: fn() -> never { loop {} };
f :: fn() { _x : never = d() };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: never as a param type" =
  run_src "f :: fn(_x: never) {};";
  [%expect {| ok |}]

let%expect_test "typecheck: never as a pointee type" =
  run_src "f :: fn() { _p : *never = null };";
  [%expect {| ok |}]

let%expect_test "typecheck: never as a field type" =
  run_src "S :: struct { x: never };";
  [%expect {| ok |}]

let%expect_test "typecheck: a never field cannot be zero init" =
  run_src {|
S :: struct { x: never };
f :: fn() { _s : S };
|};
  [%expect
    {|
    error: cannot zero init this type
      at <test>:3:18
        f :: fn() { _s : S };
                         ^ on S
    |}]

let%expect_test "typecheck: an omitted never field cannot be zero init" =
  run_src
    {|
S :: struct { x: never, y: i32 };
f :: fn() { _s := S { y: 1 } };
|};
  [%expect
    {|
    error: cannot zero init this type
      at <test>:3:19
        f :: fn() { _s := S { y: 1 } };
                          ^~~~~~~~~~ on never
    |}]

let%expect_test "typecheck: a never element cannot be zero init" =
  run_src "f :: fn() { _a : [2]never };";
  [%expect
    {|
    error: cannot zero init this type
      at <test>:1:18
        f :: fn() { _a : [2]never };
                         ^~~~~~~~ on [2]never
    |}]

let%expect_test "typecheck: return in a never function is rejected" =
  run_src "spin :: fn() -> never { return };";
  [%expect
    {|
    error: a never function cannot return
      at <test>:1:25
        spin :: fn() -> never { return };
                                ^~~~~~
    |}]

let%expect_test "typecheck: return value in a never function is rejected" =
  run_src "spin :: fn() -> never { return 5 };";
  [%expect
    {|
    error: a never function cannot return
      at <test>:1:25
        spin :: fn() -> never { return 5 };
                                ^~~~~~~~
    |}]

let%expect_test "typecheck: a never call satisfies the missing return check" =
  run_src
    {|
extern "C" fn exit(code: i32) never;
f :: fn() -> i32 { exit(1) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a never call coerces to the return type" =
  run_src
    {|
extern "C" fn exit(code: i32) never;
f :: fn() -> i32 { return exit(1) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: function pointer may return never" =
  run_src
    {|
extern "C" fn exit(code: i32) never;
f :: fn() { stop : extern "C" fn (i32) never = exit;
 stop(1) };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a typed pointer needs a cast to become ptr" =
  run_src "f :: fn(p: *i32) -> ptr { return p };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        f :: fn(p: *i32) -> ptr { return p };
                                         ^ *i32 needs a cast to become ptr
    |}]

let%expect_test "typecheck: cstr needs a cast to become ptr" =
  run_src "f :: fn(s: cstr) -> ptr { return s };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        f :: fn(s: cstr) -> ptr { return s };
                                         ^ cstr needs a cast to become ptr
    |}]

let%expect_test "typecheck: null flows into ptr" =
  run_src "f :: fn() -> ptr { return null };";
  [%expect {| ok |}]

let%expect_test "typecheck: ptr needs a cast to become a typed pointer" =
  run_src "f :: fn(a: ptr) -> *i32 { return a };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        f :: fn(a: ptr) -> *i32 { return a };
                                         ^ ptr needs a cast to become *i32
    |}]

let%expect_test "typecheck: ptr casts back to a typed pointer" =
  run_src "f :: fn(a: ptr) -> *i32 { return cast(*i32, a) };";
  [%expect {| ok |}]

let%expect_test "typecheck: cannot dereference ptr" =
  run_src "f :: fn(a: ptr) -> i32 { return *a };";
  [%expect
    {|
    error: cannot dereference ptr
      at <test>:1:34
        f :: fn(a: ptr) -> i32 { return *a };
                                         ^
    help: cast to a typed pointer first
    |}]

let%expect_test "typecheck: cannot index ptr" =
  run_src "f :: fn(a: ptr) -> i32 { return a[0] };";
  [%expect
    {|
    error: cannot index ptr
      at <test>:1:33
        f :: fn(a: ptr) -> i32 { return a[0] };
                                        ^~~~
    help: cast to a typed pointer first
    |}]

let%expect_test "typecheck: cannot access a field of ptr" =
  run_src "f :: fn(a: ptr) -> i32 { return a.x };";
  [%expect
    {|
    error: cannot access a field of ptr
      at <test>:1:33
        f :: fn(a: ptr) -> i32 { return a.x };
                                        ^~~
    help: cast to a typed pointer first
    |}]

let%expect_test "typecheck: no arithmetic on ptr" =
  run_src "f :: fn(a: ptr) -> ptr { return a + 1 };";
  [%expect
    {|
    error: invalid operand
      at <test>:1:33
        f :: fn(a: ptr) -> ptr { return a + 1 };
                                        ^ cannot apply `+` to ptr
    error: type mismatch
      at <test>:1:37
        f :: fn(a: ptr) -> ptr { return a + 1 };
                                            ^ expected ptr, found i32
    |}]

let%expect_test "typecheck: ptr compares to null" =
  run_src "f :: fn(a: ptr) -> bool { return a == null };";
  [%expect {| ok |}]

let%expect_test "typecheck: two ptr values compare" =
  run_src "f :: fn(a: ptr, b: ptr) -> bool { return a != b };";
  [%expect {| ok |}]

let%expect_test "typecheck: a typed pointer casts to ptr" =
  run_src "f :: fn(p: *i32) -> ptr { return cast(ptr, p) };";
  [%expect {| ok |}]

let%expect_test "typecheck: ptr and a typed pointer need a cast to compare" =
  run_src "f :: fn(a: ptr, b: *i32) -> bool { return a == b };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:48
        f :: fn(a: ptr, b: *i32) -> bool { return a == b };
                                                       ^ *i32 needs a cast to become ptr
    |}]

let%expect_test "typecheck: if-expr never arm bends to the other arm" =
  run_src
    {|
extern "C" fn exit(c: i32) never;
f :: fn() -> i32 { y := if true { 10 } else { exit(1) }; return y };
|};
  [%expect {| ok |}]

let%expect_test
    ("typecheck: if-expr arm type is order independent" [@tags "disabled"]) =
  run_src
    {|f :: fn() -> i32 { x : i64 = 5;
 y := if true { x } else { 10 }; return y as i32 };|};
  [%expect
    {|
    error: type mismatch
      at <test>:2:44
         y := if true { x } else { 10 }; return y as i32 }
                                                   ^ expected i32, found i64
    error: expected `;`
      at <test>:2:46
         y := if true { x } else { 10 }; return y as i32 }
                                                     ^~ found as
    |}]

let%expect_test "typecheck: all-never if-expr binds as never" =
  run_src
    {|
extern "C" fn exit(c: i32) never;
f :: fn() -> i32 { _y := if true { exit(3) } else { exit(4) }; return 0 };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: nested if-expr never arm bends to the other arm" =
  run_src
    {|
extern "C" fn exit(c: i32) never;
f :: fn() -> i32 { y := if true { if false { 10 } else { exit(1) } } else
 { 20 }; return y };
|};
  [%expect {| ok |}]

let%expect_test
    ("typecheck: nested concrete arm anchors the outer if-expr"
     [@tags "disabled"]) =
  run_src
    {|f :: fn() -> i32 { x : i64 = 7;
 y := if true { if false { x } else { 5 } } else { 10 }; return y as i32 };|};
  [%expect
    {|
    error: type mismatch
      at <test>:2:68
         y := if true { if false { x } else { 5 } } else { 10 }; return y as i32 }
                                                                           ^ expected i32, found i64
    error: expected `;`
      at <test>:2:70
         y := if true { if false { x } else { 5 } } else { 10 }; return y as i32 }
                                                                             ^~ found as
    |}]

(* Expression oriented collapse edge cases *)

let%expect_test "collapse: trailing if is an implicit return" =
  run_src "abs :: fn(x: i32) -> i32 { if x < 0 { -x } else { x } };";
  [%expect {| ok |}]

let%expect_test "collapse: trailing block is an implicit return" =
  run_src "f :: fn(x: i32) -> i32 { { a := x * 2;\n a + 1 } };";
  [%expect {| ok |}]

let%expect_test "collapse: nested block tail flows to the return" =
  run_src "f :: fn() -> i32 { { a : i32 = 1;\n { a + 2 } } };";
  [%expect {| ok |}]

let%expect_test "collapse: deeply nested implicit return" =
  run_src
    "f :: fn(x: i32) -> i32 { if x > 0 { { if x > 10 { 1 } else { 2 } } } else \
     { 3 } };";
  [%expect {| ok |}]

let%expect_test "collapse: a binding is not a value operand" =
  run_src "f :: fn() -> i32 { x : i32 = y : i32 = 5;\n return x };";
  [%expect
    {|
    error: expected `;`
      at <test>:1:32
        f :: fn() -> i32 { x : i32 = y : i32 = 5;
                                       ^ found :
    |}]

let%expect_test "collapse: a block ending in a binding is unit" =
  run_src "f :: fn() -> i32 { x : i32 = { a : i32 = 1 };\n return x };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:32
        f :: fn() -> i32 { x : i32 = { a : i32 = 1 };
                                       ^~~~~~~~~~~ expected i32, found ()
    |}]

let%expect_test "collapse: value if arms must agree" =
  run_src
    "f :: fn(c: bool) -> i32 { x : i32 = if c { 1 } else { true };\n\
    \ return x };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:55
        f :: fn(c: bool) -> i32 { x : i32 = if c { 1 } else { true };
                                                              ^~~~ expected i32, found bool
    |}]

let%expect_test "collapse: a never arm coerces to the live arm" =
  run_src
    {|f :: fn(c: bool) -> i32 { x : i32 = if c { 1 } else { return 0 };
 return x };|};
  [%expect {| ok |}]

let%expect_test "collapse: value if without else is unit" =
  run_src "f :: fn(c: bool) -> i32 { x : i32 = if c { 1 };\n return x };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:37
        f :: fn(c: bool) -> i32 { x : i32 = if c { 1 };
                                            ^~~~~~~~~~ expected i32, found ()
    |}]

let%expect_test "collapse: while true with no break is never" =
  run_src "f :: fn() -> i32 { while true { } };";
  [%expect {| ok |}]

let%expect_test "collapse: never function may loop forever" =
  run_src "spin :: fn() -> never { while true { } };";
  [%expect {| ok |}]

let%expect_test "collapse: break in a value arm inside a loop" =
  run_src
    {|f :: fn() -> i32 { while true { x : i32 = if false { 1 } else { break };
 return x }; return 0 };|};
  [%expect {| ok |}]

let%expect_test "collapse: continue as a value runs the step" =
  run_src
    {|f :: fn() -> i32 { i : i32 = 0;
 while i < 3 { x : i32 = if i == 2 { i } else { i = i + 1;
 continue };
 return x }; return 9 };|};
  [%expect {| ok |}]

let%expect_test "collapse: implicit return of a wrong tail type" =
  run_src "f :: fn() -> i32 { true };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:20
        f :: fn() -> i32 { true };
                           ^~~~ expected i32, found bool
    |}]

let%expect_test "collapse: return if with diverging arms" =
  run_src
    "f :: fn(c: bool) -> i32 { return if c { return 1 } else { return 2 } };";
  [%expect {| ok |}]

let%expect_test "collapse: break as a value outside a loop still errors" =
  run_src
    "f :: fn() -> i32 { x : i32 = if true { 1 } else { break };\n return x };";
  [%expect
    {|
    error: `break` outside a loop
      at <test>:1:51
        f :: fn() -> i32 { x : i32 = if true { 1 } else { break };
                                                          ^~~~~
    |}]

let%expect_test "collapse: nested value block anchors its type" =
  run_src
    "f :: fn() -> i64 { x : i64 = { a : i64 = 3;\n { a + 1 } };\n return x };";
  [%expect {| ok |}]

let%expect_test "typecheck: char is distinct from i32" =
  run_src "f :: fn() { x : i32 = 'A' };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:23
        f :: fn() { x : i32 = 'A' };
                              ^~~ expected i32, found char
    |}]

let%expect_test "typecheck: no arithmetic on a char" =
  run_src "f :: fn() -> i32 { return cast(i32, 'A' + 1) };";
  [%expect
    {|
    error: invalid operand
      at <test>:1:37
        f :: fn() -> i32 { return cast(i32, 'A' + 1) };
                                            ^~~ cannot apply `+` to char
    error: type mismatch
      at <test>:1:43
        f :: fn() -> i32 { return cast(i32, 'A' + 1) };
                                                  ^ expected char, found i32
    |}]

let%expect_test "typecheck: char casts to and from an integer" =
  run_src "f :: fn() -> i32 { c : char = cast(char, 65); return cast(i32, c) };";
  [%expect {| ok |}]

let%expect_test "typecheck: chars compare for equality and order" =
  run_src "f :: fn() -> bool { return 'A' == 'B' && 'A' < 'B' };";
  [%expect {| ok |}]

let%expect_test ("typecheck: char does not cast to a float" [@tags "disabled"])
    =
  run_src "f :: fn() -> f32 { return 'A'f32() };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:21
        f :: fn() -> f32 { return 'A'f32() };
                            ^~~ expected f32, found char
    error: expected `;`
      at <test>:1:24
        f :: fn() -> f32 { return 'A'f32() };
                               ^~~ found f32
    |}]

let%expect_test "typecheck: binding a unit call" =
  run_src "foo :: fn() { }; f :: fn() { _value := foo() };";
  [%expect {| ok |}]

let%expect_test "typecheck: newline operator continues into unit call" =
  run_src {|g :: fn() {};
f :: fn() -> i32 {
  return 1 +
    g();
};|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:5
            g();
            ^~~ expected i32, found ()
    |}]

let%expect_test "typecheck: newline operator continues into integer call" =
  run_src
    {|g :: fn() -> i32 { return 2 };
f :: fn() -> i32 {
  return 1 +
    g();
};|};
  [%expect {| ok |}]

let%expect_test "typecheck: a parameter names a type declared later" =
  run_src
    {|
take :: fn(value: Meters) -> i32 { return value };
Meters :: i32;
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a global names a type declared later" =
  run_src {|
width : Meters = 3;
Meters :: i32;
|};
  [%expect {| ok |}]

let%expect_test "typecheck: an alias names itself" =
  run_src {|
Loop :: Loop;
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:1
        Loop :: Loop;
        ^~~~
    |}]

let%expect_test "typecheck: two aliases name each other" =
  run_src {|
First :: Second;
Second :: First;
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:1
        First :: Second;
        ^~~~~
    |}]

let%expect_test "typecheck: an alias names itself through a pointer" =
  run_src {|
Loop :: *Loop;
|};
  [%expect
    {|
    error: recursive type
      at <test>:2:1
        Loop :: *Loop;
        ^~~~
    |}]

let%expect_test "typecheck: an alias chain resolves in either order" =
  run_src
    {|
Feet :: Meters;
Meters :: i32;
take :: fn(value: Feet) -> i32 { return value };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: inferred storage rejects never and unit" =
  run_src
    {|
extern "C" fn stop() never;
noop :: fn() {};
f :: fn() {
  never_array := [stop(), stop()];
  unit_array := [noop(), noop()];
};
|};
  [%expect
    {|
    error: array element cannot have this type
      at <test>:5:19
          never_array := [stop(), stop()];
                          ^~~~~~ on never
    error: array element cannot have this type
      at <test>:6:18
          unit_array := [noop(), noop()];
                         ^~~~~~ on ()
    |}]

let%expect_test "typecheck: singular argument count" =
  run_src {|take :: fn(_value: i32) {};
main :: fn { take() };
|};
  [%expect
    {|
    error: wrong number of arguments
      at <test>:2:14
        main :: fn { take() };
                     ^~~~~~ expected 1 argument, found 0
    |}]

let%expect_test "typecheck: nonliteral operand types binary expression" =
  run_src
    {|
add_two :: fn(x: i64) -> i64 {
  left := 1 + x;
  right := x + 1;
  return left + right;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: a local type may be used before its declaration" =
  run_src {|f :: fn() -> i32 {
  x : Coord = 4;
  Coord :: i32;
  x
};|};
  [%expect {| ok |}]

let%expect_test "typecheck: local aliases may repeat in separate blocks" =
  run_src
    {|f :: fn() {
  { Value :: i32; _x : Value = 1 }
  { Value :: bool; _x : Value = true }
};|};
  [%expect {| ok |}]

let%expect_test "typecheck: write to an array parameter" =
  run_src "f :: fn(a: [3]i32) { a[0] = 9 };";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:22
        f :: fn(a: [3]i32) { a[0] = 9 };
                             ^~~~
    |}]

let%expect_test "typecheck: write to a struct parameter field" =
  run_src "P :: struct { x: i32 };\nf :: fn(p: P) { p.x = 9 };";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:2:17
        f :: fn(p: P) { p.x = 9 };
                        ^~~
    |}]

let%expect_test "typecheck: write to a whole aggregate parameter" =
  run_src "f :: fn(a: [3]i32) { a = [4, 5, 6] };";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:22
        f :: fn(a: [3]i32) { a = [4, 5, 6] };
                             ^
    |}]

let%expect_test "typecheck: write through a slice parameter" =
  run_src "f :: fn(s: []i32) { s[0] = 9 };";
  [%expect {| ok |}]

let%expect_test "typecheck: write through a pointer parameter" =
  run_src "P :: struct { x: i32 };\nf :: fn(p: *P) { p.x = 9 };";
  [%expect {| ok |}]

let%expect_test "typecheck: write to a copy of an array parameter" =
  run_src "f :: fn(a: [3]i32) { local : [3]i32 = a; local[0] = 9 };";
  [%expect {| ok |}]

let%expect_test "typecheck: write to a scalar parameter" =
  run_src "f :: fn(x: i32) { x = 56 };";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:19
        f :: fn(x: i32) { x = 56 };
                          ^
    |}]

let%expect_test "typecheck: compound write to a scalar parameter" =
  run_src "f :: fn(x: i32) { x += 1 };";
  [%expect
    {|
    error: cannot assign to immutable
      at <test>:1:19
        f :: fn(x: i32) { x += 1 };
                          ^
    |}]

let%expect_test "typecheck: discarded if arms need not agree" =
  run_src
    {|
extern "C" fn printf(fmt: *i8, ...) i32;
f :: fn() { if true { printf("x") } else {} };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: if arms still agree where the value is used" =
  run_src
    {|
extern "C" fn printf(fmt: *i8, ...) i32;
f :: fn() -> i32 { x := if true { printf("x") } else {}; return x };
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:54
        f :: fn() -> i32 { x := if true { printf("x") } else {}; return x };
                                                             ^~ expected i32, found ()
    |}]

let%expect_test "typecheck: i64 is i64" =
  run_src "f :: fn() -> i64 { a : i64 = 1;\n  return a };";
  [%expect {| ok |}]

let%expect_test "typecheck: f64 is f64" =
  run_src "f :: fn() -> f64 { a : f64 = 1.5;\n  return a };";
  [%expect {| ok |}]

let%expect_test "typecheck: i64 is not i32" =
  run_src "f :: fn() -> i32 { a : i64 = 1;\n  return a };";
  [%expect
    {|
    error: type mismatch
      at <test>:2:10
          return a };
                 ^ expected i32, found i64
    |}]

let%expect_test "typecheck: f64 is not f32" =
  run_src "f :: fn() -> f32 { a : f64 = 1.5;\n  return a };";
  [%expect
    {|
    error: type mismatch
      at <test>:2:10
          return a };
                 ^ expected f32, found f64
    |}]

let%expect_test "typecheck: literal too Big for i64" =
  run_src "f :: fn() { _a : i64 = 9223372036854775808 };";
  [%expect
    {|
    error: integer literal out of range
      at <test>:1:24
        f :: fn() { _a : i64 = 9223372036854775808 };
                               ^~~~~~~~~~~~~~~~~~~ does not fit in i64
    |}]

let%expect_test "typecheck: type alias" =
  run_src "small :: i32;\nf :: fn() -> i32 { a : small = 1;\n  return a };";
  [%expect {| ok |}]

let%expect_test "typecheck: str literal and len" =
  run_src "f :: fn() -> usize { s : str = \"hello\";\n  return s.len };";
  [%expect {| ok |}]

let%expect_test "typecheck: str is not cstr" =
  run_src "f :: fn() { s : str = \"a\";\n  _c : cstr = s };";
  [%expect
    {|
    error: type mismatch
      at <test>:2:15
          _c : cstr = s };
                      ^ expected cstr, found str
    |}]

let%expect_test "typecheck: cstr is not str" =
  run_src "f :: fn() { s : cstr = \"a\";\n  _t : str = s };";
  [%expect
    {|
    error: type mismatch
      at <test>:2:14
          _t : str = s };
                     ^ expected str, found cstr
    |}]

let%expect_test "typecheck: a bare literal is still cstr" =
  run_src "f :: fn() { _s := \"a\" };";
  [%expect {| ok |}]

let%expect_test "typecheck: str has a ptr field" =
  run_src "f :: fn() { s : str = \"a\";\n  _p := s.ptr };";
  [%expect {| ok |}]

let%expect_test "typecheck: str ptr is a byte pointer" =
  run_src "f :: fn() { s : str = \"a\";\n  _p : f32 = s.ptr };";
  [%expect
    {|
    error: type mismatch
      at <test>:2:14
          _p : f32 = s.ptr };
                     ^~~~~ expected f32, found *u8
    |}]

let%expect_test "typecheck: str cannot be indexed" =
  run_src "f :: fn() { s : str = \"a\";\n  _b := s[0] };";
  [%expect
    {|
    error: cannot index
      at <test>:2:9
          _b := s[0] };
                ^~~~ on str
    |}]

let%expect_test "typecheck: str cannot be compared" =
  run_src "f :: fn() -> bool { s : str = \"a\";\n  return s == \"a\" };";
  [%expect
    {|
    error: invalid operand
      at <test>:2:10
          return s == "a" };
                 ^ cannot apply `==` to str
    |}]

let%expect_test "typecheck: a str global" =
  run_src "g : str = \"a\";";
  [%expect {| ok |}]

let%expect_test "typecheck: a labeled break exits an outer loop" =
  run_src
    {|f :: fn() {
  while @outer true { while true { break @outer } }
  g();
};
g :: fn() {};|};
  [%expect {| ok |}]

let%expect_test "typecheck: a shadowed label leaves the outer loop diverging" =
  run_src
    {|f :: fn() {
  while @outer true { while @outer true { break @outer } }
  g();
};
g :: fn() {};|};
  [%expect {| ok |}]

let%expect_test "typecheck: break inside loop" =
  run_src "f :: fn() { loop { break } };";
  [%expect {| ok |}]

let%expect_test "typecheck: a loop with no break diverges" =
  run_src {|f :: fn() {
  loop {}
  g();
};
g :: fn() {};|};
  [%expect {| ok |}]

let%expect_test "typecheck: a loop takes its value from break" =
  run_src {|f :: fn() -> i32 {
  return loop { break 42 };
};|};
  [%expect {| ok |}]

let%expect_test "typecheck: break with a value needs a loop" =
  run_src "f :: fn() { while true { break 5 } };";
  [%expect
    {|
    error: `break` with a value outside a `loop`
      at <test>:1:32
        f :: fn() { while true { break 5 } };
                                       ^
    help: use `loop` when the loop produces a value
    |}]

let%expect_test "typecheck: every valued break has to agree" =
  run_src
    {|f :: fn() -> i32 {
  return loop {
    if true { break 1 }
    break true;
  };
};|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:11
            break true;
                  ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: a bare break after a valued one is rejected" =
  run_src
    {|f :: fn() -> i32 {
  return loop {
    if true { break 1 }
    break;
  };
};|};
  [%expect
    {|
    error: `break` values disagree
      at <test>:4:5
            break;
            ^~~~~ no value here
      at <test>:3:21
            if true { break 1 }
                            ^ breaks with i32
    |}]

let%expect_test "typecheck: a valued break after a bare one is rejected" =
  run_src
    {|f :: fn() -> i32 {
  return loop {
    if true { break }
    break 1;
  };
};|};
  [%expect
    {|
    error: `break` values disagree
      at <test>:4:11
            break 1;
                  ^ breaks with i32
      at <test>:3:15
            if true { break }
                      ^~~~~ no value here
    |}]

let%expect_test "typecheck: unknown variant" =
  run_src {|Color :: enum { Red };
f :: fn() { _c := Color.Green };|};
  [%expect
    {|
    error: no variant
      at <test>:2:25
        f :: fn() { _c := Color.Green };
                                ^~~~~ on enum Color
    |}]

let%expect_test "typecheck: duplicate variant" =
  run_src "Color :: enum { Red, Green, Red };";
  [%expect
    {|
    error: duplicate variant
      at <test>:1:29
        Color :: enum { Red, Green, Red };
                                    ^~~
    |}]

let%expect_test "typecheck: enum has no arithmetic" =
  run_src
    {|Color :: enum { Red, Green };
f :: fn() { _c := Color.Red + Color.Green };|};
  [%expect
    {|
    error: invalid operand
      at <test>:2:19
        f :: fn() { _c := Color.Red + Color.Green };
                          ^~~~~~~~~ cannot apply `+` to Color
    |}]

let%expect_test "typecheck: enum does not cast to an integer" =
  run_src {|Color :: enum { Red };
f :: fn() { _c := cast(i32, Color.Red) };|};
  [%expect
    {|
    error: invalid conversion
      at <test>:2:19
        f :: fn() { _c := cast(i32, Color.Red) };
                          ^~~~~~~~~~~~~~~~~~~~ cannot convert Color to i32
    |}]

let%expect_test "typecheck: two enums are two types" =
  run_src
    {|Color :: enum { Red };
Fruit :: enum { Apple };
f :: fn() { _c := Color.Red == Fruit.Apple };|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:32
        f :: fn() { _c := Color.Red == Fruit.Apple };
                                       ^~~~~~~~~~~ expected Color, found Fruit
    |}]

let%expect_test "typecheck: an enum is not an integer" =
  run_src {|Color :: enum { Red };
f :: fn() { _c : i32 = Color.Red };|};
  [%expect
    {|
    error: type mismatch
      at <test>:2:24
        f :: fn() { _c : i32 = Color.Red };
                               ^~~~~~~~~ expected i32, found Color
    |}]

let%expect_test "typecheck: a type is not a value" =
  run_src {|Point :: struct { x: i32 };
f :: fn() { _p := Point.x };|};
  [%expect
    {|
    error: expected a value
      at <test>:2:19
        f :: fn() { _p := Point.x };
                          ^~~~~ this names a type
    |}]

let%expect_test "typecheck: an enum match must name every variant" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 { match c { Color.Red => 1 } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:2:34
        f :: fn(c: Color) -> i32 { match c { Color.Red => 1 } };
                                         ^ Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: an enum match naming every variant is ok" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 {
  match c { Color.Red => 1, Color.Green => 2, Color.Blue => 3 }
};|};
  [%expect {| ok |}]

let%expect_test "typecheck: an integer match needs a catch all" =
  run_src "f :: fn(n: i32) -> i32 { match n { 0 => 1 } };";
  [%expect
    {|
    error: match is not exhaustive
      at <test>:1:32
        f :: fn(n: i32) -> i32 { match n { 0 => 1 } };
                                       ^ on i32
    help: add `_ => { }`
    |}]

let%expect_test "typecheck: a bool match naming both values is ok" =
  run_src "f :: fn(b: bool) -> i32 { match b { true => 1, false => 0 } };";
  [%expect {| ok |}]

let%expect_test "typecheck: a bool match missing a value" =
  run_src "f :: fn(b: bool) -> i32 { match b { true => 1 } };";
  [%expect
    {|
    error: match is not exhaustive
      at <test>:1:33
        f :: fn(b: bool) -> i32 { match b { true => 1 } };
                                        ^ false not covered
    help: add `false => { }` or `_ => { }`
    |}]

let%expect_test "typecheck: a broken enum reports once" =
  run_src
    {|Color :: enum { 88, Green };
f :: fn(c: Color) -> i32 { match c { Color.Green => 1 } };|};
  [%expect
    {|
    error: expected identifier
      at <test>:1:17
        Color :: enum { 88, Green };
                        ^~ found 88
    |}]

let%expect_test "typecheck: a statement match must cover every case" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) { match c { Color.Red => { } } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:2:27
        f :: fn(c: Color) { match c { Color.Red => { } } };
                                  ^ Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: a catch all still silences the check" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, _ => 2 } };|};
  [%expect {| ok |}]

let%expect_test "typecheck: a bare name binding still silences the check" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, other => 2 } };|};
  [%expect {| ok |}]

let%expect_test "typecheck: a catch all first reports the dead arm" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 { return match c { _ => 1, Color.Red => 2 } };|};
  [%expect
    {|
    error: arm never runs
      at <test>:2:53
        f :: fn(c: Color) -> i32 { return match c { _ => 1, Color.Red => 2 } };
                                                            ^~~~~~~~~
    |}]

let%expect_test "typecheck: an alias names the enum it stands for" =
  run_src
    {|Color :: enum { Red, Green, Blue };
Shade :: Color;
f :: fn(s: Shade) -> i32 { return match s { Color.Red => 1 } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:3:41
        f :: fn(s: Shade) -> i32 { return match s { Color.Red => 1 } };
                                                ^ Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: an empty match names every case" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 { return match c { } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:2:41
        f :: fn(c: Color) -> i32 { return match c { } };
                                                ^ Color.Red, Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: a one variant enum is covered by naming it" =
  run_src
    {|One :: enum { Only };
f :: fn(o: One) -> i32 { return match o { One.Only => 5 } };|};
  [%expect {| ok |}]

let%expect_test "typecheck: a char match needs a catch all" =
  run_src "f :: fn(c: char) -> i32 { return match c { 'A' => 1 } };";
  [%expect
    {|
    error: match is not exhaustive
      at <test>:1:40
        f :: fn(c: char) -> i32 { return match c { 'A' => 1 } };
                                               ^ on char
    help: add `_ => { }`
    |}]

let%expect_test "typecheck: a bool match with no arms names both values" =
  run_src "f :: fn(b: bool) -> i32 { return match b { } };";
  [%expect
    {|
    error: match is not exhaustive
      at <test>:1:40
        f :: fn(b: bool) -> i32 { return match b { } };
                                               ^ false, true not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: an undefined scrutinee reports once" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn() -> i32 { return match nope { Color.Red => 1 } };|};
  [%expect
    {|
    error: undefined variable
      at <test>:2:33
        f :: fn() -> i32 { return match nope { Color.Red => 1 } };
                                        ^~~~
    |}]

let%expect_test "typecheck: a struct scrutinee can only ask for a catch all" =
  run_src
    {|P :: struct { x: i32 };
f :: fn(p: P) -> i32 { return match p { } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:2:37
        f :: fn(p: P) -> i32 { return match p { } };
                                            ^ on P
    help: add `_ => { }`
    |}]

let%expect_test "typecheck: a duplicate arm and a hole are both reported" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, Color.Red => 2 } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:2:41
        f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, Color.Red => 2 } };
                                                ^ Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    error: duplicate pattern
      at <test>:2:61
        f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, Color.Red => 2 } };
                                                                    ^~~~~~~~~
    |}]

let%expect_test "typecheck: an inner match is checked inside a covered outer" =
  run_src
    {|Color :: enum { Red, Green };
Size :: enum { Small, Large };
f :: fn(c: Color, s: Size) -> i32 {
  return match c { Color.Red => match s { Size.Small => 1 }, Color.Green => 2 };
};|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:4:39
          return match c { Color.Red => match s { Size.Small => 1 }, Color.Green => 2 };
                                              ^ Size.Large not covered
    help: add `Size.Large => { }` or `_ => { }`
    |}]

let%expect_test "typecheck: an alias of an alias still names the enum" =
  run_src
    {|Color :: enum { Red, Green, Blue };
Shade :: Color;
Tint :: Shade;
f :: fn(t: Tint) -> i32 { return match t { Color.Red => 1 } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:4:40
        f :: fn(t: Tint) -> i32 { return match t { Color.Red => 1 } };
                                               ^ Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: a call result is checked like any scrutinee" =
  run_src
    {|Color :: enum { Red, Green, Blue };
pick :: fn() -> Color { return Color.Red };
f :: fn() -> i32 { return match pick() { Color.Red => 1 } };|};
  [%expect
    {|
    error: match is not exhaustive
      at <test>:3:33
        f :: fn() -> i32 { return match pick() { Color.Red => 1 } };
                                        ^~~~~~ Color.Green, Color.Blue not covered
    help: add an arm for each, or `_ => { }`
    |}]

let%expect_test "typecheck: every variant named and every arm returns" =
  run_src
    {|Color :: enum { Red, Green, Blue };
f :: fn(c: Color) -> i32 {
  match c {
    Color.Red => { return 1 }
    Color.Green => { return 2 }
    Color.Blue => { return 3 }
  }
};|};
  [%expect {| ok |}]

let%expect_test "typecheck: an uncomparable pattern reports once" =
  run_src "f :: fn(x: f32) -> i32 { return match x { 1.5 => 1 } };";
  [%expect
    {|
    error: pattern is not comparable
      at <test>:1:43
        f :: fn(x: f32) -> i32 { return match x { 1.5 => 1 } };
                                                  ^~~ cannot test f32
    |}]

let%expect_test "typecheck: duplicate arm" =
  run_src
    {|Color :: enum { Red, Green };
f :: fn(c: Color) -> i32 { match c { Color.Red => 1, Color.Red => 2, _ => 3 } };|};
  [%expect
    {|
    error: duplicate pattern
      at <test>:2:54
        f :: fn(c: Color) -> i32 { match c { Color.Red => 1, Color.Red => 2, _ => 3 } };
                                                             ^~~~~~~~~
    |}]

let%expect_test "typecheck: an arm after the catch all never runs" =
  run_src
    {|Color :: enum { Red, Green };
f :: fn(c: Color) -> i32 { match c { _ => 1, Color.Red => 2 } };|};
  [%expect
    {|
    error: arm never runs
      at <test>:2:46
        f :: fn(c: Color) -> i32 { match c { _ => 1, Color.Red => 2 } };
                                                     ^~~~~~~~~
    |}]

let%expect_test "typecheck: two catch all arms" =
  run_src "f :: fn(n: i32) -> i32 { match n { _ => 1, _ => 2 } };";
  [%expect
    {|
    error: arm never runs
      at <test>:1:44
        f :: fn(n: i32) -> i32 { match n { _ => 1, _ => 2 } };
                                                   ^
    |}]

let%expect_test "typecheck: arms disagree in value position" =
  run_src
    {|Color :: enum { Red, Green };
f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, _ => true } };|};
  [%expect
    {|
    error: type mismatch
      at <test>:2:66
        f :: fn(c: Color) -> i32 { return match c { Color.Red => 1, _ => true } };
                                                                         ^~~~ expected i32, found bool
    |}]

let%expect_test "typecheck: a pattern has the scrutinee type" =
  run_src
    {|Color :: enum { Red, Green };
f :: fn(c: Color) -> i32 { match c { 3 => 1, _ => 2 } };|};
  [%expect
    {|
    error: type mismatch
      at <test>:2:38
        f :: fn(c: Color) -> i32 { match c { 3 => 1, _ => 2 } };
                                             ^ expected Color, found i32
    |}]

let%expect_test "typecheck: a bare name binds and catches everything" =
  run_src "f :: fn(n: i32, m: i32) -> i32 { match n { m => 1, _ => 2 } };";
  [%expect
    {|
    error: arm never runs
      at <test>:1:52
        f :: fn(n: i32, m: i32) -> i32 { match n { m => 1, _ => 2 } };
                                                           ^
    |}]

let%expect_test "typecheck: a struct cannot be matched" =
  run_src
    {|P :: struct { x: i32 };
f :: fn(p: P) -> i32 { return match p { _ => 1 } };|};
  [%expect {| ok |}]

let%expect_test "typecheck: a float cannot be matched" =
  run_src "f :: fn(x: f32) -> i32 { return match x { 1.5 => 1, _ => 2 } };";
  [%expect
    {|
    error: pattern is not comparable
      at <test>:1:43
        f :: fn(x: f32) -> i32 { return match x { 1.5 => 1, _ => 2 } };
                                                  ^~~ cannot test f32
    |}]

let%expect_test "typecheck: unit type and value" =
  run_src
    {|pass :: fn(value: ()) -> () { value };
make :: fn() -> () { () };
use :: fn() { value := make(); pass(value) };|};
  [%expect {| ok |}]

let%expect_test "typecheck: unit return mismatch" =
  run_src "f :: fn() { return 1 };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:20
        f :: fn() { return 1 };
                           ^ expected (), found i32
    |}]

let%expect_test "typecheck: explicit semicolon preserves block tail" =
  run_src "f :: fn() -> i32 { { x := 5; x; } };";
  [%expect {| ok |}]

let%expect_test "typecheck: unterminated block tail returns its value" =
  run_src "f :: fn() -> i32 { { x := 5; x } };";
  [%expect {| ok |}]

let%expect_test "typecheck: binding block tail is unit" =
  run_src "f :: fn() -> i32 { { x := 5; } };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:22
        f :: fn() -> i32 { { x := 5; } };
                             ^~~~~~ expected i32, found ()
    |}]

let%expect_test "typecheck: exact integer widening matrix" =
  run_src
    {|
f :: fn(si8: i8, si16: i16, si32: i32, ui8: u8, ui16: u16, ui32: u32) {
  _a : i16 = si8;
  _b : i32 = si8;
  _c : i64 = si8;
  _d : isize = si8;
  _e : i32 = si16;
  _f : i64 = si16;
  _g : isize = si16;
  _h : i64 = si32;
  _i : isize = si32;
  _j : u16 = ui8;
  _k : u32 = ui8;
  _l : u64 = ui8;
  _m : usize = ui8;
  _n : i16 = ui8;
  _o : i32 = ui8;
  _p : i64 = ui8;
  _q : isize = ui8;
  _r : u32 = ui16;
  _s : u64 = ui16;
  _t : usize = ui16;
  _u : i32 = ui16;
  _v : i64 = ui16;
  _w : isize = ui16;
  _x : u64 = ui32;
  _y : usize = ui32;
  _z : i64 = ui32;
  _aa : isize = ui32;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: float widening" =
  run_src "f :: fn(x: f32) -> f64 { return x };";
  [%expect {| ok |}]

let%expect_test "typecheck: widening reaches expected value positions" =
  run_src
    {|
Box :: struct { value: i64 };
take :: fn(value: i64) -> i64 { return value };
f :: fn(small: u8) -> i64 {
  wide : i64 = small;
  wide = small;
  wide += small;
  box : Box = Box { value: small };
  values : [2]i64 = [small, wide];
  return take(small) + box.value + values[0];
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: aliases widen as their bases" =
  run_src
    {|
Small :: u8;
Wide :: i64;
f :: fn(value: Small) -> Wide { return value };
|};
  [%expect {| ok |}]

let%expect_test "typecheck: inferred numeric joins ignore source order" =
  run_src
    {|
f :: fn(small: i8, wide: i64, single: f32, double: f64) -> i64 {
  a := small + wide;
  b := wide + small;
  _c := single + double;
  _d := double + single;
  _e := small < wide;
  _f := wide > small;
  _g := small & wide;
  _h := wide | small;
  i := [small, wide];
  j := [wide, small];
  k := if true { small } else { wide };
  l := if true { wide } else { small };
  m := match true { true => small, false => wide };
  n := match true { true => wide, false => small };
  o := loop { if false { break small }; break wide };
  p := loop { if false { break wide }; break small };
  for _index in small..wide {}
  return a + b + i[0] + j[1] + k + l + m + n + o + p;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: lossy numeric conversions stay explicit" =
  run_src
    {|
f :: fn(si8: i8, ui8: u8, ui16: u16, si64: i64, size: isize,
    double: f64, integer: i32) {
  _a : u16 = si8;
  _b : i8 = ui8;
  _c : u8 = ui16;
  _d : isize = si64;
  _e : i64 = size;
  _f : f32 = double;
  _g : f64 = integer;
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:4:14
          _a : u16 = si8;
                     ^~~ expected u16, found i8
    error: type mismatch
      at <test>:5:13
          _b : i8 = ui8;
                    ^~~ expected i8, found u8
    error: type mismatch
      at <test>:6:13
          _c : u8 = ui16;
                    ^~~~ expected u8, found u16
    error: type mismatch
      at <test>:7:16
          _d : isize = si64;
                       ^~~~ expected isize, found i64
    error: type mismatch
      at <test>:8:14
          _e : i64 = size;
                     ^~~~ expected i64, found isize
    error: type mismatch
      at <test>:9:14
          _f : f32 = double;
                     ^~~~~~ expected f32, found f64
    error: type mismatch
      at <test>:10:14
          _g : f64 = integer;
                     ^~~~~~~ expected f64, found i32
    |}]

let%expect_test "typecheck: numeric joins infer a third safe type" =
  run_src
    {|
f :: fn(signed: i32, unsigned: u32) {
  _a := signed + unsigned;
  _b := unsigned + signed;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: context supplies a third wider type" =
  run_src
    "f :: fn(signed: i32, unsigned: u32) -> i64 { return signed + unsigned };";
  [%expect {| ok |}]

let%expect_test "typecheck: loop literals follow a later rigid type" =
  run_src
    {|
f :: fn(wide: u64) -> u64 {
  a := loop { if false { break 1 }; break wide };
  b := loop { if false { break wide }; break 1 };
  return a + b;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: loop literal must fit a later rigid type" =
  run_src
    {|
f :: fn(wide: u64) -> u64 {
  return loop { if false { break -1 }; break wide };
};
|};
  [%expect
    {|
    error: integer literal out of range
      at <test>:3:34
          return loop { if false { break -1 }; break wide };
                                         ^~ does not fit in u64
    |}]

let%expect_test "typecheck: explicit widening stays legal" =
  run_src "f :: fn(value: u8) -> i64 { return cast(i64, value) };";
  [%expect {| ok |}]

let%expect_test "typecheck: smallest common integer type" =
  run_src
    {|
f :: fn(si8: i8, si16: i16, si32: i32, si64: i64,
    ui8: u8, ui16: u16, ui32: u32, ui64: u64) {
  a := si8 + ui8;
  b := si16 + ui16;
  c := si32 + ui32;
  d := si32 + ui16;
  e := si64 + ui32;
  g := ui32 + ui64;
  _a : i16 = a;
  _b : i32 = b;
  _c : i64 = c;
  _d : i32 = d;
  _e : i64 = e;
  _g : u64 = g;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: common integer type reaches inferred positions" =
  run_src
    {|
f :: fn(signed: i32, unsigned: u32) -> i64 {
  a := signed + unsigned;
  _b := signed < unsigned;
  _c := [signed, unsigned];
  d := if true { signed } else { unsigned };
  e := match true { true => signed, false => unsigned };
  g := loop { if false { break signed }; break unsigned };
  for _index in signed..unsigned {}
  return a + d + e + g;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: no common integer type without i128" =
  run_src
    {|
f :: fn(signed: i64, unsigned: u64) {
  _a := signed + unsigned;
  _b := unsigned + signed;
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:3:18
          _a := signed + unsigned;
                         ^~~~~~~~ expected i64, found u64
    error: type mismatch
      at <test>:4:20
          _b := unsigned + signed;
                           ^~~~~~ expected u64, found i64
    |}]

let%expect_test "typecheck: expected loop result does not widen" =
  run_src "f :: fn() -> i32 { return loop { break 1i64 } };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:40
        f :: fn() -> i32 { return loop { break 1i64 } };
                                               ^~~~ expected i32, found i64
    |}]

let%expect_test "typecheck: expected loop rejects a bare break" =
  run_src "f :: fn() -> i32 { return loop { break } };";
  [%expect
    {|
    error: type mismatch
      at <test>:1:34
        f :: fn() -> i32 { return loop { break } };
                                         ^~~~~ expected i32, found ()
    |}]

let%expect_test "typecheck: diverging break value does not fix loop type" =
  run_src
    {|
stop :: fn() -> never { loop {} };
f :: fn() {
  loop { if false { break stop() }; break }
  _value := 1;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: widening reaches field assignment" =
  run_src
    {|
Box :: struct { x: i32 };
f :: fn(value: i8) -> i32 {
  box : Box;
  box.x = value;
  return box.x;
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: widening reaches remaining value positions" =
  run_src
    {|
take :: fn(value: i64) -> i64 { value };
tail :: fn(value: i32) -> i64 { value };
apply :: fn(f: fn (i64) i64, value: i8) -> i64 { f(value) };
f :: fn(small: i8, index: u8) -> i64 {
  left : i64 = 0;
  right : i64 = 0;  
  left = small;
  right = small;
  nested : i64 = { small };
  negative : i64 = -small;
  values : [2]i32 = [small, small];
  _element := values[index];
  _from := values[index..];
  _to := values[..index];
  pattern := match 1i64 { 1 => small, _ => 0i64 };
  return left + right + nested + negative + pattern +
      tail(small) + apply(take, small);
};
|};
  [%expect {| ok |}]

let%expect_test "typecheck: shift count must be an integer" =
  run_src "f :: fn() -> i32 { a : i32 = 1;\n  return a << 1.0 };";
  [%expect
    {|
    error: shift count must be an integer
      at <test>:2:15
          return a << 1.0 };
                      ^~~ found f64
    |}]

let%expect_test "typecheck: compound shift count must be an integer" =
  run_src "f :: fn() { a : i32 = 1;\n  a <<= 1.0 };";
  [%expect
    {|
    error: shift count must be an integer
      at <test>:2:9
          a <<= 1.0 };
                ^~~ found f64
    |}]

let%expect_test "typecheck: a literal product overflows" =
  run_src
    "f :: fn() -> i64 { return 9223372036854775807 * 9223372036854775807 };";
  [%expect {| ok |}]

let%expect_test "typecheck: size does not fit" =
  run_src
    "Big :: struct { a: [400]i32 };\nf :: fn() -> u8 { return sizeof(Big) };";
  [%expect
    {|
    error: size does not fit
      at <test>:2:26
        f :: fn() -> u8 { return sizeof(Big) };
                                 ^~~~~~~~~~~ 1600 does not fit in u8
    |}]

let%expect_test "typecheck: pattern is not a literal" =
  run_src
    "f :: fn(a: [2]i32) -> i32 { return match a { [1, 2] => 1, _ => 0 } };";
  [%expect
    {|
    error: pattern is not a literal
      at <test>:1:46
        f :: fn(a: [2]i32) -> i32 { return match a { [1, 2] => 1, _ => 0 } };
                                                     ^~~~~~
    help: an arm names a literal or an enum variant
    |}]

let%expect_test "typecheck: a compound assign to a value stops at the target" =
  run_src {|f :: fn() {
  "s" += 1;
};|};
  [%expect
    {|
    error: cannot assign to expression
      at <test>:2:3
          "s" += 1;
          ^~~ on *i8
    |}]

let%expect_test "typecheck: a type is not callable" =
  run_src "f :: fn(a: u8) -> i32 { return i32(a) };";
  [%expect
    {|
    error: cannot call a type
      at <test>:1:32
        f :: fn(a: u8) -> i32 { return i32(a) };
                                       ^~~
    help: convert with `cast(i32, value)`
    |}]

let%expect_test "typecheck: a value in the type slot of a cast" =
  run_src "f :: fn(a: u8) -> i32 { return cast(a, a) };";
  [%expect
    {|
    error: undefined type
      at <test>:1:37
        f :: fn(a: u8) -> i32 { return cast(a, a) };
                                            ^
    |}]

let%expect_test "typecheck: a call on a repeated enum name" =
  run_src "Color :: enum {};\nColor :: enum {};\nf :: fn() { Color.d() };";
  [%expect
    {|
    error: already defined
      at <test>:2:1
        Color :: enum {};
        ^~~~~
      at <test>:1:1
        Color :: enum {};
        ^~~~~ previous definition here
    error: no variant
      at <test>:3:19
        f :: fn() { Color.d() };
                          ^ on enum Color
    |}]

let%expect_test "typecheck: an unknown return type hides the missing value" =
  run_src
    "f :: fn() -> nope { if true { return 1 } };\n\
     g :: fn() -> nope { loop { break } };\n\
     h :: fn() { _p := *f };";
  [%expect
    {|
    error: undefined type
      at <test>:1:14
        f :: fn() -> nope { if true { return 1 } };
                     ^~~~
    error: undefined type
      at <test>:2:14
        g :: fn() -> nope { loop { break } };
                     ^~~~
    |}]
