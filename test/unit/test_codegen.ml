(* SPDX-License-Identifier: Apache-2.0 *)

open Pipeline

let%expect_test "qbe accepts scalar MIR" =
  run_codegen_ok "main :: fn() -> i32 { return 2 + 3 };";
  [%expect {| ok |}]

let%expect_test "qbe accepts padded structs" =
  run_codegen_ok
    {|
P :: struct { a: i8, b: i64, c: i8 };
main :: fn() -> i32 { return sizeof(P) as i32 };
|};
  [%expect
    {|
    error: expected `;`
      at <test>:3:40
        main :: fn() -> i32 { return sizeof(P) as i32 };
                                               ^~ found as
    |}]

let%expect_test ("qbe accepts string aggregates" [@tags "disabled"]) =
  run_codegen_ok
    {|
Box :: struct { text: str, value: i32 };
make :: fn() -> str { return "hello" };
main :: fn() -> i32 {
  box : Box = Box { text: "field", value: 1 };
  copy : str = make();
  return (box.text.len + copy.len) as i32;
};
|};
  [%expect
    {|
    error: type mismatch
      at <test>:7:11
          return (box.text.len + copy.len) as i32;
                  ^~~~~~~~~~~~ expected i32, found usize
    error: type mismatch
      at <test>:7:26
          return (box.text.len + copy.len) as i32;
                                 ^~~~~~~~ expected i32, found usize
    error: expected `;`
      at <test>:7:36
          return (box.text.len + copy.len) as i32;
                                           ^~ found as
    |}]

let%expect_test "qbe accepts checked operations" =
  run_codegen_ok
    {|
main :: fn() -> i32 {
  values : [2]i32 = [10, 20];
  return values[1];
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts main allocation" =
  run_codegen_ok
    {|
Pair :: struct { left: i32, right: i32 };
main :: fn() -> i32 {
  pair := Pair { left: 1, right: 2 };
  return pair.left + pair.right;
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts local structs" =
  run_codegen_ok
    {|
main :: fn() -> i32 {
Pair :: struct { left: i32, right: i32 };
  pair := Pair { left: 1, right: 2 };
  return pair.left + pair.right;
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts external struct calls" =
  run_codegen_ok
    {|
Pair :: struct { left: i32, right: i32 };
extern "C" fn consume(pair: Pair) i32;
main :: fn() -> i32 {
  pair := Pair { left: 1, right: 2 };
  return consume(pair);
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts external struct returns" =
  run_codegen_ok
    {|
Pair :: struct { left: i32, right: i32 };
extern "C" fn produce() Pair;
main :: fn() -> i32 {
  pair := produce();
  return pair.left;
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts Ripe struct returns" =
  run_codegen_ok
    {|
Pair :: struct { left: i32, right: i32 };
extern "Ripe" fn produce() Pair;
main :: fn() -> i32 {
  pair := produce();
  return pair.left;
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts scalar locals" =
  run_codegen_ok
    {|
main :: fn() -> i32 {
  value : i32 = 1;
  value = value + 2;
  addressed : i32 = 3;
  pointer := &addressed;
  *pointer = value;
  return addressed;
};
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts extern aggregate arguments" =
  run_codegen_ok
    {|
extern "C" fn takes_slice(s: []i32) i32;
extern "C" fn takes_str(s: str) i32;
main :: fn() -> i32 {
  xs : [4]i32 = [1, 2, 3, 4];
  view : []i32 = xs[0..4];
  return takes_slice(view) + takes_str("hi");
};
|};
  [%expect {| ok |}]

let%expect_test "qbe hands a C export its aggregates by value" =
  run_codegen
    {|
Pair :: struct { left: i32, right: i32 };
extern "C" fn first(p: Pair) i32 { return p.left }
|};
  [%expect
    {|
    type :Pair = { w, w }

    export function w $first(:Pair %t0) {
    @start
    %p =l alloc4 8
    blit %t0, %p, 8
    %t1 =w loadsw %p
    ret %t1
    }
    |}]

let%expect_test "qbe returns a C aggregate to the caller" =
  run_codegen_ok
    {|
Pair :: struct { left: i32, right: i32 };
extern "C" fn make(x: i32) Pair { return Pair { left: x, right: x } }
main :: fn() -> i32 {
  pair := make(2);
  return pair.left + pair.right;
};
|};
  [%expect {| ok |}]

let%expect_test "qbe keeps the Ripe ABI for a Ripe export" =
  run_codegen
    {|
Pair :: struct { left: i32, right: i32 };
extern "Ripe" fn first(p: Pair) i32 { return p.left }
|};
  [%expect
    {|
    type :Pair = { w, w }

    export function w $first(l %t0) {
    @start
    %p =l alloc4 8
    blit %t0, %p, 8
    %t1 =w loadsw %p
    ret %t1
    }
    |}]
