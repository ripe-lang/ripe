(* SPDX-License-Identifier: GPL-2.0-only *)

open Pipeline

let%expect_test "qbe accepts scalar MIR" =
  run_codegen_ok "func main() i32 { return 2 + 3 }";
  [%expect {| ok |}]

let%expect_test "qbe accepts padded structs" =
  run_codegen_ok
    {|
struct P { a: i8, b: i64, c: i8 }
func main() i32 { return sizeof(P) as i32 }
|};
  [%expect
    {|
    error: expected `;`
      at <test>:3:36
        func main() i32 { return sizeof(P) as i32 }
                                           ^~ found as
    |}]

let%expect_test "qbe accepts string aggregates" =
  run_codegen_ok
    {|
struct Box { text: str, value: i32 }
func make() str { return "hello" }
func main() i32 {
  var box: Box = Box { text: "field", value: 1 }
  var copy: str = make()
  return (box.text.len + copy.len) as i32
}
|};
  [%expect
    {|
    error: type mismatch
      at <test>:7:11
          return (box.text.len + copy.len) as i32
                  ^~~~~~~~~~~~ expected i32, found usize
    error: type mismatch
      at <test>:7:26
          return (box.text.len + copy.len) as i32
                                 ^~~~~~~~ expected i32, found usize
    error: expected `;`
      at <test>:7:36
          return (box.text.len + copy.len) as i32
                                           ^~ found as
    |}]

let%expect_test "qbe accepts checked operations" =
  run_codegen_ok
    {|
func main() i32 {
  var values: [2]i32 = [10, 20]
  return values[1]
}
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts public declarations" =
  run_codegen_ok
    {|
pub var count: i32 = 1
pub func value() i32 { return count }
func main() i32 { return value() }
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts main allocation" =
  run_codegen_ok
    {|
struct Pair { left: i32, right: i32 }
func main() i32 {
  var pair = Pair { left: 1, right: 2 }
  return pair.left + pair.right
}
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts local structs" =
  run_codegen_ok
    {|
func main() i32 {
  struct Pair { left: i32, right: i32 }
  var pair = Pair { left: 1, right: 2 }
  return pair.left + pair.right
}
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts external struct calls" =
  run_codegen_ok
    {|
struct Pair { left: i32, right: i32 }
extern "C" func consume(pair: Pair) i32
func main() i32 {
  var pair = Pair { left: 1, right: 2 }
  return consume(pair)
}
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts external struct returns" =
  run_codegen_ok
    {|
struct Pair { left: i32, right: i32 }
extern "C" func produce() Pair
func main() i32 {
  var pair = produce()
  return pair.left
}
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts Ripe struct returns" =
  run_codegen_ok
    {|
struct Pair { left: i32, right: i32 }
extern "Ripe" func produce() Pair
func main() i32 {
  var pair = produce()
  return pair.left
}
|};
  [%expect {| ok |}]

let%expect_test "qbe accepts scalar locals" =
  run_codegen_ok
    {|
func main() i32 {
  var value: i32 = 1
  value = value + 2
  var addressed: i32 = 3
  var pointer = &addressed
  *pointer = value
  return addressed
}
|};
  [%expect {| ok |}]
