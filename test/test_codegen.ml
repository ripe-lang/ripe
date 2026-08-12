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
  [%expect {| ok |}]

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
  [%expect {| ok |}]

let%expect_test "qbe accepts checked operations" =
  run_codegen_ok
    {|
func main() i32 {
  var values: [2]i32 = [10, 20]
  return values[1]
}
|};
  [%expect {| ok |}]

let%expect_test "qbe exports public declarations" =
  run_codegen_contains
    {|
pub var count: i32 = 1
pub func value() i32 { return count }
func main() i32 { return value() }
|}
    [ "export data $count"; "export function w $value" ];
  [%expect
    {|
    export data $count: true
    export function w $value: true
    |}]

let%expect_test "qbe preserves main allocation alignment" =
  run_codegen_contains
    {|
struct Pair { left: i32, right: i32 }
func main() i32 {
  var pair = Pair { left: 1, right: 2 }
  return pair.left + pair.right
}
|}
    [ "%pair =l alloc4 8" ];
  [%expect {| %pair =l alloc4 8: true |}]

let%expect_test "qbe gives local structs unique names" =
  run_codegen_contains
    {|
func main() i32 {
  struct Pair { left: i32, right: i32 }
  var pair = Pair { left: 1, right: 2 }
  return pair.left + pair.right
}
|}
    [ "type :_Rlocal" ];
  [%expect {| type :_Rlocal: true |}]

let%expect_test "qbe uses aggregate types for external struct calls" =
  run_codegen_contains
    {|
struct Pair { left: i32, right: i32 }
extern "C" func consume(pair: Pair) i32
func main() i32 {
  var pair = Pair { left: 1, right: 2 }
  return consume(pair)
}
|}
    [ "call $consume(:Pair " ];
  [%expect {| call $consume(:Pair : true |}]

let%expect_test "qbe uses aggregate types for external struct returns" =
  run_codegen_contains
    {|
struct Pair { left: i32, right: i32 }
extern "C" func produce() Pair
func main() i32 {
  var pair = produce()
  return pair.left
}
|}
    [ "=:Pair call $produce(" ];
  [%expect {| =:Pair call $produce(: true |}]

let%expect_test "qbe uses Ripe ABI for Ripe struct returns" =
  run_codegen_contains
    {|
struct Pair { left: i32, right: i32 }
extern "Ripe" func produce() Pair
func main() i32 {
  var pair = produce()
  return pair.left
}
|}
    [ "call $produce(l " ];
  [%expect {| call $produce(l : true |}]
