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
  let box: Box = Box { text: "field", value: 1 }
  let copy: str = make()
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
