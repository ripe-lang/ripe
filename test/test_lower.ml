(* SPDX-License-Identifier: GPL-2.0-only *)

open Pipeline

let%expect_test "lower: block statements land in the surrounding list" =
  run_lower
    {|
func f() i32 {
  var x: i32 = 1
  {
    var y: i32 = 2
    x = y
  }
  return x
}
|};
  [%expect {| f { bind x block { bind y expr } return } |}]

let%expect_test "lower: compound assignment splices flat" =
  run_lower "func f() i32 {\n  var x: i32 = 1\n  x += 2\n  return x\n}";
  [%expect {| f { bind x bind compound.p.0 expr return } |}]

let%expect_test "lower: pair assignment saves both values before stores" =
  run_lower "func f(a: i32, b: i32) { a, b = b, a }";
  [%expect {| f { bind pair.first.0 bind pair.second.1 expr expr } |}]
