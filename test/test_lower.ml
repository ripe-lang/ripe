(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

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
  run_lower "func f() i32 { var x: i32 = 1 x += 2 return x }";
  [%expect {| f { bind x bind compound.p.0 expr return } |}]
