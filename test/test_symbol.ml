(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe
open Helpers

let%expect_test "symbol: is_func covers only func and extern" =
  dump_kinds Symbol.is_func;
  [%expect
    {|
    Func true
    Extern true
    Global false
    (Local Let) false
    (Local Const) false
    (Local Var) false
    Param false
    ForVar false
    |}]

let%expect_test "symbol: is_global covers only global" =
  dump_kinds Symbol.is_global;
  [%expect
    {|
    Func false
    Extern false
    Global true
    (Local Let) false
    (Local Const) false
    (Local Var) false
    Param false
    ForVar false
    |}]
