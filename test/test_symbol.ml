(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe

let%expect_test "symbol: is_func covers only func and extern" =
  List.iter
    (fun k -> Printf.printf "%s %b\n" (Symbol.show_kind k) (Symbol.is_func k))
    [ Func; Extern; Global; Const; Var; Param; ForVar ];
  [%expect
    {|
    Func true
    Extern true
    Global false
    Const false
    Var false
    Param false
    ForVar false
    |}]

let%expect_test "symbol: is_global covers only global" =
  List.iter
    (fun k -> Printf.printf "%s %b\n" (Symbol.show_kind k) (Symbol.is_global k))
    [ Func; Extern; Global; Const; Var; Param; ForVar ];
  [%expect
    {|
    Func false
    Extern false
    Global true
    Const false
    Var false
    Param false
    ForVar false
    |}]
