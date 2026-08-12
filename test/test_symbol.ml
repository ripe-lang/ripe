(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe

let all_kinds =
  [
    Symbol.Func;
    Extern;
    Global Ast.Var;
    Global Ast.Comptime;
    Type;
    Local Ast.Comptime;
    Local Ast.Var;
    Param;
    ForVar;
    MatchBind;
  ]

let dump_kinds pred =
  List.iter
    (fun kind -> Printf.printf "%s %b\n" (Symbol.show_kind kind) (pred kind))
    all_kinds

let%expect_test "symbol: is_func covers only func and extern" =
  dump_kinds Symbol.is_func;
  [%expect
    {|
    Func true
    Extern true
    (Global Var) false
    (Global Comptime) false
    Type false
    (Local Comptime) false
    (Local Var) false
    Param false
    ForVar false
    MatchBind false
    |}]

let%expect_test "symbol: is_global covers only global" =
  dump_kinds Symbol.is_global;
  [%expect
    {|
    Func false
    Extern false
    (Global Var) true
    (Global Comptime) true
    Type false
    (Local Comptime) false
    (Local Var) false
    Param false
    ForVar false
    MatchBind false
    |}]
