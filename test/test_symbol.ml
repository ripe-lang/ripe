(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe

let all_kinds =
  [
    Symbol.Func;
    Extern;
    Global Ast.Let;
    Global Ast.Comptime;
    Type;
    Local Ast.Let;
    Local Ast.Comptime;
    Local Ast.Var;
    Param;
    ForVar;
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
    (Global Let) false
    (Global Comptime) false
    Type false
    (Local Let) false
    (Local Comptime) false
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
    (Global Let) true
    (Global Comptime) true
    Type false
    (Local Let) false
    (Local Comptime) false
    (Local Var) false
    Param false
    ForVar false
    |}]
