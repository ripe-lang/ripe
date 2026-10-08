(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let all_kinds =
  [
    Symbol.Func;
    Extern;
    Global;
    Type;
    Local;
    Param;
    ForVar;
    MatchBind;
    Error;
    LocalFunc;
    LocalType;
  ]

let symbol id = Fake.symbol ~kind:Symbol.Func id

let dump_kinds pred =
  List.iter
    (fun kind -> Printf.printf "%s %b\n" (Symbol.show_kind kind) (pred kind))
    all_kinds

let%expect_test "symbol: is_func covers only fn and extern" =
  dump_kinds Symbol.is_func;
  [%expect
    {|
    Func true
    Extern true
    Global false
    Type false
    Local false
    Param false
    ForVar false
    MatchBind false
    Error false
    LocalFunc true
    LocalType false
    |}]

let%expect_test "symbol: is_global covers only global" =
  dump_kinds Symbol.is_global;
  [%expect
    {|
    Func false
    Extern false
    Global true
    Type false
    Local false
    Param false
    ForVar false
    MatchBind false
    Error false
    LocalFunc false
    LocalType false
    |}]

let%expect_test "symbol: is_immutable covers what cannot be assigned" =
  dump_kinds Symbol.is_immutable;
  [%expect
    {|
    Func false
    Extern false
    Global false
    Type false
    Local false
    Param true
    ForVar true
    MatchBind true
    Error false
    LocalFunc false
    LocalType false
    |}]

let%expect_test "symbol: two ids make two keys" =
  let a = Symbol.key (symbol 1) in
  let b = Symbol.key (symbol 2) in
  Printf.printf "same %b\n" (a = b);
  [%expect {| same false |}]

let%expect_test "symbol: a table finds a symbol by its key" =
  let table = Symbol.Table.create 8 in
  Symbol.Table.replace table (Symbol.key (symbol 1)) "one";
  Symbol.Table.replace table (Symbol.key (symbol 2)) "two";
  let get id =
    Option.value
      (Symbol.Table.find_opt table (Symbol.key (symbol id)))
      ~default:"none"
  in
  Printf.printf "%s | %s | %s | %d\n" (get 1) (get 2) (get 9)
    (Symbol.Table.length table);
  [%expect {| one | two | none | 2 |}]
