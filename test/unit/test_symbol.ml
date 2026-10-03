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
    Module;
  ]

let symbol module_id id = Fake.symbol ~module_id ~kind:Symbol.Func id

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
    Module false
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
    Module false
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
    Module true
    |}]

let%expect_test "symbol: a key packs a module and an id back apart" =
  let show module_id id =
    let key = Symbol.key (symbol module_id id) in
    Printf.printf "%d %d -> module %d id %d\n" module_id id
      (Symbol.module_id_of_key key)
      (Symbol.id_of_key key)
  in
  show 0 0;
  show 0 1;
  show 1 0;
  show 7 42;
  show 1000 999999;
  [%expect
    {|
    0 0 -> module 0 id 0
    0 1 -> module 0 id 1
    1 0 -> module 1 id 0
    7 42 -> module 7 id 42
    1000 999999 -> module 1000 id 999999
    |}]

let%expect_test "symbol: the prelude and unresolved keys stay out of the way" =
  Printf.printf "prelude %d\n" Symbol.prelude_module_id;
  Printf.printf "unresolved module %d\n"
    (Symbol.module_id_of_key Symbol.unresolved_key);
  [%expect {|
    prelude -2
    unresolved module -1
    |}]

let%expect_test "symbol: two ids in one module make two keys" =
  let a = Symbol.key (symbol 3 1) in
  let b = Symbol.key (symbol 3 2) in
  let c = Symbol.key (symbol 4 1) in
  Printf.printf "same %b across modules %b\n" (a = b) (a = c);
  [%expect {| same false across modules false |}]

let%expect_test "symbol: a table keeps the module apart from the id" =
  let table = Symbol.Table.create 8 in
  Symbol.Table.replace table (Symbol.key (symbol 1 2)) "one two";
  Symbol.Table.replace table (Symbol.key (symbol 2 1)) "two one";
  let get module_id id =
    Option.value
      (Symbol.Table.find_opt table (Symbol.key (symbol module_id id)))
      ~default:"none"
  in
  Printf.printf "%s | %s | %s | %d\n" (get 1 2) (get 2 1) (get 9 9)
    (Symbol.Table.length table);
  [%expect {| one two | two one | none | 2 |}]
