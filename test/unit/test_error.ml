(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Spanutils
open Diag

let%expect_test "error: internal compiler error" =
  let src = "main :: fn() {};\n" in
  render src
    (Diagnostic.internal ~span:(span src "main") "test invariant failed");
  [%expect
    {|
    error: internal compiler error
      at <test>:1:1
        main :: fn() {};
        ^~~~
    test invariant failed
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]
