(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let show name = print_endline (Mangle.declaration name)

let%expect_test "mangle: a name gets its length up front" =
  show "main";
  [%expect {| _R4main |}]

let%expect_test "mangle: a name longer than nine characters" =
  show "binary_search";
  [%expect {| _R13binary_search |}]
