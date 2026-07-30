(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe

let show path name = print_endline (Mangle.declaration path name)

let%expect_test "mangle: a name at the root" =
  show [] "main";
  [%expect {| _R4main |}]

let%expect_test "mangle: a name inside one module" =
  show [ "math" ] "add";
  [%expect {| _R4math3add |}]

let%expect_test "mangle: a name inside a nested module" =
  show [ "math"; "vector" ] "first";
  [%expect {| _R4math6vector5first |}]

let%expect_test "mangle: a component longer than nine characters" =
  show [ "collections" ] "binary_search";
  [%expect {| _R11collections13binary_search |}]
