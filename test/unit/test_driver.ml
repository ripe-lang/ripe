(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let%expect_test "driver: every stage has a name the CLI can take" =
  let stages =
    [ Driver.Tokens; Ast; Resolve; Tast; Check; Mir; Qbe; Asm; Obj; Bin ]
  in
  print_endline (String.concat ", " (List.map Driver.stage_name stages));
  [%expect {| tokens, ast, resolve, tast, check, mir, qbe, asm, obj, bin |}]
