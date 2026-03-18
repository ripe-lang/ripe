(* SPDX-License-Identifier: GPL-2.0-only *)

open Ripe.Ast

let e desc = mk_expr desc
let s sdesc = mk_stmt sdesc

let run decls =
  match Ripe.Typechecker.typecheck decls with
  | _ -> print_endline "ok"
  | exception Ripe.Typechecker.TypeError msg ->
      print_endline ("TypeError: " ^ msg)

let func ?(params = []) ?(ret = None) name body =
  Func { name; params; ret; body; modifiers = [] }

let%expect_test "break outside loop" =
  run [ func "f" [ s Break ] ];
  [%expect {| TypeError: break statement must be inside a loop |}]

let%expect_test "continue outside loop" =
  run [ func "f" [ s Continue ] ];
  [%expect {| TypeError: continue statement must be inside a loop |}]

let%expect_test "unbound variable" =
  run [ func "f" [ s (Expr (e (Ident "x"))) ] ];
  [%expect {| TypeError: unbound variable: x |}]

let%expect_test "type mismatch in let" =
  run [ func "f" [ s (Let ("x", Some (Named "bool"), e (Int 42))) ] ];
  [%expect {| TypeError: type mismatch: expected TBool, got (TInt I32) |}]

let%expect_test "wrong number of arguments" =
  run [ func "g" []; func "f" [ s (Expr (e (Call ("g", [ e (Int 1) ])))) ] ];
  [%expect {| TypeError: wrong number of arguments: expected 0, got 1 |}]

let%expect_test "deref non-pointer" =
  run [ func "f" [ s (Expr (e (UnOp (Deref, e (Int 42))))) ] ];
  [%expect {| TypeError: cannot dereference non-pointer type: (TInt I32) |}]

let%expect_test "null assigned to non-pointer" =
  run [ func "f" [ s (Let ("x", Some (Named "i32"), e Null)) ] ];
  [%expect {| TypeError: type mismatch: expected (TInt I32), got TNull |}]

let%expect_test "identity function" =
  run
    [
      func
        ~params:[ { name = "a"; typ = Named "i32" } ]
        ~ret:(Some (Named "i32")) "id"
        [ s (Return (Some (e (Ident "a")))) ];
    ];
  [%expect {| ok |}]

let%expect_test "null assigned to pointer" =
  run [ func "f" [ s (Let ("p", Some (Pointer (Named "i32")), e Null)) ] ];
  [%expect {| ok |}]

let%expect_test "break inside while" =
  run [ func "f" [ s (While (e (Bool true), [ s Break ])) ] ];
  [%expect {| ok |}]

let%expect_test "forward reference" =
  run [ func "f" [ s (Expr (e (Call ("g", [])))) ]; func "g" [] ];
  [%expect {| ok |}]
