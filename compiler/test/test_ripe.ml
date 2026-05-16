(* SPDX-License-Identifier: GPL-2.0-only *)

let parse src =
  Ripe.Lexer.reset ();
  let lexbuf = Lexing.from_string src in
  Ripe.Parser.parse Ripe.Lexer.read lexbuf

let run_src src =
  match parse src with
  | decls -> (
      match Ripe.Typechecker.typecheck "<test>" src decls with
      | _ -> print_endline "ok"
      | exception Ripe.Typechecker.TypeErrors msgs ->
          List.iter (fun msg -> print_endline ("TypeError: " ^ msg)) msgs)
  | exception Ripe.Parser.ParseError (_, msg) ->
      print_endline ("ParseError: " ^ msg)

let%expect_test "break outside loop" =
  run_src "f() { break }";
  [%expect {| TypeError: <test>:1:7: break outside loop |}]

let%expect_test "continue outside loop" =
  run_src "f() { continue }";
  [%expect {| TypeError: <test>:1:7: continue outside loop |}]

let%expect_test "unbound variable" =
  run_src "f() { x }";
  [%expect {| TypeError: <test>:1:7: undefined variable 'x' |}]

let%expect_test "type mismatch in let" =
  run_src "f() { let x: bool = 42 }";
  [%expect {| TypeError: <test>:1:21: type mismatch: expected bool, got i32 |}]

let%expect_test "wrong number of arguments" =
  run_src "g() {} f() { g(1) }";
  [%expect {| TypeError: <test>:1:14: expected 0 arguments, got 1 |}]

let%expect_test "null assigned to non-pointer" =
  run_src "f() { let x: i32 = null }";
  [%expect {| TypeError: <test>:1:20: type mismatch: expected i32, got null |}]

let%expect_test "identity function" =
  run_src "id(a: i32): i32 { return a }";
  [%expect {| ok |}]

let%expect_test "null assigned to pointer" =
  run_src "f() { let p: ^i32 = null }";
  [%expect {| ok |}]

let%expect_test "break inside while" =
  run_src "f() { while true { break } }";
  [%expect {| ok |}]

let%expect_test "forward reference" =
  run_src "f() { g() } g() {}";
  [%expect {| ok |}]

let%expect_test "call no args" =
  run_src "g() {} f() { g() }";
  [%expect {| ok |}]

let%expect_test "call with args" =
  run_src "add(x: i32, y: i32) {} f() { add(1, 2) }";
  [%expect {| ok |}]
