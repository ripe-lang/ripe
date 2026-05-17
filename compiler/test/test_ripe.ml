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
  run_src "func f() { break }";
  [%expect {| TypeError: <test>:1:12: break outside loop |}]

let%expect_test "continue outside loop" =
  run_src "func f() { continue }";
  [%expect {| TypeError: <test>:1:12: continue outside loop |}]

let%expect_test "unbound variable" =
  run_src "func f() { x }";
  [%expect {| TypeError: <test>:1:12: undefined variable 'x' |}]

let%expect_test "type mismatch in let" =
  run_src "func f() { let x: bool = 42 }";
  [%expect
    {|
    <test>:1:26: warning: 'x' declared but never used
    TypeError: <test>:1:26: expected bool but found i32
    |}]

let%expect_test "wrong number of arguments" =
  run_src "func g() {} func f() { g(1) }";
  [%expect {| TypeError: <test>:1:24: expected 0 arguments but got 1 |}]

let%expect_test "null assigned to non-pointer" =
  run_src "func f() { let x: i32 = null }";
  [%expect
    {|
    <test>:1:25: warning: 'x' declared but never used
    TypeError: <test>:1:25: expected i32 but found null
    |}]

let%expect_test "identity function" =
  run_src "func id(a: i32) i32 { return a }";
  [%expect {| ok |}]

let%expect_test "null assigned to pointer" =
  run_src "func f() { let p: *i32 = null }";
  [%expect
    {|
    <test>:1:26: warning: 'p' declared but never used
    ok
    |}]

let%expect_test "break inside while" =
  run_src "func f() { while true { break } }";
  [%expect {| ok |}]

let%expect_test "forward reference" =
  run_src "func f() { g() } func g() {}";
  [%expect {| ok |}]

let%expect_test "call no args" =
  run_src "func g() {} func f() { g() }";
  [%expect {| ok |}]

let%expect_test "call with args" =
  run_src "func add(x: i32, y: i32) {} func f() { add(1, 2) }";
  [%expect {| ok |}]
