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
  run_src "func f() { const x: bool = 42 }";
  [%expect
    {|
    <test>:1:28: warning: 'x' declared but never used
    TypeError: <test>:1:28: expected bool but found i32
    |}]

let%expect_test "wrong number of arguments" =
  run_src "func g() {} func f() { g(1) }";
  [%expect {| TypeError: <test>:1:24: expected 0 arguments but got 1 |}]

let%expect_test "null assigned to non-pointer" =
  run_src "func f() { const x: i32 = null }";
  [%expect
    {|
    <test>:1:27: warning: 'x' declared but never used
    TypeError: <test>:1:27: expected i32 but found null
    |}]

let%expect_test "identity function" =
  run_src "func id(a: i32) i32 { return a }";
  [%expect {| ok |}]

let%expect_test "null assigned to pointer" =
  run_src "func f() { const p: *i32 = null }";
  [%expect
    {|
    <test>:1:28: warning: 'p' declared but never used
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

let%expect_test "fn ptr assign and call" =
  run_src
    "func add(a: i32, b: i32) i32 { return a + b } func f() { var op: (i32, \
     i32) i32 = add; op(1, 2) }";
  [%expect {| ok |}]

let%expect_test "fn ptr inferred from function name" =
  run_src
    "func add(a: i32, b: i32) i32 { return a + b } func f() { var op = add; \
     op(1, 2) }";
  [%expect {| ok |}]

let%expect_test "fn ptr signature mismatch" =
  run_src
    "func add(a: i32, b: i32) i32 { return a + b } func f() { var op: (i32) \
     i32 = add }";
  [%expect
    {|
    <test>:1:78: warning: 'op' declared but never used
    TypeError: <test>:1:78: expected (i32) i32 but found (i32, i32) i32
    |}]

let%expect_test "non-callable variable" =
  run_src "func f() { var x: i32 = 5; x(1) }";
  [%expect {| TypeError: <test>:1:28: 'x' is not callable |}]

let%expect_test "fn ptr as parameter" =
  run_src
    "func add(a: i32, b: i32) i32 { return a + b } func apply(f: (i32, i32) \
     i32, a: i32, b: i32) i32 { return f(a, b) } func g() { apply(add, 1, 2) }";
  [%expect {| ok |}]

let%expect_test "fn ptr wrong arity at call" =
  run_src
    "func add(a: i32, b: i32) i32 { return a + b } func f() { var op: (i32, \
     i32) i32 = add; op(1) }";
  [%expect {| TypeError: <test>:1:88: expected 2 arguments but got 1 |}]

let%expect_test "fn ptr forward reference" =
  run_src
    "func f() { var op: (i32, i32) i32 = add; op(1, 2) } func add(a: i32, b: \
     i32) i32 { return a + b }";
  [%expect {| ok |}]

let%expect_test "fn ptr returning fn ptr" =
  run_src
    "func add(a: i32, b: i32) i32 { return a + b } func get_op() (i32, i32) \
     i32 { return add } func f() { var op = get_op(); op(1, 2) }";
  [%expect {| ok |}]

let%expect_test "void fn ptr zero args" =
  run_src "func noop() {} func f() { var p: () = noop; p() }";
  [%expect {| ok |}]

let%expect_test "global const read from function" =
  run_src "const X: i32 = 42 func f() i32 { return X }";
  [%expect {| ok |}]

let%expect_test "global var read and write" =
  run_src "var n: i32 = 0 func f() i32 { n = n + 1; return n }";
  [%expect {| ok |}]

let%expect_test "global var zero init" =
  run_src "var flag: bool func f() bool { return flag }";
  [%expect {| ok |}]

let%expect_test "global forward reference" =
  run_src "func f() i32 { return X } const X: i32 = 7";
  [%expect {| ok |}]

let%expect_test "assign to const global" =
  run_src "const X: i32 = 1 func f() { X = 2 }";
  [%expect {| TypeError: <test>:1:29: cannot assign to const 'X' |}]

let%expect_test "non-const global initializer" =
  run_src "func g() i32 { return 1 } const X: i32 = g()";
  [%expect
    {| TypeError: <test>:1:42: initializer for 'X' must be a constant expression |}]

let%expect_test "const requires initializer" =
  run_src "const X: i32";
  [%expect
    {| TypeError: <test>:1:1: 'X' is const and must have an initializer |}]
