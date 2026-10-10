(* SPDX-License-Identifier: Apache-2.0 *)

open Spanutils
open Pipeline

let decl_name_span = function
  | Ripe.Ast.Func fd | Ripe.Ast.Extern fd -> (fd.func_name, fd.func_span)
  | Ripe.Ast.Struct sd -> (sd.struct_name, sd.struct_span)
  | Ripe.Ast.Global gd -> (gd.name, gd.span)
  | Ripe.Ast.TypeAlias td -> (td.alias_name, td.alias_span)
  | Ripe.Ast.Enum ed -> (ed.enum_name, ed.enum_span)

let%expect_test "resolve: global and function collide" =
  run_src {|
x : i32 = 1;
x :: fn() -> i32 { return 0 };
|};
  [%expect
    {|
    error: already defined
      at <test>:3:1
        x :: fn() -> i32 { return 0 };
        ^
      at <test>:2:1
        x : i32 = 1;
        ^ previous definition here
    |}]

let%expect_test "resolve: collision reported in either order" =
  run_src {|
x :: fn() -> i32 { return 0 };
x : i32 = 1;
|};
  [%expect
    {|
    error: already defined
      at <test>:3:1
        x : i32 = 1;
        ^
      at <test>:2:1
        x :: fn() -> i32 { return 0 };
        ^ previous definition here
    |}]

let%expect_test "resolve: duplicate function same signature" =
  run_src {|
f :: fn() {};
f :: fn() {};
|};
  [%expect
    {|
    error: already defined
      at <test>:3:1
        f :: fn() {};
        ^
      at <test>:2:1
        f :: fn() {};
        ^ previous definition here
    |}]

let%expect_test "resolve: duplicate function different signature" =
  run_src
    {|
f :: fn() -> i32 { return 0 };
f :: fn(a: i32) -> i32 { return a };
|};
  [%expect
    {|
    error: already defined
      at <test>:3:1
        f :: fn(a: i32) -> i32 { return a };
        ^
      at <test>:2:1
        f :: fn() -> i32 { return 0 };
        ^ previous definition here
    |}]

let%expect_test "resolve: nested block shadow does not leak" =
  run_src
    {|
main :: fn() -> i32 {
  x : i32 = 1;
  { x : i32 = 2 }
  return x;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: same scope redeclare reads old binding" =
  run_src
    {|
main :: fn() -> i32 {
  x : i32 = 1;
  x : i32 = x + 4;
  return x;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: loop variable is scoped to the loop" =
  run_src
    {|
main :: fn() -> i32 {
  i : i32 = 99;
  for i in 0..3 { }
  return i;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: cannot assign to a function name" =
  run_src {|
g :: fn() {};
main :: fn() -> i32 {
  g = g;
  return 0;
};
|};
  [%expect
    {|
    error: cannot assign to function
      at <test>:4:3
          g = g;
          ^
    |}]

let%expect_test "resolve: address of a function lowers" =
  run_codegen
    {|
g :: fn() -> i32 { return 7 };
main :: fn() -> i32 {
  _p := &g;
  return 0;
};
|};
  [%expect
    {|
    function w $_R1g() {
    @start
    ret 7
    }

    function w $_R4main() {
    @start
    %m1 =l alloc8 8
    storel $_R1g, %m1
    %_p =l copy %m1
    ret 0
    }

    export function w $main() {
    @start
    %t0 =w call $_R4main()
    ret %t0
    }
    |}]

let%expect_test "resolve: a binding shadowing a global is assignable" =
  run_src
    {|
C : i32 = 5;
main :: fn() -> i32 {
  C : i32 = 1;
  C = 2;
  return C;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: duplicate parameter names" =
  run_src {|
f :: fn(a: i32, a: i32) -> i32 { return a };
|};
  [%expect
    {|
    error: already defined
      at <test>:2:17
        f :: fn(a: i32, a: i32) -> i32 { return a };
                        ^~~~~~
      at <test>:2:9
        f :: fn(a: i32, a: i32) -> i32 { return a };
                ^~~~~~ previous definition here
    |}]

let%expect_test "resolve: function called before its definition" =
  run_src
    {|
main :: fn() -> i32 { return g() };
g :: fn() -> i32 { return 7 };
|};
  [%expect {| ok |}]

let%expect_test "resolve: poison variable lets type checking continue" =
  run_src
    {|f :: fn() -> i32 { return missing };
g :: fn() -> i32 { return true };|};
  [%expect
    {|
    error: undefined variable
      at <test>:1:27
        f :: fn() -> i32 { return missing };
                                  ^~~~~~~
    error: type mismatch
      at <test>:2:27
        g :: fn() -> i32 { return true };
                                  ^~~~ expected i32, found bool
    |}]

let%expect_test "resolve: poison type lets type checking continue" =
  run_src {|f :: fn(x: Missing) {};
g :: fn() -> i32 { return true };|};
  [%expect
    {|
    error: undefined type
      at <test>:1:12
        f :: fn(x: Missing) {};
                   ^~~~~~~
    error: type mismatch
      at <test>:2:27
        g :: fn() -> i32 { return true };
                                  ^~~~ expected i32, found bool
    |}]

let%expect_test "resolve: shadow inside if body does not leak" =
  run_src
    {|
main :: fn() -> i32 {
  x : i32 = 1;
  if x > 0 {
    x : i32 = 2;
    x = x + 1;
  }
  return x;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: shadow inside while body does not leak" =
  run_src
    {|
main :: fn() -> i32 {
  x : i32 = 0;
  while x < 3 {
    y : i32 = x;
    x = y + 1;
  }
  return x;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: local inside for body does not leak" =
  run_src
    {|
main :: fn() -> i32 {
  sum : i32 = 0;
  for i in 0..3 {
    t : i32 = i;
    sum = sum + t;
  }
  return sum;
};
|};
  [%expect {| ok |}]

let%expect_test "resolve: loop variable is not visible after the loop" =
  run_src
    {|
main :: fn() -> i32 {
  s : i32 = 0;
  for i in 0..3 { s = s + i }
  return i;
};
|};
  [%expect
    {|
    error: undefined variable
      at <test>:5:10
          return i;
                 ^
    |}]

let%expect_test "resolve: extern and function names coexist" =
  run_src
    {|
extern "C" fn puts(s: cstr) i32;
main :: fn() -> i32 { return 0 };
|};
  [%expect {| ok |}]

let%expect_test "resolve: same local name in two functions" =
  run_src
    {|
a :: fn() -> i32 {
  x : i32 = 1;
  return x;
};
b :: fn() -> i32 {
  x : i32 = 2;
  return x;
};
main :: fn() -> i32 { return a() + b() };
|};
  [%expect {| ok |}]

let%expect_test "resolve: call to an undefined function" =
  run_src {|
main :: fn() -> i32 { return nope() };
|};
  [%expect
    {|
    error: undefined function
      at <test>:2:30
        main :: fn() -> i32 { return nope() };
                                     ^~~~
    |}]

let%expect_test "resolve: global is visible in a function" =
  run_src {|
C : i32 = 5;
main :: fn() -> i32 { return C };
|};
  [%expect {| ok |}]

let%expect_test "resolve: nested block reads the enclosing param" =
  run_src
    {|
f :: fn(a: i32) -> i32 {
  {
    a : i32 = a + 1;
    return a;
  }
};
main :: fn() -> i32 { return f(1) };
|};
  [%expect {| ok |}]

let%expect_test "resolve: only an extern ABI keeps the name C spells" =
  let decls, uses =
    resolve_src
      {|
main :: fn() -> i32 { return 0 };
extern "C" fn puts(s: cstr) i32;
extern "C" fn exported(x: i32) i32 { return x }
extern "Ripe" fn unmangled(x: i32) i32 { return x }
plain :: fn(x: i32) -> i32 { return x };
binary_search :: fn() -> i32 { return 0 };
|}
  in
  let show (decl : Ripe.Ast.decl) =
    match decl with
    | Ripe.Ast.Func fd | Ripe.Ast.Extern fd ->
        let sym = Ripe.Resolve.sym_at uses fd.func_span in
        Printf.printf "%s -> %s\n" sym.Ripe.Symbol.name
          sym.Ripe.Symbol.link_name
    | _ -> ()
  in
  List.iter show decls;
  [%expect
    {|
    main -> _R4main
    puts -> puts
    exported -> exported
    unmangled -> unmangled
    plain -> _R5plain
    binary_search -> _R13binary_search
    |}]

let%expect_test "resolve: a local function may call a later sibling" =
  run_src
    {|f :: fn() -> i32 {
  first :: fn(x: i32) -> i32 { second(x) };
  second :: fn(x: i32) -> i32 { x + 1 };
  first(4)
};|};
  [%expect {| ok |}]

let%expect_test "resolve: a local function cannot capture a variable" =
  run_src
    {|f :: fn() -> i32 {
  x := 4;
  read :: fn() -> i32 { x };
  read()
};|};
  [%expect
    {|
    error: local function cannot capture variable
      at <test>:3:25
          read :: fn() -> i32 { x };
                                ^
    |}]

let%expect_test "resolve: a local declaration stays in its block" =
  run_src {|f :: fn() {
  { type Coord = i32 }
  x : Coord = 1;
};|};
  [%expect
    {|
    error: undefined type
      at <test>:3:7
          x : Coord = 1;
              ^~~~~
    |}]

let%expect_test "resolve: a captured variable shadows a module function" =
  run_src
    {|x :: fn() -> i32 { 7 };
outer :: fn() -> i32 {
  x := 1;
  inner :: fn() -> i32 { x() };
  inner()
};|};
  [%expect
    {|
    error: local function cannot capture variable
      at <test>:4:26
          inner :: fn() -> i32 { x() };
                                 ^
    |}]

let%expect_test "resolve: a span with no symbol comes back empty" =
  let src = {|target :: fn() -> i32 { return 1 };|} in
  let decls, uses = resolve_src src in
  let _, recorded = decl_name_span (List.hd decls) in
  let show what sp =
    Printf.printf "%s %s\n" what
      (match Ripe.Resolve.sym_at_opt uses sp with
      | Some s -> s.Ripe.Symbol.name
      | None -> "none")
  in
  show "the declaration" recorded;
  show "just the name" (span src "target");
  show "a literal" (span src "1");
  show "nothing" Ripe.Span.dummy;
  [%expect
    {|
    the declaration target
    just the name none
    a literal none
    nothing none
    |}]

let%expect_test "resolve: a symbol keeps its key and source name" =
  let src = {|target :: fn() -> i32 { return 1 };|} in
  let decls, uses = resolve_src src in
  let _, recorded = decl_name_span (List.hd decls) in
  let sym = Ripe.Resolve.sym_at uses recorded in
  let keyname = Ripe.Resolve.keyname_of sym in
  Printf.printf "%s key matches %b\n"
    (Ripe.Keyname.show keyname)
    (Ripe.Keyname.key keyname = Ripe.Symbol.key sym);
  [%expect {| target key matches true |}]

let%expect_test "resolve: a function declared in a body is lifted out" =
  let src =
    {|outer :: fn() -> i32 {
  inner :: fn() -> i32 { return 1 };
  return inner();
};|}
  in
  let decls, uses = resolve_src src in
  let name (decl : Ripe.Ast.decl) =
    let n, _ = decl_name_span decl in
    Ripe.Interner.text n.value
  in
  Printf.printf "top %s\n" (String.concat " " (List.map name decls));
  Printf.printf "lifted %s\n"
    (String.concat " " (List.map name (Ripe.Resolve.local_decls uses)));
  [%expect {|
    top outer
    lifted inner
    |}]

let%expect_test "resolve: nothing is lifted when no body declares one" =
  let _, uses = resolve_src {|target :: fn() -> i32 { return 1 };|} in
  Printf.printf "%d\n" (List.length (Ripe.Resolve.local_decls uses));
  [%expect {| 0 |}]

let%expect_test "resolve: the builtin types are all in scope" =
  let _, uses = resolve_src {|f :: fn() -> i32 { return 1 };|} in
  let builtins = Ripe.Resolve.builtins uses in
  let name (_, t) = Ripe.Types.show_ty t in
  print_endline (String.concat " " (List.map name builtins));
  Printf.printf "%d\n" (List.length builtins);
  [%expect
    {|
    i8 i16 i32 i64 u8 u16 u32 u64 isize usize f32 f64 bool char cstr str never ptr
    18
    |}]

let%expect_test "resolve: the dump lists what each name resolved to" =
  let _, uses = resolve_src {|f :: fn(a: i32) -> i32 { return a };|} in
  print_string (Ripe.Resolve.dump uses);
  [%expect
    {|
    Resolver output

    Each line maps a source byte range to its definition.
    * (start,end): source byte range
    * #id: declaration ID
    * #-id: built in declaration
    * kind name: resolved definition

    (0,35) -> #0 Func f
    (8,14) -> #1 Param a
    (11,14) -> #-4 Type i32
    (19,22) -> #-4 Type i32
    (32,33) -> #1 Param a
    |}]
