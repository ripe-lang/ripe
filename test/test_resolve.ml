(* SPDX-License-Identifier: GPL-2.0-only *)

open Span_utils
open Pipeline

let decl_name_span = function
  | Ripe.Ast.Func fd | Ripe.Ast.Extern fd -> (fd.name, fd.span)
  | Ripe.Ast.Struct sd -> (sd.name, sd.span)
  | Ripe.Ast.Global gd -> (gd.name, gd.span)
  | Ripe.Ast.TypeAlias td | Ripe.Ast.Newtype td -> (td.name, td.span)

let compare_module_symbols src =
  let first_symbol module_id =
    match resolve_src module_id src with
    | decl :: _, uses ->
        let _, span = decl_name_span decl in
        Ripe.Resolve.sym_at uses span
    | [], _ -> failwith "expected a declaration"
  in
  let first = first_symbol 4 in
  let second = first_symbol 9 in
  Printf.printf "%d %d %b" first.module_id second.module_id (first = second)

let dump_decl_visibilities src =
  let decls, uses = resolve_src 0 src in
  List.iter
    (fun decl ->
      let name, span = decl_name_span decl in
      let sym = Ripe.Resolve.sym_at uses span in
      Printf.printf "%s %s\n" name (Ripe.Symbol.show_visibility sym.visibility))
    decls

let%expect_test "resolve: global and function collide" =
  run_src {|
var x: i32 = 1
func x() i32 { return 0 }
|};
  [%expect
    {|
    error: already defined: x
      at <test>:3:1
        func x() i32 { return 0 }
        ^~~~~~~~~~~~~~~~~~~~~~~~~
      at <test>:2:1
        var x: i32 = 1
        ^~~~~~~~~~~~~~ previous definition here
    |}]

let%expect_test "resolve: collision reported in either order" =
  run_src {|
func x() i32 { return 0 }
var x: i32 = 1
|};
  [%expect
    {|
    error: already defined: x
      at <test>:3:1
        var x: i32 = 1
        ^~~~~~~~~~~~~~
      at <test>:2:1
        func x() i32 { return 0 }
        ^~~~~~~~~~~~~~~~~~~~~~~~~ previous definition here
    |}]

let%expect_test "resolve: duplicate function same signature" =
  run_src {|
func f() {}
func f() {}
|};
  [%expect
    {|
    error: already defined: f
      at <test>:3:1
        func f() {}
        ^~~~~~~~~~~
      at <test>:2:1
        func f() {}
        ^~~~~~~~~~~ previous definition here
    |}]

let%expect_test "resolve: duplicate function different signature" =
  run_src {|
func f() i32 { return 0 }
func f(a: i32) i32 { return a }
|};
  [%expect
    {|
    error: already defined: f
      at <test>:3:1
        func f(a: i32) i32 { return a }
        ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      at <test>:2:1
        func f() i32 { return 0 }
        ^~~~~~~~~~~~~~~~~~~~~~~~~ previous definition here
    |}]

let%expect_test "resolve: nested block shadow does not leak" =
  run_src
    {|
func main() i32 {
  var x: i32 = 1
  { var x: i32 = 2 }
  return x
}
|};
  [%expect
    {|
    warning: unused variable: x
      at <test>:4:9
          { var x: i32 = 2 }
                ^
    help: prefix with an underscore: _x
    ok
    |}]

let%expect_test "resolve: same scope redeclare reads old binding" =
  run_src
    {|
func main() i32 {
  var x: i32 = 1
  var x: i32 = x + 4
  return x
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: loop variable is scoped to the loop" =
  run_src
    {|
func main() i32 {
  var i: i32 = 99
  for i in 0..3 { }
  return i
}
|};
  [%expect
    {|
    warning: unused variable: i
      at <test>:4:7
          for i in 0..3 { }
              ^
    help: prefix with an underscore: _i
    ok
    |}]

let%expect_test "resolve: cannot assign to a function name" =
  run_src {|
func g() {}
func main() i32 {
  g = g
  return 0
}
|};
  [%expect
    {|
    error: cannot assign to function: g
      at <test>:4:3
          g = g
          ^
    |}]

let%expect_test "resolve: address of a function lowers" =
  run_codegen
    {|
func g() i32 { return 7 }
func main() i32 {
  let _p = &g
  return 0
}
|};
  [%expect
    {|
    function w $g() {
    @start
        ret 7
    }

    export function w $main() {
    @start
        %_p =l alloc8 8
        %t0 =l copy $g
        storel %t0, %_p
        ret 0
    }
    |}]

let%expect_test "resolve: var shadowing a let global is assignable" =
  run_src
    {|
let C: i32 = 5
func main() i32 {
  var C: i32 = 1
  C = 2
  return C
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: duplicate parameter names" =
  run_src {|
func f(a: i32, a: i32) i32 { return a }
|};
  [%expect
    {|
    error: already defined: a
      at <test>:2:16
        func f(a: i32, a: i32) i32 { return a }
                       ^~~~~~
      at <test>:2:8
        func f(a: i32, a: i32) i32 { return a }
               ^~~~~~ previous definition here
    |}]

let%expect_test "resolve: function called before its definition" =
  run_src {|
func main() i32 { return g() }
func g() i32 { return 7 }
|};
  [%expect {| ok |}]

let%expect_test "resolve: poison variable lets type checking continue" =
  run_src {|func f() i32 { return missing }
func g() i32 { return true }|};
  [%expect
    {|
    error: undefined variable: missing
      at <test>:1:23
        func f() i32 { return missing }
                              ^~~~~~~
    error: type mismatch
      at <test>:2:23
        func g() i32 { return true }
                              ^~~~ expected i32, found bool
    |}]

let%expect_test "resolve: poison type lets type checking continue" =
  run_src {|func f(x: Missing) {}
func g() i32 { return true }|};
  [%expect
    {|
    error: undefined type: Missing
      at <test>:1:11
        func f(x: Missing) {}
                  ^~~~~~~
    error: type mismatch
      at <test>:2:23
        func g() i32 { return true }
                              ^~~~ expected i32, found bool
    |}]

let%expect_test "resolve: shadow inside if body does not leak" =
  run_src
    {|
func main() i32 {
  var x: i32 = 1
  if x > 0 {
    var x: i32 = 2
    x = x + 1
  }
  return x
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: shadow inside while body does not leak" =
  run_src
    {|
func main() i32 {
  var x: i32 = 0
  while x < 3 {
    var y: i32 = x
    x = y + 1
  }
  return x
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: local inside for body does not leak" =
  run_src
    {|
func main() i32 {
  var sum: i32 = 0
  for i in 0..3 {
    var t: i32 = i
    sum = sum + t
  }
  return sum
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: loop variable is not visible after the loop" =
  run_src
    {|
func main() i32 {
  var s: i32 = 0
  for i in 0..3 { s = s + i }
  return i
}
|};
  [%expect
    {|
    error: undefined variable: i
      at <test>:5:10
          return i
                 ^
    |}]

let%expect_test "resolve: extern and function names coexist" =
  run_src {|
extern func puts(s: cstr) i32
func main() i32 { return 0 }
|};
  [%expect {| ok |}]

let%expect_test "resolve: same local name in two functions" =
  run_src
    {|
func a() i32 {
  var x: i32 = 1
  return x
}
func b() i32 {
  var x: i32 = 2
  return x
}
func main() i32 { return a() + b() }
|};
  [%expect {| ok |}]

let%expect_test "resolve: call to an undefined function" =
  run_src {|
func main() i32 { return nope() }
|};
  [%expect
    {|
    error: undefined function: nope
      at <test>:2:26
        func main() i32 { return nope() }
                                 ^~~~
    |}]

let%expect_test "resolve: global let is visible in a function" =
  run_src {|
let C: i32 = 5
func main() i32 { return C }
|};
  [%expect {| ok |}]

let%expect_test "resolve: nested block reads the enclosing param" =
  run_src
    {|
func f(a: i32) i32 {
  {
    var a: i32 = a + 1
    return a
  }
}
func main() i32 { return f(1) }
|};
  [%expect {| ok |}]

let%expect_test "resolve: symbols from different modules are distinct" =
  compare_module_symbols "func f() {}";
  [%expect {| 4 9 false |}]

let%expect_test "resolve: declarations carry visibility" =
  dump_decl_visibilities
    {|
public func api() {}
func helper() {}
public struct point {}
struct secret {}
|};
  [%expect
    {|
    api Public
    helper Private
    point Public
    secret Private
    |}]
