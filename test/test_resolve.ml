(* SPDX-License-Identifier: Apache-2.0 *)

open Spanutils
open Pipeline

let decl_name_span = function
  | Ripe.Ast.Func fd | Ripe.Ast.Extern fd -> (fd.func_name, fd.func_span)
  | Ripe.Ast.Struct sd -> (sd.struct_name, sd.struct_span)
  | Ripe.Ast.Global gd -> (gd.name, gd.span)
  | Ripe.Ast.TypeAlias td -> (td.alias_name, td.alias_span)
  | Ripe.Ast.Enum ed -> (ed.enum_name, ed.enum_span)

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
      Printf.printf "%s %s\n" (Ripe.Ast.ident_text name)
        (Ripe.Symbol.show_visibility sym.visibility))
    decls

let%expect_test "resolve: global and function collide" =
  run_src {|
var x: i32 = 1
func x() i32 { return 0 }
|};
  [%expect
    {|
    error: already defined
      at <test>:3:6
        func x() i32 { return 0 }
             ^
      at <test>:2:5
        var x: i32 = 1
            ^ previous definition here
    |}]

let%expect_test "resolve: collision reported in either order" =
  run_src {|
func x() i32 { return 0 }
var x: i32 = 1
|};
  [%expect
    {|
    error: already defined
      at <test>:3:5
        var x: i32 = 1
            ^
      at <test>:2:6
        func x() i32 { return 0 }
             ^ previous definition here
    |}]

let%expect_test "resolve: duplicate function same signature" =
  run_src {|
func f() {}
func f() {}
|};
  [%expect
    {|
    error: already defined
      at <test>:3:6
        func f() {}
             ^
      at <test>:2:6
        func f() {}
             ^ previous definition here
    |}]

let%expect_test "resolve: duplicate function different signature" =
  run_src {|
func f() i32 { return 0 }
func f(a: i32) i32 { return a }
|};
  [%expect
    {|
    error: already defined
      at <test>:3:6
        func f(a: i32) i32 { return a }
             ^
      at <test>:2:6
        func f() i32 { return 0 }
             ^ previous definition here
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
    error: cannot assign to function
      at <test>:4:3
          g = g
          ^
    |}]

let%expect_test "resolve: address of a function lowers" =
  run_codegen
    {|
func g() i32 { return 7 }
func main() i32 {
  var _p = &g
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
    %_p =l copy $g
    ret 0
    }
    |}]

let%expect_test "resolve: var shadowing a global is assignable" =
  run_src
    {|
var C: i32 = 5
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
    error: already defined
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
    error: undefined variable
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
    error: undefined type
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
    error: undefined variable
      at <test>:5:10
          return i
                 ^
    |}]

let%expect_test "resolve: extern and function names coexist" =
  run_src {|
extern "C" func puts(s: cstr) i32
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
    error: undefined function
      at <test>:2:26
        func main() i32 { return nope() }
                                 ^~~~
    |}]

let%expect_test "resolve: global is visible in a function" =
  run_src {|
var C: i32 = 5
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
pub func api() {}
func helper() {}
pub struct point {}
struct secret {}
pub var LIMIT: i32 = 1
var count: i32 = 0
pub type meters = i32
|};
  [%expect
    {|
    api Public
    helper Private
    point Public
    secret Private
    LIMIT Public
    count Private
    meters Public
    |}]

let%expect_test "resolve: a call reaches into an imported module" =
  run_resolve_program
    [
      ("main.rp", {|
import math
func main() { math.add(1) }
|});
      ("math.rp", {|
pub func add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: an unknown member of an import is reported" =
  run_resolve_program
    [
      ("main.rp", {|
import math
func main() { math.nope(1) }
|});
      ("math.rp", {|
pub func add(x: i32) {}
|});
    ];
  [%expect
    {|
    error: undefined function
      at <test>:3:15
        func main() { math.nope(1) }
                      ^~~~~~~~~
    |}]

let%expect_test "resolve: a local shadows an import of the same name" =
  run_resolve_program
    [
      ("main.rp", {|
import math
func main() { var math = 1; math.nope(1) }
|});
      ("math.rp", {|
pub func add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: an import and a function cannot share a name" =
  run_resolve_program
    [
      ("main.rp", {|
import math
func math() {}
func main() { math.add(1) }
|});
      ("math.rp", {|
pub func add(x: i32) {}
|});
    ];
  [%expect
    {|
    error: already defined
      at <test>:3:6
        func math() {}
             ^~~~
      at <test>:2:1
        import math
        ^~~~~~~~~~~ previous definition here
    |}]

(* A struct and a func already share a name here so an import does too *)
let%expect_test "resolve: an import and a struct can share a name" =
  run_resolve_program
    [
      ( "main.rp",
        {|
import math
struct math { x: i32 }
func main() { math.add(1) }
|} );
      ("math.rp", {|
pub func add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a nested import binds its final name" =
  run_resolve_program
    [
      ("main.rp", {|
import math.vector
func main() { vector.add(1) }
|});
      ("math/vector.rp", {|
pub func add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: imports with the same final name collide" =
  run_resolve_program
    [
      ("main.rp", {|
import math.vector
import geometry.vector
func main() {}
|});
      ("math/vector.rp", {|
pub func add(x: i32) {}
|});
      ("geometry/vector.rp", {|
pub func scale(x: i32) {}
|});
    ];
  [%expect
    {|
    error: already defined
      at <test>:3:1
        import geometry.vector
        ^~~~~~~~~~~~~~~~~~~~~~
      at <test>:2:1
        import math.vector
        ^~~~~~~~~~~~~~~~~~ previous definition here
    |}]

let%expect_test "resolve: a type annotation reaches into an imported module" =
  run_resolve_program
    [
      ("main.rp", {|
import math
func main() { var d: math.meters = 0 }
|});
      ("math.rp", {|
pub type meters = i32
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a private type in another module is reported" =
  run_resolve_program
    [
      ("main.rp", {|
import math
func main() { var d: math.meters = 0 }
|});
      ("math.rp", {|
type meters = i32
|});
    ];
  [%expect
    {|
    error: private declaration
      at <test>:3:22
        func main() { var d: math.meters = 0 }
                             ^~~~~~~~~~~
      at <test>:2:1
        type meters = i32
        ^~~~~~~~~~~~~~~~~ declared private here
    |}]

let%expect_test "resolve: main outside the root module is mangled" =
  let resolved, _ =
    load_program
      [
        ("main.rp", {|
import math
func main() i32 { return 0 }
|});
        ("math.rp", {|
pub func main() {}
|});
      ]
  in
  let show (decl : Ripe.Ast.decl) =
    match decl with
    | Ripe.Ast.Func fd ->
        let sym = Ripe.Resolve.sym_at resolved.Ripe.Resolve.uses fd.func_span in
        Printf.printf "%s -> %s\n" sym.Ripe.Symbol.name
          sym.Ripe.Symbol.link_name
    | _ -> ()
  in
  List.iter show resolved.Ripe.Resolve.decls;
  [%expect {|
    main -> main
    main -> _R4math4main
    |}]

let%expect_test "resolve: only a public ABI keeps the name C spells" =
  let resolved, _ =
    load_program
      [
        ("main.rp", {|
import ffi
func main() i32 { return 0 }
|});
        ( "ffi.rp",
          {|
extern "C" func puts(s: cstr) i32
pub extern "C" func exported(x: i32) i32 { return x }
pub extern "Ripe" func unmangled(x: i32) i32 { return x }
extern "C" func callback(x: i32) i32 { return x }
pub func plain(x: i32) i32 { return x }
|}
        );
      ]
  in
  let show (decl : Ripe.Ast.decl) =
    match decl with
    | Ripe.Ast.Func fd | Ripe.Ast.Extern fd ->
        let sym = Ripe.Resolve.sym_at resolved.Ripe.Resolve.uses fd.func_span in
        Printf.printf "%s -> %s\n" sym.Ripe.Symbol.name
          sym.Ripe.Symbol.link_name
    | _ -> ()
  in
  List.iter show resolved.Ripe.Resolve.decls;
  [%expect
    {|
    main -> main
    puts -> puts
    exported -> exported
    unmangled -> unmangled
    callback -> _R3ffi8callback
    plain -> _R3ffi5plain
    |}]

let%expect_test "resolve: a public import is callable from another module" =
  run_resolve_program
    [
      ("main.rp", {|
import ffi
func main() i32 { return ffi.puts("hi") }
|});
      ("ffi.rp", {|
pub extern "C" func puts(s: cstr) i32
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a private import stays in its module" =
  run_resolve_program
    [
      ("main.rp", {|
import ffi
func main() i32 { return ffi.puts("hi") }
|});
      ("ffi.rp", {|
extern "C" func puts(s: cstr) i32
|});
    ];
  [%expect
    {|
    error: private declaration
      at <test>:3:26
        func main() i32 { return ffi.puts("hi") }
                                 ^~~~~~~~
      at <test>:2:12
        extern "C" func puts(s: cstr) i32
                   ^~~~~~~~~~~~~~~~~~~~~~ declared private here
    |}]

let%expect_test "resolve: a local function may call a later sibling" =
  run_src
    {|func f() i32 {
  func first(x: i32) i32 { second(x) }
  func second(x: i32) i32 { x + 1 }
  first(4)
}|};
  [%expect {| ok |}]

let%expect_test "resolve: a local function cannot capture a variable" =
  run_src {|func f() i32 {
  var x = 4
  func read() i32 { x }
  read()
}|};
  [%expect
    {|
    error: local function cannot capture variable
      at <test>:3:21
          func read() i32 { x }
                            ^
    |}]

let%expect_test "resolve: a local declaration stays in its block" =
  run_src {|func f() {
  { type Coord = i32 }
  var x: Coord = 1
}|};
  [%expect
    {|
    error: undefined type
      at <test>:3:10
          var x: Coord = 1
                 ^~~~~
    |}]

let%expect_test "resolve: a captured variable shadows a module function" =
  run_src
    {|func x() i32 { 7 }
func outer() i32 {
  var x = 1
  func inner() i32 { x() }
  inner()
}|};
  [%expect
    {|
    error: local function cannot capture variable
      at <test>:4:22
          func inner() i32 { x() }
                             ^
    |}]

let%expect_test "resolve: an import resolves through a search root" =
  run_resolve_program ~search_roots:[ "/libs" ]
    [
      ("main.rp", {|
import std.io
func main() { io.write() }
|});
      ("/libs/std/io.rp", {|
module io
pub func write() {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a relative module shadows a search root" =
  run_resolve_program ~search_roots:[ "/libs" ]
    [
      ("main.rp", {|
import std.io
func main() { io.here() }
|});
      ("std/io.rp", {|
module io
pub func here() {}
|});
      ("/libs/std/io.rp", {|
module io
pub func write() {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a missing import lists every root tried" =
  run_resolve_program ~search_roots:[ "/libs"; "/other" ]
    [ ("main.rp", {|
import std.io
func main() {}
|}) ];
  [%expect
    {|
    error: module not found
      at <test>:2:1
        import std.io
        ^~~~~~~~~~~~~
      tried std/io.rp
            /libs/std/io.rp
            /other/std/io.rp
    |}]

let%expect_test "resolve: a span with no symbol comes back empty" =
  let src = {|func target() i32 { return 1 }|} in
  let decls, uses = resolve_src 0 src in
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

let%expect_test "resolve: a symbol carries the qualified name it resolves to" =
  let src = {|func target() i32 { return 1 }|} in
  let decls, uses = resolve_src 3 src in
  let _, recorded = decl_name_span (List.hd decls) in
  let sym = Ripe.Resolve.sym_at uses recorded in
  let qname = Ripe.Resolve.qname_of uses sym in
  Printf.printf "%s key matches %b\n" (Ripe.Qname.show qname)
    (Ripe.Qname.key qname = Ripe.Symbol.key sym);
  [%expect {| target key matches true |}]

let%expect_test "resolve: a function declared in a body is lifted out" =
  let src =
    {|func outer() i32 {
  func inner() i32 { return 1 }
  return inner()
}|}
  in
  let decls, uses = resolve_src 0 src in
  let name (decl : Ripe.Ast.decl) =
    let n, _ = decl_name_span decl in
    Ripe.Ast.ident_text n
  in
  Printf.printf "top %s\n" (String.concat " " (List.map name decls));
  Printf.printf "lifted %s\n"
    (String.concat " " (List.map name (Ripe.Resolve.local_decls uses)));
  [%expect {|
    top outer
    lifted inner
    |}]

let%expect_test "resolve: nothing is lifted when no body declares one" =
  let _, uses = resolve_src 0 {|func target() i32 { return 1 }|} in
  Printf.printf "%d\n" (List.length (Ripe.Resolve.local_decls uses));
  [%expect {| 0 |}]

let%expect_test "resolve: a span reports the module path it sits in" =
  let src = {|func target() i32 { return 1 }|} in
  let _, uses = resolve_src 0 src in
  let path = Ripe.Resolve.module_path_at uses (span src "target") in
  Printf.printf "%S\n" (String.concat "." path);
  [%expect {| "" |}]

let%expect_test "resolve: the builtin types are all in scope" =
  let _, uses = resolve_src 0 {|func f() i32 { return 1 }|} in
  let builtins = Ripe.Resolve.builtins uses in
  let name (_, builtin) =
    match builtin with
    | Ripe.Types.BTy t -> Ripe.Types.show_ty t
    | Ripe.Types.BOpaque -> "opaque"
  in
  print_endline (String.concat " " (List.map name builtins));
  Printf.printf "%d\n" (List.length builtins);
  [%expect
    {|
    i8 i16 i32 i64 u8 u16 u32 u64 isize usize f32 f64 bool char cstr str never opaque
    18
    |}]

let%expect_test "resolve: the dump lists what each name resolved to" =
  let _, uses = resolve_src 0 {|func f(a: i32) i32 { return a }|} in
  print_string (Ripe.Resolve.dump uses);
  [%expect
    {|
    Resolver output

    Each line maps a source byte range to its definition.
    * (start,end): source byte range
    * #module.id: declaration ID
    * #-module.id: built in declaration
    * kind name: resolved definition

    (0,31) -> #0.0 Func f
    (7,13) -> #0.1 Param a
    (10,13) -> #-2.2 Type i32
    (15,18) -> #-2.2 Type i32
    (28,29) -> #0.1 Param a
    |}]
