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
var x: i32 = 1;
fn x() i32 { return 0 }
|};
  [%expect
    {|
    error: already defined
      at <test>:3:4
        fn x() i32 { return 0 }
           ^
      at <test>:2:5
        var x: i32 = 1;
            ^ previous definition here
    |}]

let%expect_test "resolve: collision reported in either order" =
  run_src {|
fn x() i32 { return 0 }
var x: i32 = 1;
|};
  [%expect
    {|
    error: already defined
      at <test>:3:5
        var x: i32 = 1;
            ^
      at <test>:2:4
        fn x() i32 { return 0 }
           ^ previous definition here
    |}]

let%expect_test "resolve: duplicate function same signature" =
  run_src {|
fn f() {}
fn f() {}
|};
  [%expect
    {|
    error: already defined
      at <test>:3:4
        fn f() {}
           ^
      at <test>:2:4
        fn f() {}
           ^ previous definition here
    |}]

let%expect_test "resolve: duplicate function different signature" =
  run_src {|
fn f() i32 { return 0 }
fn f(a: i32) i32 { return a }
|};
  [%expect
    {|
    error: already defined
      at <test>:3:4
        fn f(a: i32) i32 { return a }
           ^
      at <test>:2:4
        fn f() i32 { return 0 }
           ^ previous definition here
    |}]

let%expect_test "resolve: nested block shadow does not leak" =
  run_src
    {|
fn main() i32 {
  var x: i32 = 1;
  { var x: i32 = 2 }
  return x;
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
fn main() i32 {
  var x: i32 = 1;
  var x: i32 = x + 4;
  return x;
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: loop variable is scoped to the loop" =
  run_src
    {|
fn main() i32 {
  var i: i32 = 99;
  for i in 0..3 { }
  return i;
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
fn g() {}
fn main() i32 {
  g = g;
  return 0;
}
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
fn g() i32 { return 7 }
fn main() i32 {
  var _p = &g;
  return 0;
}
|};
  [%expect
    {|
    function w $g() {
    @start
    ret 7
    }

    function w $_R4main() {
    @start
    %_p =l copy $g
    ret 0
    }

    export function w $main() {
    @start
    %t0 =w call $_R4main()
    ret %t0
    }
    |}]

let%expect_test "resolve: var shadowing a global is assignable" =
  run_src
    {|
var C: i32 = 5;
fn main() i32 {
  var C: i32 = 1;
  C = 2;
  return C;
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: duplicate parameter names" =
  run_src {|
fn f(a: i32, a: i32) i32 { return a }
|};
  [%expect
    {|
    error: already defined
      at <test>:2:14
        fn f(a: i32, a: i32) i32 { return a }
                     ^~~~~~
      at <test>:2:6
        fn f(a: i32, a: i32) i32 { return a }
             ^~~~~~ previous definition here
    |}]

let%expect_test "resolve: function called before its definition" =
  run_src {|
fn main() i32 { return g() }
fn g() i32 { return 7 }
|};
  [%expect {| ok |}]

let%expect_test "resolve: poison variable lets type checking continue" =
  run_src {|fn f() i32 { return missing }
fn g() i32 { return true }|};
  [%expect
    {|
    error: undefined variable
      at <test>:1:21
        fn f() i32 { return missing }
                            ^~~~~~~
    error: type mismatch
      at <test>:2:21
        fn g() i32 { return true }
                            ^~~~ expected i32, found bool
    |}]

let%expect_test "resolve: poison type lets type checking continue" =
  run_src {|fn f(x: Missing) {}
fn g() i32 { return true }|};
  [%expect
    {|
    error: undefined type
      at <test>:1:9
        fn f(x: Missing) {}
                ^~~~~~~
    error: type mismatch
      at <test>:2:21
        fn g() i32 { return true }
                            ^~~~ expected i32, found bool
    |}]

let%expect_test "resolve: shadow inside if body does not leak" =
  run_src
    {|
fn main() i32 {
  var x: i32 = 1;
  if x > 0 {
    var x: i32 = 2;
    x = x + 1;
  }
  return x;
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: shadow inside while body does not leak" =
  run_src
    {|
fn main() i32 {
  var x: i32 = 0;
  while x < 3 {
    var y: i32 = x;
    x = y + 1;
  }
  return x;
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: local inside for body does not leak" =
  run_src
    {|
fn main() i32 {
  var sum: i32 = 0;
  for i in 0..3 {
    var t: i32 = i;
    sum = sum + t;
  }
  return sum;
}
|};
  [%expect {| ok |}]

let%expect_test "resolve: loop variable is not visible after the loop" =
  run_src
    {|
fn main() i32 {
  var s: i32 = 0;
  for i in 0..3 { s = s + i }
  return i;
}
|};
  [%expect
    {|
    error: undefined variable
      at <test>:5:10
          return i;
                 ^
    |}]

let%expect_test "resolve: extern and function names coexist" =
  run_src {|
extern "C" fn puts(s: cstr) i32;
fn main() i32 { return 0 }
|};
  [%expect {| ok |}]

let%expect_test "resolve: same local name in two functions" =
  run_src
    {|
fn a() i32 {
  var x: i32 = 1;
  return x;
}
fn b() i32 {
  var x: i32 = 2;
  return x;
}
fn main() i32 { return a() + b() }
|};
  [%expect {| ok |}]

let%expect_test "resolve: call to an undefined function" =
  run_src {|
fn main() i32 { return nope() }
|};
  [%expect
    {|
    error: undefined function
      at <test>:2:24
        fn main() i32 { return nope() }
                               ^~~~
    |}]

let%expect_test "resolve: global is visible in a function" =
  run_src {|
var C: i32 = 5;
fn main() i32 { return C }
|};
  [%expect {| ok |}]

let%expect_test "resolve: nested block reads the enclosing param" =
  run_src
    {|
fn f(a: i32) i32 {
  {
    var a: i32 = a + 1;
    return a;
  }
}
fn main() i32 { return f(1) }
|};
  [%expect {| ok |}]

let%expect_test "resolve: symbols from different modules are distinct" =
  compare_module_symbols "fn f() {}";
  [%expect {| 4 9 false |}]

let%expect_test "resolve: declarations carry visibility" =
  dump_decl_visibilities
    {|
pub fn api() {}
fn helper() {}
pub struct point {}
struct secret {}
pub var LIMIT: i32 = 1;
var count: i32 = 0;
pub type meters = i32;
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
import math;
fn main() { math.add(1) }
|});
      ("math.rp", {|
pub fn add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: an unknown member of an import is reported" =
  run_resolve_program
    [
      ("main.rp", {|
import math;
fn main() { math.nope(1) }
|});
      ("math.rp", {|
pub fn add(x: i32) {}
|});
    ];
  [%expect
    {|
    error: undefined function
      at <test>:3:13
        fn main() { math.nope(1) }
                    ^~~~~~~~~
    |}]

let%expect_test "resolve: a local shadows an import of the same name" =
  run_resolve_program
    [
      ("main.rp", {|
import math;
fn main() { var math = 1; math.nope(1) }
|});
      ("math.rp", {|
pub fn add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: an import and a function cannot share a name" =
  run_resolve_program
    [
      ("main.rp", {|
import math;
fn math() {}
fn main() { math.add(1) }
|});
      ("math.rp", {|
pub fn add(x: i32) {}
|});
    ];
  [%expect
    {|
    error: already defined
      at <test>:3:4
        fn math() {}
           ^~~~
      at <test>:2:1
        import math;
        ^~~~~~~~~~~ previous definition here
    |}]

(* A struct and a fn already share a name here so an import does too *)
let%expect_test "resolve: an import and a struct can share a name" =
  run_resolve_program
    [
      ( "main.rp",
        {|
import math;
struct math { x: i32 }
fn main() { math.add(1) }
|} );
      ("math.rp", {|
pub fn add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a nested import binds its final name" =
  run_resolve_program
    [
      ("main.rp", {|
import math.vector;
fn main() { vector.add(1) }
|});
      ("math/vector.rp", {|
pub fn add(x: i32) {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: imports with the same final name collide" =
  run_resolve_program
    [
      ("main.rp", {|
import math.vector;
import geometry.vector;
fn main() {}
|});
      ("math/vector.rp", {|
pub fn add(x: i32) {}
|});
      ("geometry/vector.rp", {|
pub fn scale(x: i32) {}
|});
    ];
  [%expect
    {|
    error: already defined
      at <test>:3:1
        import geometry.vector;
        ^~~~~~~~~~~~~~~~~~~~~~
      at <test>:2:1
        import math.vector;
        ^~~~~~~~~~~~~~~~~~ previous definition here
    |}]

let%expect_test "resolve: a type annotation reaches into an imported module" =
  run_resolve_program
    [
      ("main.rp", {|
import math;
fn main() { var d: math.meters = 0 }
|});
      ("math.rp", {|
pub type meters = i32;
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a private type in another module is reported" =
  run_resolve_program
    [
      ("main.rp", {|
import math;
fn main() { var d: math.meters = 0 }
|});
      ("math.rp", {|
type meters = i32;
|});
    ];
  [%expect
    {|
    error: private declaration
      at <test>:3:20
        fn main() { var d: math.meters = 0 }
                           ^~~~~~~~~~~
      at <test>:2:1
        type meters = i32;
        ^~~~~~~~~~~~~~~~~ declared private here
    |}]

let%expect_test "resolve: main outside the root module is mangled" =
  let resolved, _ =
    load_program
      [
        ("main.rp", {|
import math;
fn main() i32 { return 0 }
|});
        ("math.rp", {|
pub fn main() {}
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
    main -> _R4main4main
    main -> _R4math4main
    |}]

let%expect_test "resolve: only a public ABI keeps the name C spells" =
  let resolved, _ =
    load_program
      [
        ("main.rp", {|
import ffi;
fn main() i32 { return 0 }
|});
        ( "ffi.rp",
          {|
extern "C" fn puts(s: cstr) i32;
pub extern "C" fn exported(x: i32) i32 { return x }
pub extern "Ripe" fn unmangled(x: i32) i32 { return x }
extern "C" fn callback(x: i32) i32 { return x }
pub fn plain(x: i32) i32 { return x }
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
    main -> _R4main4main
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
import ffi;
fn main() i32 { return ffi.puts("hi") }
|});
      ("ffi.rp", {|
pub extern "C" fn puts(s: cstr) i32;
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a private import stays in its module" =
  run_resolve_program
    [
      ("main.rp", {|
import ffi;
fn main() i32 { return ffi.puts("hi") }
|});
      ("ffi.rp", {|
extern "C" fn puts(s: cstr) i32;
|});
    ];
  [%expect
    {|
    error: private declaration
      at <test>:3:24
        fn main() i32 { return ffi.puts("hi") }
                               ^~~~~~~~
      at <test>:2:12
        extern "C" fn puts(s: cstr) i32;
                   ^~~~~~~~~~~~~~~~~~~~ declared private here
    |}]

let%expect_test "resolve: a local function may call a later sibling" =
  run_src
    {|fn f() i32 {
  fn first(x: i32) i32 { second(x) }
  fn second(x: i32) i32 { x + 1 }
  first(4)
}|};
  [%expect {| ok |}]

let%expect_test "resolve: a local function cannot capture a variable" =
  run_src {|fn f() i32 {
  var x = 4;
  fn read() i32 { x }
  read()
}|};
  [%expect
    {|
    error: local function cannot capture variable
      at <test>:3:19
          fn read() i32 { x }
                          ^
    |}]

let%expect_test "resolve: a local declaration stays in its block" =
  run_src {|fn f() {
  { type Coord = i32 }
  var x: Coord = 1;
}|};
  [%expect
    {|
    error: undefined type
      at <test>:3:10
          var x: Coord = 1;
                 ^~~~~
    |}]

let%expect_test "resolve: a captured variable shadows a module function" =
  run_src
    {|fn x() i32 { 7 }
fn outer() i32 {
  var x = 1;
  fn inner() i32 { x() }
  inner()
}|};
  [%expect
    {|
    error: local function cannot capture variable
      at <test>:4:20
          fn inner() i32 { x() }
                           ^
    |}]

let%expect_test "resolve: an import resolves through a search root" =
  run_resolve_program ~search_roots:[ "/libs" ]
    [
      ("main.rp", {|
import std.io;
fn main() { io.write() }
|});
      ("/libs/std/io.rp", {|
module io;
pub fn write() {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a relative module shadows a search root" =
  run_resolve_program ~search_roots:[ "/libs" ]
    [
      ("main.rp", {|
import std.io;
fn main() { io.here() }
|});
      ("std/io.rp", {|
module io;
pub fn here() {}
|});
      ("/libs/std/io.rp", {|
module io;
pub fn write() {}
|});
    ];
  [%expect {| ok |}]

let%expect_test "resolve: a missing import lists every root tried" =
  run_resolve_program ~search_roots:[ "/libs"; "/other" ]
    [ ("main.rp", {|
import std.io;
fn main() {}
|}) ];
  [%expect
    {|
    error: module not found
      at <test>:2:1
        import std.io;
        ^~~~~~~~~~~~~
      tried std/io.rp
            /libs/std/io.rp
            /other/std/io.rp
    |}]

let%expect_test "resolve: a span with no symbol comes back empty" =
  let src = {|fn target() i32 { return 1 }|} in
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
  let src = {|fn target() i32 { return 1 }|} in
  let decls, uses = resolve_src 3 src in
  let _, recorded = decl_name_span (List.hd decls) in
  let sym = Ripe.Resolve.sym_at uses recorded in
  let qname = Ripe.Resolve.qname_of uses sym in
  Printf.printf "%s key matches %b\n" (Ripe.Qname.show qname)
    (Ripe.Qname.key qname = Ripe.Symbol.key sym);
  [%expect {| target key matches true |}]

let%expect_test "resolve: a function declared in a body is lifted out" =
  let src =
    {|fn outer() i32 {
  fn inner() i32 { return 1 }
  return inner();
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
  let _, uses = resolve_src 0 {|fn target() i32 { return 1 }|} in
  Printf.printf "%d\n" (List.length (Ripe.Resolve.local_decls uses));
  [%expect {| 0 |}]

let%expect_test "resolve: a span reports the module path it sits in" =
  let src = {|fn target() i32 { return 1 }|} in
  let _, uses = resolve_src 0 src in
  let path = Ripe.Resolve.module_path_at uses (span src "target") in
  Printf.printf "%S\n" (String.concat "." path);
  [%expect {| "" |}]

let%expect_test "resolve: the builtin types are all in scope" =
  let _, uses = resolve_src 0 {|fn f() i32 { return 1 }|} in
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
  let _, uses = resolve_src 0 {|fn f(a: i32) i32 { return a }|} in
  print_string (Ripe.Resolve.dump uses);
  [%expect
    {|
    Resolver output

    Each line maps a source byte range to its definition.
    * (start,end): source byte range
    * #module.id: declaration ID
    * #-module.id: built in declaration
    * kind name: resolved definition

    (0,29) -> #0.0 Func f
    (5,11) -> #0.1 Param a
    (8,11) -> #-2.2 Type i32
    (13,16) -> #-2.2 Type i32
    (26,27) -> #0.1 Param a
    |}]
