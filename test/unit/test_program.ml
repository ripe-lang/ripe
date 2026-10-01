(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let dump (program : Program.t) =
  let unit_ (u : Program.unit_) =
    Printf.printf "  unit %s base %d decls %d\n" u.source.filename u.source.base
      (List.length u.ast.Ast.decls)
  in
  let dependency (d : Program.dependency) =
    Printf.printf "  imports module %d\n" d.target
  in
  let module_ (m : Program.module_) =
    Printf.printf "module %d %s%s\n" m.module_id (String.concat "." m.path)
      (if m.failed then " failed" else "");
    List.iter unit_ m.units;
    List.iter dependency m.dependencies
  in
  Array.iter module_ program.modules

let load ?search_roots files =
  let program, diags = Pipeline.load_tree ?search_roots files in
  List.iter (Diag.render_in program) (Diagnostic.drain diags);
  program

let show ?search_roots files = dump (load ?search_roots files)

(* The root is boilerplate so a test shows only what it imports *)
let importing what =
  ("main.rp", Printf.sprintf "import %s;\nfn main() i32 { return 0 }" what)

let%expect_test "program: a lone root file is one module" =
  show [ ("main.rp", {|fn main() i32 { return 0 }|}) ];
  [%expect {|
    module 0 main
      unit main.rp base 0 decls 1
    |}]

let%expect_test "program: an import brings in a second module" =
  show
    [
      ("main.rp", {|import math;
fn main() i32 { return math.add(1) }|});
      ("math.rp", {|pub fn add(a: i32) i32 { return a }|});
    ];
  [%expect
    {|
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math
      unit math.rp base 50 decls 1
    |}]

let%expect_test "program: two files sharing an import load it once" =
  show
    [
      ("main.rp", {|import left;
import right;
fn main() i32 { return 0 }|});
      ("left.rp", {|import shared;
pub fn l() {}|});
      ("right.rp", {|import shared;
pub fn r() {}|});
      ("shared.rp", {|pub fn s() {}|});
    ];
  [%expect
    {|
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
      imports module 3
    module 1 left
      unit left.rp base 54 decls 1
      imports module 2
    module 2 shared
      unit shared.rp base 83 decls 1
    module 3 right
      unit right.rp base 97 decls 1
      imports module 2
    |}]

let%expect_test "program: a directory of files merges into one module" =
  show
    [
      importing "math";
      ("math/add.rp", {|module math;
pub fn add() {}|});
      ("math/sub.rp", {|module math;
pub fn sub() {}|});
    ];
  [%expect
    {|
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math
      unit math/add.rp base 40 decls 1
      unit math/sub.rp base 69 decls 1
    |}]

let%expect_test "program: a merged module concatenates the decls of its files" =
  let program =
    load
      [
        importing "math";
        ("math/add.rp", {|module math;
pub fn add() {}|});
        ("math/sub.rp", {|module math;
pub fn sub() {}
pub fn neg() {}|});
      ]
  in
  Printf.printf "%d\n" (List.length (Program.module_decls program.modules.(1)));
  [%expect {| 3 |}]

let%expect_test "program: a module that is both a file and a directory clashes"
    =
  show
    [
      importing "math";
      ("math.rp", {|pub fn add() {}|});
      ("math/extra.rp", {|module math;
pub fn sub() {}|});
    ];
  [%expect
    {|
    error: module is both a file and a directory
      at <test>:1:1
        import math;
        ^~~~~~~~~~~
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math failed
      unit math.rp base 40 decls 0
    |}]

let%expect_test "program: a missing module lists every root it tried" =
  show ~search_roots:[ "vendor"; "std" ] [ importing "math" ];
  [%expect
    {|
    error: module not found
      at <test>:1:1
        import math;
        ^~~~~~~~~~~
      tried math.rp
            vendor/math.rp
            std/math.rp
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math failed
      unit math.rp base 40 decls 0
    |}]

let%expect_test "program: a search root supplies a module beside the source" =
  show ~search_roots:[ "vendor" ]
    [ importing "math"; ("vendor/math.rp", {|pub fn add() {}|}) ];
  [%expect
    {|
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math
      unit vendor/math.rp base 40 decls 1
    |}]

let%expect_test "program: an import cycle names the hops it went through" =
  show
    [
      importing "a";
      ("a.rp", {|import b;
pub fn fa() {}|});
      ("b.rp", {|import a;
pub fn fb() {}|});
    ];
  [%expect
    {|
    error: import cycle
      at <test>:1:1
        import a;
        ^~~~~~~~
      module a
        imports b from a.rp
        imports a from b.rp
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 a
      unit a.rp base 37 decls 1
      imports module 2
    module 2 b
      unit b.rp base 62 decls 1
      imports module 1
    |}]

let%expect_test "program: a module importing itself is a cycle" =
  show [ importing "a"; ("a.rp", {|import a;
pub fn fa() {}|}) ];
  [%expect
    {|
    error: import cycle
      at <test>:1:1
        import a;
        ^~~~~~~~
      module a
        imports a from a.rp
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 a
      unit a.rp base 37 decls 1
      imports module 1
    |}]

let%expect_test "program: a header naming the wrong module is reported" =
  show
    [
      importing "math";
      ("math/add.rp", {|module math;
pub fn add() {}|});
      ("math/sub.rp", {|module arithmetic;
pub fn sub() {}|});
    ];
  [%expect
    {|
    error: module name mismatch
      at <test>:1:1
        module arithmetic;
        ^~~~~~~~~~~~~~~~~ expected math
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math
      unit math/add.rp base 40 decls 1
      unit math/sub.rp base 69 decls 1
    |}]

let%expect_test "program: a header naming the parent points at the parent" =
  show
    [
      importing "math.vec";
      ("math/vec/one.rp", {|module vec;
pub fn one() {}|});
      ("math/vec/two.rp", {|module math;
pub fn two() {}|});
    ];
  [%expect
    {|
    error: module name mismatch
      at <test>:1:1
        module math;
        ^~~~~~~~~~~ expected vec
    help: import `math` instead
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math.vec
      unit math/vec/one.rp base 44 decls 1
      unit math/vec/two.rp base 72 decls 1
    |}]

let%expect_test "program: a merged file with no header is reported" =
  show
    [
      importing "math";
      ("math/add.rp", {|module math;
pub fn add() {}|});
      ("math/sub.rp", {|pub fn sub() {}|});
    ];
  [%expect
    {|
    error: missing module header
      at <test>:1:1
        pub fn sub() {}
        ^ expected `module math`
    help: every file beside a module header needs the same header
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math
      unit math/add.rp base 40 decls 1
      unit math/sub.rp base 69 decls 1
    |}]

let%expect_test "program: a single file module needs no header" =
  show [ importing "math"; ("math.rp", {|pub fn add() {}|}) ];
  [%expect
    {|
    module 0 main
      unit main.rp base 0 decls 1
      imports module 1
    module 1 math
      unit math.rp base 40 decls 1
    |}]

let%expect_test "program: every file gets its own slice of the offset space" =
  let program = load [ importing "math"; ("math.rp", {|pub fn add() {}|}) ] in
  let at pos = (Program.source_at program pos).Program.filename in
  Printf.printf "%s %s %s %s\n" (at 0) (at 39) (at 40) (at 1000);
  [%expect {| main.rp main.rp math.rp math.rp |}]

let%expect_test
    "program: an offset before the first file falls back to the root" =
  let program = load [ ("main.rp", {|fn main() i32 { return 0 }|}) ] in
  print_endline (Program.source_at program (-1)).Program.filename;
  [%expect {| main.rp |}]
