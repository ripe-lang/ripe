(* SPDX-License-Identifier: Apache-2.0 *)

module M = Ripe.Mir

let span : Ripe.Ast.span = Ripe.Ast.dummy_span

let local (ty : Ripe.Types.ty) : M.local =
  { M.name = None; ty; storage = M.Temp; span }

let place (base : M.local_id) (projections : M.projection list) : M.place =
  { M.base = M.Local base; projections; place_span = span }

let copy (ty : Ripe.Types.ty) (base : M.local_id) : M.operand =
  { M.desc = M.Copy (place base []); ty; span }

let term (desc : M.terminator_desc) : M.terminator option =
  Some { M.desc; span }

let block ?(statements = []) (terminator : M.terminator option) : M.block =
  { M.statements; terminator }

let func ?(locals = [| local (Ripe.Types.TInt Ripe.Types.I32) |])
    ?(blocks = [||]) ?(return_ty = Ripe.Types.TInt Ripe.Types.I32) () : M.func =
  {
    M.name = "f";
    source_name = "f";
    public = false;
    abi = Ripe.Types.Ripe;
    params = [];
    result = None;
    locals;
    blocks;
    return_ty;
    entry_point = false;
    span;
  }

let func_with_block ?locals ?return_ty ?(statements = []) terminator =
  func ?locals ?return_ty ~blocks:[| block ~statements terminator |] ()

let program ?(structs = []) (function_ : M.func) : M.program =
  { M.structs; globals = []; functions = [ function_ ] }

let verify (program : M.program) : unit =
  match Ripe.Mir.verify program with
  | Ok () -> print_endline "ok"
  | Error errors ->
      List.iter (fun error -> print_endline (Ripe.Mir.show_error error)) errors

let verify_func ?structs function_ = verify (program ?structs function_)

let%expect_test "mir: straight line scalar function" =
  Pipeline.run_mir "func add(a: i32, b: i32) i32 { return a + b }";
  [%expect
    {|
    func add(%0: i32, %1: i32) i32 {
      local %0 a: i32 param
      local %1 b: i32 param
      local %2: i32 temp

      block0:
        %2 = copy %0 + copy %1
        return copy %2
    }
    |}]

let%expect_test "mir verifier: every block has a terminator" =
  verify_func (func_with_block None);
  [%expect {| f: block 0 has no terminator |}]

let%expect_test "mir verifier: every referenced block exists" =
  verify_func (func_with_block (term (M.Jump 1)));
  [%expect {| f: block 1 does not exist |}]

let%expect_test "mir verifier: every local has a type" =
  verify_func
    (func_with_block ~locals:[| local Ripe.Types.TError |] (term M.Unreachable));
  [%expect {| f: local has no type |}]

let%expect_test "mir verifier: every place projection is valid" =
  let bad_place = place 0 [ M.Deref ] in
  let value : M.value =
    {
      M.desc = M.Use (copy (Ripe.Types.TInt Ripe.Types.I32) 0);
      ty = Ripe.Types.TInt Ripe.Types.I32;
    }
  in
  let statement : M.statement =
    { M.desc = M.Assign (bad_place, value); span }
  in
  verify_func (func_with_block ~statements:[ statement ] (term M.Unreachable));
  [%expect {| f: deref projection requires a pointer |}]

let%expect_test "mir verifier: returns match the function type" =
  let returned : M.operand =
    { M.desc = M.Const (M.Bool true); ty = Ripe.Types.TBool; span }
  in
  verify_func (func_with_block (term (M.ReturnValue (Some returned))));
  [%expect {| f: return has type bool but function returns i32 |}]

let%expect_test "mir verifier: every referenced local exists" =
  let returned = copy (Ripe.Types.TInt Ripe.Types.I32) 4 in
  verify_func (func_with_block (term (M.ReturnValue (Some returned))));
  [%expect {| f: local 4 does not exist |}]

let%expect_test "mir verifier: aggregate call storage has the result type" =
  let struct_name = Ripe.Qname.unresolved "pair" in
  let pair = Ripe.Types.TStruct (struct_name, []) in
  let call : M.call =
    {
      M.destination = Some (place 0 []);
      callee = M.Direct "make_pair";
      kind = M.Internal;
      args = [];
      return_ty = pair;
      variadic_start = None;
    }
  in
  let statement : M.statement = { M.desc = M.Call call; span } in
  let struct_decl : M.struct_decl =
    {
      M.name = struct_name;
      fields =
        [ Ripe.Types.TInt Ripe.Types.I32; Ripe.Types.TInt Ripe.Types.I32 ];
      local = false;
    }
  in
  verify_func ~structs:[ struct_decl ]
    (func_with_block ~statements:[ statement ] (term M.Unreachable));
  [%expect {| f: aggregate result storage has type i32 but call returns pair |}]

let%expect_test "mir: continue uses one shared step block" =
  Pipeline.run_mir
    {|
func f() i32 {
  var sum: i32 = 0
  for i in 0..5 {
    if i == 2 { continue }
    sum += i
  }
  return sum
}
|};
  [%expect
    {|
    func f() i32 {
      local %0 sum: i32 user
      local %1 i: i32 user
      local %2 for.hi: i32 temp
      local %3: bool temp
      local %4: bool temp
      local %5: i32 temp
      local %6: i32 temp

      block0:
        %0 = 0
        %1 = 0
        %2 = 5
        jump block1

      block1:
        %3 = copy %1 < copy %2
        branch copy %3 block2 block4

      block2:
        %4 = copy %1 == 2
        branch copy %4 block6 block7

      block3:
        %6 = copy %1 + 1
        %1 = copy %6
        jump block1

      block4:
        return copy %0

      block5:
        %5 = copy %0 + copy %1
        %0 = copy %5
        jump block3

      block6:
        jump block3

      block7:
        jump block5
    }
    |}]

(* BROKEN: const eval stripped, a const name reaches MIR with no storage
   let%expect_test "mir: only semantic constants fold before lowering" =
     Pipeline.run_mir
       {|
   const n: i32 = 2 + 3

   func f() i32 {
     return n + (1 + 2)
   }
   |};
     [%expect {|
       func f() i32 {
         block0:
           return 8
       }
       |}]
*)

let%expect_test "mir: labeled break targets the outer loop" =
  Pipeline.run_mir
    {|
extern "C" func printf(fmt: cstr, ...) i32

func main() i32 {
  var n = 0
  outer: loop {
    loop {
      n += 1
      if n == 4 { break :outer }
    }
  }
  printf("n=%d\n", n)
  return n
}
|};
  [%expect
    {|
    func main() i32 {
      local %0 n: i32 user
      local %1: i32 temp
      local %2: bool temp
      local %3: i32 temp

      block0:
        %0 = 0
        jump block1

      block1:
        jump block3

      block2:
        %3 = call @printf("n=%d\n", copy %0)
        return copy %0

      block3:
        %1 = copy %0 + 1
        %0 = copy %1
        %2 = copy %0 == 4
        branch copy %2 block6 block7

      block4:
        jump block1

      block5:
        jump block3

      block6:
        jump block2

      block7:
        jump block5
    }
    |}]

let%expect_test "mir: labeled break writes the outer loop value" =
  Pipeline.run_mir
    {|
extern "C" func printf(fmt: cstr, ...) i32

func main() i32 {
  var i = 0
  var found = outer: loop {
    var j = 0
    loop {
      j += 1
      if j == 4 { break :outer i * 100 + j }
    }
  }
  printf("found=%d\n", found)
  return found
}
|};
  [%expect
    {|
    func main() i32 {
      local %0 i: i32 user
      local %1 found: i32 user
      local %2 j: i32 user
      local %3: i32 temp
      local %4: bool temp
      local %5: i32 temp
      local %6: i32 temp
      local %7: i32 temp

      block0:
        %0 = 0
        jump block1

      block1:
        %2 = 0
        jump block3

      block2:
        %7 = call @printf("found=%d\n", copy %1)
        return copy %1

      block3:
        %3 = copy %2 + 1
        %2 = copy %3
        %4 = copy %2 == 4
        branch copy %4 block6 block7

      block4:
        jump block1

      block5:
        jump block3

      block6:
        %5 = copy %0 * 100
        %6 = copy %5 + copy %2
        %1 = copy %6
        jump block2

      block7:
        jump block5
    }
    |}]

let%expect_test "mir: a bounds check splits the block it guards" =
  Pipeline.run_mir "func get(a: []i32, i: usize) i32 { return a[i] }";
  [%expect
    {|
    func get(%0: []i32, %1: usize) i32 {
      local %0 a: []i32 param
      local %1 i: usize param
      local %2: usize temp

      block0:
        %2 = len %0
        assert_bounds copy %1 copy %2 block2 block1

      block1:
        panic bounds copy %1 copy %2

      block2:
        return copy %0[copy %1]
    }
    |}]

let%expect_test "mir: a -1 divisor skips the divide" =
  Pipeline.run_mir "func d(a: i32, b: i32) i32 { return a / b }";
  [%expect
    {|
    func d(%0: i32, %1: i32) i32 {
      local %0 a: i32 param
      local %1 b: i32 param
      local %2: i32 temp
      local %3: bool temp
      local %4: i32 temp
      local %5: i32 temp

      block0:
        assert_div_zero copy %1 block2 block1

      block1:
        panic div_zero copy %1

      block2:
        %3 = copy %1 == -1
        branch copy %3 block3 block4

      block3:
        %4 = -copy %0
        %2 = copy %4
        jump block5

      block4:
        %5 = copy %0 / copy %1
        %2 = copy %5
        jump block5

      block5:
        return copy %2
    }
    |}]

let%expect_test "mir: a returned str literal goes through storage" =
  Pipeline.run_mir {|func make() str { return "hello" }|};
  [%expect
    {|
    func make() str {
      local %0 result: str result

      block0:
        %0 = str "hello"
        return
    }
    |}]

(* BROKEN: const eval stripped, a const name reaches MIR with no storage
   let%expect_test "mir: a folded binding initializer needs no arithmetic" =
     Pipeline.run_mir
       {|
   const k: i32 = 6

   func f(runtime: i32) i32 {
     var folded: i32 = k * 7
     var partial: i32 = k + runtime
     return folded + partial
   }
   |};
     [%expect
       {|
       func f(%0: i32) i32 {
         local %0 runtime: i32 param
         local %1 folded: i32 user
         local %2 partial: i32 user
         local %3: i32 temp

         block0:
           %1 = 42
           %2 = 6 + copy %0
           %3 = copy %1 + copy %2
           return copy %3
       }
       |}]
*)

let%expect_test "mir: a positional struct literal lowers like a named one" =
  Pipeline.run_mir
    {|
struct pair { x: i32; y: i32 }

func f(a: i32, b: i32) i32 {
  var positional = pair { a, b }
  var named = pair { y: b, x: a }
  return positional.x + named.y
}
|};
  [%expect
    {|
    func f(%0: i32, %1: i32) i32 {
      local %0 a: i32 param
      local %1 b: i32 param
      local %2 positional: pair user
      local %3 named: pair user
      local %4: i32 temp

      block0:
        %2 = zero
        %2.field0 = copy %0
        %2.field1 = copy %1
        %3 = zero
        %3.field1 = copy %1
        %3.field0 = copy %0
        %4 = copy %2.field0 + copy %3.field1
        return copy %4
    }
    |}]

let%expect_test "mir: struct fields run in the order they are written" =
  Pipeline.run_mir
    {|
struct pair { x: i32; y: i32 }

func side(v: i32) i32 { return v }

func f() pair {
  return pair { y: side(1), x: side(2) }
}
|};
  [%expect
    {|
    func side(%0: i32) i32 {
      local %0 v: i32 param

      block0:
        return copy %0
    }

    func f() pair {
      local %0 result: pair result
      local %1: i32 temp
      local %2: i32 temp

      block0:
        %0 = zero
        %1 = call @side(1)
        %0.field1 = copy %1
        %2 = call @side(2)
        %0.field0 = copy %2
        return
    }
    |}]

let%expect_test "mir: a short circuit and skips the right side" =
  Pipeline.run_mir "func f(a: bool, b: bool) bool { return a && b }";
  [%expect
    {|
    func f(%0: bool, %1: bool) bool {
      local %0 a: bool param
      local %1 b: bool param
      local %2: bool temp

      block0:
        branch copy %0 block1 block2

      block1:
        %2 = copy %1
        jump block3

      block2:
        %2 = false
        jump block3

      block3:
        return copy %2
    }
    |}]

let%expect_test "mir: a short circuit or skips the right side" =
  Pipeline.run_mir "func f(a: bool, b: bool) bool { return a || b }";
  [%expect
    {|
    func f(%0: bool, %1: bool) bool {
      local %0 a: bool param
      local %1 b: bool param
      local %2: bool temp

      block0:
        branch copy %0 block2 block1

      block1:
        %2 = copy %1
        jump block3

      block2:
        %2 = true
        jump block3

      block3:
        return copy %2
    }
    |}]

let%expect_test "mir: an if used as a value writes one result local" =
  Pipeline.run_mir "func f(a: bool) i32 { return if a { 1 } else { 2 } }";
  [%expect
    {|
    func f(%0: bool) i32 {
      local %0 a: bool param
      local %1: i32 temp

      block0:
        branch copy %0 block2 block3

      block1:
        return copy %1

      block2:
        %1 = 1
        jump block1

      block3:
        %1 = 2
        jump block1
    }
    |}]

let%expect_test "mir: a match lowers to a chain of tests" =
  Pipeline.run_mir
    {|func f(n: i32) i32 {
  return match n {
    1 => 10
    2 => 20
    _ => 0
  }
}|};
  [%expect
    {|
    func f(%0: i32) i32 {
      local %0 n: i32 param
      local %1: i32 temp
      local %2: bool temp
      local %3: bool temp

      block0:
        %2 = copy %0 == 1
        branch copy %2 block3 block2

      block1:
        return copy %1

      block2:
        %3 = copy %0 == 2
        branch copy %3 block5 block4

      block3:
        %1 = 10
        jump block1

      block4:
        %1 = 0
        jump block1

      block5:
        %1 = 20
        jump block1
    }
    |}]

let%expect_test "mir: a range for counts without a bounds check" =
  Pipeline.run_mir
    {|func f() i32 {
  var t: i32 = 0
  for i in 0..3 { t += i }
  return t
}|};
  [%expect
    {|
    func f() i32 {
      local %0 t: i32 user
      local %1 i: i32 user
      local %2 for.hi: i32 temp
      local %3: bool temp
      local %4: i32 temp
      local %5: i32 temp

      block0:
        %0 = 0
        %1 = 0
        %2 = 3
        jump block1

      block1:
        %3 = copy %1 < copy %2
        branch copy %3 block2 block4

      block2:
        %4 = copy %0 + copy %1
        %0 = copy %4
        jump block3

      block3:
        %5 = copy %1 + 1
        %1 = copy %5
        jump block1

      block4:
        return copy %0
    }
    |}]

let%expect_test "mir: an inclusive range stops one step later" =
  Pipeline.run_mir
    {|func f() i32 {
  var t: i32 = 0
  for i in 0..=3 { t += i }
  return t
}|};
  [%expect
    {|
    func f() i32 {
      local %0 t: i32 user
      local %1 i: i32 user
      local %2 for.hi: i32 temp
      local %3: bool temp
      local %4: i32 temp
      local %5: bool temp
      local %6: i32 temp

      block0:
        %0 = 0
        %1 = 0
        %2 = 3
        jump block1

      block1:
        %3 = copy %1 <= copy %2
        branch copy %3 block2 block4

      block2:
        %4 = copy %0 + copy %1
        %0 = copy %4
        jump block3

      block3:
        %5 = copy %1 == copy %2
        branch copy %5 block4 block5

      block4:
        return copy %0

      block5:
        %6 = copy %1 + 1
        %1 = copy %6
        jump block1
    }
    |}]

let%expect_test "mir: a for over an array walks it by index" =
  Pipeline.run_mir
    {|func f(a: [3]i32) i32 {
  var t: i32 = 0
  for v in a { t += v }
  return t
}|};
  [%expect
    {|
    func f(%0: [3]i32) i32 {
      local %0 a: [3]i32 param
      local %1 t: i32 user
      local %2: *i32 temp
      local %3: usize temp
      local %4: usize temp
      local %5 v: i32 user
      local %6: *i32 temp
      local %7: usize temp
      local %8: bool temp
      local %9: i32 temp
      local %10: usize temp

      block0:
        %1 = 0
        %6 = data_ptr %0
        %7 = len %0
        %2 = copy %6
        %3 = copy %7
        %4 = 0
        jump block1

      block1:
        %8 = copy %4 < copy %3
        branch copy %8 block2 block4

      block2:
        %5 = copy %2[copy %4]
        %9 = copy %1 + copy %5
        %1 = copy %9
        jump block3

      block3:
        %10 = copy %4 + 1
        %4 = copy %10
        jump block1

      block4:
        return copy %1
    }
    |}]

let%expect_test "mir: a slice expression carries a base and a length" =
  Pipeline.run_mir "func f(a: [4]i32) []i32 { return a[1..3] }";
  [%expect
    {|
    func f(%1: [4]i32) []i32 {
      local %0 result: []i32 result
      local %1 a: [4]i32 param
      local %2: usize temp

      block0:
        %2 = len %1
        assert_slice_bounds 1 3 copy %2 block2 block1

      block1:
        panic slice_bounds 1 3 copy %2

      block2:
        %0 = slice %1 1 3
        return
    }
    |}]

let%expect_test "mir: a pair assign reads both sides before writing" =
  Pipeline.run_mir
    {|func f() i32 {
  var a: i32 = 1
  var b: i32 = 2
  a, b = b, a
  return a
}|};
  [%expect
    {|
    func f() i32 {
      local %0 a: i32 user
      local %1 b: i32 user
      local %2: i32 temp
      local %3: i32 temp

      block0:
        %0 = 1
        %1 = 2
        %2 = copy %1
        %3 = copy %0
        %0 = copy %2
        %1 = copy %3
        return copy %0
    }
    |}]

let%expect_test "mir: a compound assign reuses the place it writes" =
  Pipeline.run_mir
    {|func f() i32 {
  var a: [2]i32 = [1, 2]
  a[0] += 5
  return a[0]
}|};
  [%expect
    {|
    func f() i32 {
      local %0 a: [2]i32 user
      local %1: usize temp
      local %2: i32 temp
      local %3: usize temp

      block0:
        %0 = undef
        %0[0] = 1
        %0[1] = 2
        %1 = len %0
        assert_bounds 0 copy %1 block2 block1

      block1:
        panic bounds 0 copy %1

      block2:
        %2 = copy %0[0] + 5
        %0[0] = copy %2
        %3 = len %0
        assert_bounds 0 copy %3 block4 block3

      block3:
        panic bounds 0 copy %3

      block4:
        return copy %0[0]
    }
    |}]

let%expect_test "mir: a shift guards against an out of range count" =
  Pipeline.run_mir "func f(a: i32, b: i32) i32 { return a << b }";
  [%expect
    {|
    func f(%0: i32, %1: i32) i32 {
      local %0 a: i32 param
      local %1 b: i32 param
      local %2: i32 temp
      local %3: bool temp
      local %4: i32 temp

      block0:
        assert_negative_shift copy %1 block2 block1

      block1:
        panic negative_shift copy %1

      block2:
        %3 = copy %1 < 32
        branch copy %3 block3 block4

      block3:
        %4 = copy %0 << copy %1
        %2 = copy %4
        jump block5

      block4:
        %2 = 0
        jump block5

      block5:
        return copy %2
    }
    |}]

let%expect_test "mir: a variadic call marks where the fixed params stop" =
  Pipeline.run_mir
    {|extern "C" func printf(fmt: cstr, ...) i32
func f() i32 { return printf("%d %d\n", 1, 2) }|};
  [%expect
    {|
    func f() i32 {
      local %0: i32 temp

      block0:
        %0 = call @printf("%d %d\n", 1, 2)
        return copy %0
    }
    |}]

let%expect_test "mir: an array literal writes each element in order" =
  Pipeline.run_mir
    {|func f() i32 {
  var a: [3]i32 = [7, 8, 9]
  return a[1]
}|};
  [%expect
    {|
    func f() i32 {
      local %0 a: [3]i32 user
      local %1: usize temp

      block0:
        %0 = undef
        %0[0] = 7
        %0[1] = 8
        %0[2] = 9
        %1 = len %0
        assert_bounds 1 copy %1 block2 block1

      block1:
        panic bounds 1 copy %1

      block2:
        return copy %0[1]
    }
    |}]

let%expect_test "mir: a loop yields the value its break carries" =
  Pipeline.run_mir
    {|func f() i32 {
  var n = 0
  return loop {
    n += 1
    if n == 3 { break n }
  }
}|};
  [%expect
    {|
    func f() i32 {
      local %0 n: i32 user
      local %1: i32 temp
      local %2: i32 temp
      local %3: bool temp

      block0:
        %0 = 0
        jump block1

      block1:
        %2 = copy %0 + 1
        %0 = copy %2
        %3 = copy %0 == 3
        branch copy %3 block4 block5

      block2:
        return copy %1

      block3:
        jump block1

      block4:
        %1 = copy %0
        jump block2

      block5:
        jump block3
    }
    |}]

let%expect_test "mir: a nested field lands on one place with two projections" =
  Pipeline.run_mir
    {|struct Inner { v: i32 }
struct Outer { i: Inner }
func f(o: Outer) i32 { return o.i.v }|};
  [%expect
    {|
    func f(%0: Outer) i32 {
      local %0 o: Outer param

      block0:
        return copy %0.field0.field0
    }
    |}]

let%expect_test "mir: a deref through a pointer is a place projection" =
  Pipeline.run_mir {|func f(p: *i32) i32 {
  *p = 4
  return *p
}|};
  [%expect
    {|
    func f(%0: *i32) i32 {
      local %0 p: *i32 param

      block0:
        assert_null copy %0 block2 block1

      block1:
        panic null copy %0

      block2:
        %0.deref = 4
        assert_null copy %0 block4 block3

      block3:
        panic null copy %0

      block4:
        return copy %0.deref
    }
    |}]
