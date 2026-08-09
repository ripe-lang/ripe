open Pipeline
module M = Ripe.Mir
open M
open Ripe.Types

let span = Ripe.Ast.dummy_span

let%expect_test "mir: straight line scalar function" =
  run_mir "func add(a: i32, b: i32) i32 { return a + b }";
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

let local ty : M.local = { M.name = None; ty; storage = Temp; span }

let place base projections : M.place =
  { M.base = Local base; projections; place_span = span }

let copy ty base : M.operand = { M.desc = Copy (place base []); ty; span }
let term desc : M.terminator option = Some { M.desc; span }
let block ?(statements = []) terminator : M.block = { M.statements; terminator }

let func ?(locals = [| local (Ripe.Types.TInt I32) |]) ?(blocks = [||])
    ?(return_ty = Ripe.Types.TInt I32) () : M.func =
  {
    M.name = "f";
    params = [];
    locals;
    blocks;
    return_ty;
    entry_point = false;
    span;
  }

let program ?(structs = []) function_ : M.program =
  { M.structs; globals = []; functions = [ function_ ] }

let verify program =
  try
    Ripe.Mir_verify.verify program;
    print_endline "ok"
  with Ripe.Mir_verify.Invalid errors ->
    List.iter
      (fun error -> print_endline (Ripe.Mir_verify.show_error error))
      errors

let%expect_test "mir verifier: every block has a terminator" =
  verify (program (func ~blocks:[| block None |] ()));
  [%expect {| f: block 0 has no terminator |}]

let%expect_test "mir verifier: every referenced block exists" =
  verify (program (func ~blocks:[| block (term (Jump 1)) |] ()));
  [%expect {| f: block 1 does not exist |}]

let%expect_test "mir verifier: every local has a type" =
  verify
    (program
       (func
          ~locals:[| local Ripe.Types.TError |]
          ~blocks:[| block (term Unreachable) |]
          ()));
  [%expect {| f: local has no type |}]

let%expect_test "mir verifier: every place projection is valid" =
  let bad_place = place 0 [ Deref ] in
  let value : M.value =
    { M.desc = Use (copy (Ripe.Types.TInt I32) 0); ty = TInt I32; span }
  in
  let statement : M.statement = { M.desc = Assign (bad_place, value); span } in
  verify
    (program
       (func
          ~blocks:[| block ~statements:[ statement ] (term Unreachable) |]
          ()));
  [%expect {| f: deref projection requires a pointer |}]

let%expect_test "mir verifier: returns match the function type" =
  let returned : M.operand =
    { M.desc = Const (Bool true); ty = Ripe.Types.TBool; span }
  in
  verify
    (program (func ~blocks:[| block (term (ReturnValue (Some returned))) |] ()));
  [%expect {| f: return has type bool but function returns i32 |}]

let%expect_test "mir verifier: every referenced local exists" =
  verify
    (program
       (func
          ~blocks:[| block (term (ReturnValue (Some (copy (TInt I32) 4)))) |]
          ()));
  [%expect {| f: local 4 does not exist |}]

let%expect_test "mir verifier: aggregate call storage has the result type" =
  let struct_name = Ripe.Qname.make 0 [] "pair" in
  let pair = Ripe.Types.TStruct (struct_name, []) in
  let call : M.call =
    {
      M.destination = Some (place 0 []);
      callee = Direct "make_pair";
      kind = Internal;
      args = [];
      return_ty = pair;
      variadic_start = None;
    }
  in
  let statement : M.statement = { M.desc = Call call; span } in
  let struct_decl : M.struct_decl =
    { M.name = struct_name; fields = [ TInt I32; TInt I32 ] }
  in
  verify
    (program ~structs:[ struct_decl ]
       (func
          ~blocks:[| block ~statements:[ statement ] (term Unreachable) |]
          ()));
  [%expect {| f: aggregate result storage has type i32 but call returns pair |}]

let%expect_test "mir: continue uses one shared step block" =
  run_mir
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

let%expect_test "mir: only semantic constants fold before lowering" =
  run_mir {|
comptime n: i32 = 2 + 3

func f() i32 {
  return n + (1 + 2)
}
|};
  [%expect
    {|
    func f() i32 {
      local %0: i32 temp
      local %1: i32 temp

      block0:
        %0 = 1 + 2
        %1 = 5 + copy %0
        return copy %1
    }
    |}]
