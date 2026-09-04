(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Types
open Fake

let point = struct_ty 1 "Point"
let other = struct_ty 2 "Other"
let byte_ptr = TPointer (TInt I8)

let operand_tys =
  [ TInt I32; TFloat F64; TBool; TChar; byte_ptr; TStr; point; TUnit ]

let show_binop op name =
  List.iter (pred name (Typred.binop_accepts op)) operand_tys;
  print_newline ()

(* The typed nodes are built by hand so an lvalue check needs no front end *)
let binding name ty =
  let s = Fake.symbol ~kind:(Symbol.Local Ast.Var) ~name 10 in
  Tast.mk ty (Tast.TIdent s)

let field base index ty = Tast.mk ty (Tast.TFieldAccess (base, index))

let index base ty =
  Tast.mk ty (Tast.TIndex (base, Tast.mk (TInt Usize) (Tast.TInt 0L)))

let deref base ty = Tast.mk ty (Tast.TUnOp (Ast.Deref, base))

let place what te =
  Printf.printf "%s lvalue %b root %s (%s)\n" what (Typred.is_lvalue te)
    (match Typred.root_binding te with
    | Some s -> s.Symbol.name
    | None -> "none")
    (match Typred.root_lvalue te with
    | Some root -> show_ty root.Tast.ty
    | None -> "none")

let%expect_test "typred: a null fits any pointer but not the other way" =
  let show = pred2 "accepts" Typred.compatible in
  show byte_ptr TNull;
  show TNull byte_ptr;
  show TOpaquePtr TNull;
  show TOpaquePtr byte_ptr;
  show byte_ptr TOpaquePtr;
  show (TInt I64) TNull;
  [%expect
    {|
    *i8 accepts null = true
    null accepts *i8 = false
    *opaque accepts null = true
    *opaque accepts *i8 = true
    *i8 accepts *opaque = false
    i64 accepts null = false
    |}]

let%expect_test "typred: a cstr and a byte pointer stand in for each other" =
  let show = pred2 "accepts" Typred.compatible in
  show TCStr byte_ptr;
  show byte_ptr TCStr;
  show TCStr (TPointer (TInt U8));
  show (TPointer byte_ptr) (TPointer TCStr);
  [%expect
    {|
    cstr accepts *i8 = true
    *i8 accepts cstr = true
    cstr accepts *u8 = false
    **i8 accepts *cstr = true
    |}]

let%expect_test "typred: never fits anywhere" =
  let show = pred2 "accepts" Typred.compatible in
  show (TInt I32) TNever;
  show TNever (TInt I32);
  show TStr TNever;
  [%expect
    {|
    i32 accepts never = true
    never accepts i32 = false
    str accepts never = true
    |}]

let%expect_test "typred: an error type swallows any mismatch" =
  let show = pred2 "accepts" Typred.compatible in
  show TError (TInt I32);
  show (TInt I32) TError;
  show (TPointer TError) (TPointer TStr);
  [%expect
    {|
    <error> accepts i32 = true
    i32 accepts <error> = true
    *<error> accepts *str = true
    |}]

let%expect_test "typred: a pointer needs the same pointee" =
  let show = pred2 "accepts" Typred.compatible in
  show byte_ptr (TPointer (TInt I8));
  show byte_ptr (TPointer (TInt I32));
  show (TPointer point) (TPointer point);
  show (TPointer point) (TPointer other);
  [%expect
    {|
    *i8 accepts *i8 = true
    *i8 accepts *i32 = false
    *Point accepts *Point = true
    *Point accepts *Other = false
    |}]

let%expect_test "typred: a slice takes an array of the same element" =
  let show = pred2 "accepts" Typred.compatible in
  show (TSlice (TInt I32)) (TArray (TInt I32, 4));
  show (TSlice (TInt I32)) (TArray (TInt I8, 4));
  show (TSlice (TInt I32)) (TSlice (TInt I32));
  show (TArray (TInt I32, 4)) (TSlice (TInt I32));
  show (TArray (TInt I32, 4)) (TArray (TInt I32, 5));
  [%expect
    {|
    []i32 accepts [4]i32 = true
    []i32 accepts [4]i8 = false
    []i32 accepts []i32 = true
    [4]i32 accepts []i32 = false
    [4]i32 accepts [5]i32 = false
    |}]

let%expect_test "typred: a func matches on abi, params and result" =
  let f abi ps r = TFunc (ps, r, abi) in
  let show a b =
    let spell t =
      match t with
      | TFunc (_, _, abi) ->
          Printf.sprintf "%s %s" (show_func_abi abi) (show_ty t)
      | t -> show_ty t
    in
    Printf.printf "%s accepts %s = %b\n" (spell a) (spell b)
      (Typred.compatible a b)
  in
  show (f Ripe [ TInt I32 ] TUnit) (f Ripe [ TInt I32 ] TUnit);
  show (f Ripe [ TInt I32 ] TUnit) (f C [ TInt I32 ] TUnit);
  show (f Ripe [ TInt I32 ] TUnit) (f AbiError [ TInt I32 ] TUnit);
  show (f Ripe [ TInt I32 ] TUnit) (f Ripe [] TUnit);
  show (f Ripe [] (TInt I32)) (f Ripe [] (TInt I64));
  [%expect
    {|
    Ripe func (i32) () accepts Ripe func (i32) () = true
    Ripe func (i32) () accepts C func (i32) () = false
    Ripe func (i32) () accepts AbiError func (i32) () = true
    Ripe func (i32) () accepts Ripe func () () = false
    Ripe func () i32 accepts Ripe func () i64 = false
    |}]

let%expect_test "typred: an alias is the type behind it" =
  let show = pred2 "accepts" Typred.compatible in
  let word = TAlias (qname 3 "Word", TInt I64) in
  show word (TInt I64);
  show (TInt I64) word;
  show word (TInt I32);
  [%expect
    {|
    Word accepts i64 = true
    i64 accepts Word = true
    Word accepts i32 = false
    |}]

let%expect_test "typred: widening keeps the sign it started with" =
  let show = pred2 "widens to" Typred.widens_to in
  show (TInt I8) (TInt I32);
  show (TInt I32) (TInt I8);
  show (TInt I32) (TInt I32);
  show (TInt U8) (TInt U32);
  show (TInt U8) (TInt I32);
  show (TInt I8) (TInt U32);
  show (TFloat F32) (TFloat F64);
  show (TFloat F64) (TFloat F32);
  show (TInt I32) (TFloat F64);
  [%expect
    {|
    i8 widens to i32 = true
    i32 widens to i8 = false
    i32 widens to i32 = false
    u8 widens to u32 = true
    u8 widens to i32 = true
    i8 widens to u32 = false
    f32 widens to f64 = true
    f64 widens to f32 = false
    i32 widens to f64 = false
    |}]

let%expect_test "typred: usize and i64 are the same width so neither widens" =
  let show = pred2 "widens to" Typred.widens_to in
  show (TInt I64) (TInt Usize);
  show (TInt Usize) (TInt I64);
  show (TInt I32) (TInt Usize);
  show (TInt U32) (TInt Isize);
  [%expect
    {|
    i64 widens to usize = false
    usize widens to i64 = false
    i32 widens to usize = false
    u32 widens to isize = true
    |}]

let%expect_test "typred: a mixed sign pair climbs to the next signed width" =
  let show a b =
    Printf.printf "%s with %s = %s\n" (show_ty a) (show_ty b)
      (match Typred.common_numeric_ty a b with
      | Some t -> show_ty t
      | None -> "none")
  in
  show (TInt I32) (TInt I32);
  show (TInt I8) (TInt I32);
  show (TInt U8) (TInt I8);
  show (TInt U32) (TInt I32);
  show (TInt U64) (TInt I32);
  show (TInt I64) (TInt U64);
  show (TFloat F32) (TFloat F64);
  show (TInt I32) (TFloat F32);
  [%expect
    {|
    i32 with i32 = i32
    i8 with i32 = i32
    u8 with i8 = i16
    u32 with i32 = i64
    u64 with i32 = none
    i64 with u64 = none
    f32 with f64 = f64
    i32 with f32 = none
    |}]

let%expect_test "typred: only a bool casts to a bool" =
  let show = pred2 "casts to" Typred.cast_ok in
  show TBool TBool;
  show (TInt I32) TBool;
  show TBool (TInt I32);
  [%expect
    {|
    bool casts to bool = true
    i32 casts to bool = false
    bool casts to i32 = true
    |}]

let%expect_test "typred: a char only crosses to an integer" =
  let show = pred2 "casts to" Typred.cast_ok in
  show TChar TChar;
  show TChar (TInt I32);
  show (TInt I32) TChar;
  show TChar (TFloat F64);
  show TChar byte_ptr;
  [%expect
    {|
    char casts to char = true
    char casts to i32 = true
    i32 casts to char = true
    char casts to f64 = false
    char casts to *i8 = false
    |}]

let%expect_test "typred: a pointer and a number stay on their own side" =
  let show = pred2 "casts to" Typred.cast_ok in
  show (TInt I32) (TFloat F64);
  show (TFloat F64) (TInt I64);
  show byte_ptr TOpaquePtr;
  show byte_ptr (TInt I64);
  show (TInt I64) byte_ptr;
  show (TFloat F64) byte_ptr;
  [%expect
    {|
    i32 casts to f64 = true
    f64 casts to i64 = true
    *i8 casts to *opaque = false
    *i8 casts to i64 = false
    i64 casts to *i8 = false
    f64 casts to *i8 = false
    |}]

let%expect_test "typred: an aggregate only casts to itself" =
  let show = pred2 "casts to" Typred.cast_ok in
  show point point;
  show point other;
  show TStr (TSlice (TInt U8));
  show (TArray (TInt I32, 2)) (TSlice (TInt I32));
  [%expect
    {|
    Point casts to Point = true
    Point casts to Other = false
    str casts to []u8 = false
    [2]i32 casts to []i32 = false
    |}]

let%expect_test "typred: a bitcast needs one width and no floats" =
  let show = pred2 "bitcasts to" Typred.bitcast_ok in
  show (TInt I64) byte_ptr;
  show byte_ptr (TInt I64);
  show (TInt I32) byte_ptr;
  show (TInt Usize) TOpaquePtr;
  show (TInt I64) (TFloat F64);
  show (TInt I64) TBool;
  show (TInt I32) (TInt U32);
  show point (TInt I64);
  [%expect
    {|
    i64 bitcasts to *i8 = true
    *i8 bitcasts to i64 = true
    i32 bitcasts to *i8 = false
    usize bitcasts to *opaque = true
    i64 bitcasts to f64 = false
    i64 bitcasts to bool = false
    i32 bitcasts to u32 = true
    Point bitcasts to i64 = false
    |}]

let%expect_test "typred: arithmetic takes numbers and bit work takes integers" =
  show_binop Ast.Add "add";
  show_binop Ast.Mod "mod";
  show_binop Ast.BitAnd "bitand";
  [%expect
    {|
    add i32 = true
    add f64 = true
    add bool = false
    add char = false
    add *i8 = false
    add str = false
    add Point = false
    add () = false

    mod i32 = true
    mod f64 = false
    mod bool = false
    mod char = false
    mod *i8 = false
    mod str = false
    mod Point = false
    mod () = false

    bitand i32 = true
    bitand f64 = false
    bitand bool = false
    bitand char = false
    bitand *i8 = false
    bitand str = false
    bitand Point = false
    bitand () = false
    |}]

let%expect_test "typred: equality reaches further than ordering" =
  show_binop Ast.Eq "eq";
  show_binop Ast.Lt "lt";
  show_binop Ast.And "and";
  [%expect
    {|
    eq i32 = true
    eq f64 = true
    eq bool = true
    eq char = true
    eq *i8 = true
    eq str = false
    eq Point = false
    eq () = false

    lt i32 = true
    lt f64 = true
    lt bool = false
    lt char = true
    lt *i8 = true
    lt str = false
    lt Point = false
    lt () = false

    and i32 = true
    and f64 = true
    and bool = true
    and char = true
    and *i8 = true
    and str = true
    and Point = true
    and () = true
    |}]

let%expect_test "typred: negation takes numbers and bitnot takes integers" =
  let tys = [ TInt I32; TFloat F64; TBool; TChar; byte_ptr ] in
  let show op name =
    List.iter (pred name (Typred.unop_accepts op)) tys;
    print_newline ()
  in
  show Ast.Neg "neg";
  show Ast.BitNot "bitnot";
  show Ast.Not "not";
  [%expect
    {|
    neg i32 = true
    neg f64 = true
    neg bool = false
    neg char = false
    neg *i8 = false

    bitnot i32 = true
    bitnot f64 = false
    bitnot bool = false
    bitnot char = false
    bitnot *i8 = false

    not i32 = true
    not f64 = true
    not bool = true
    not char = true
    not *i8 = true
    |}]

let%expect_test "typred: an unknown suffix falls back to the default kind" =
  let show s =
    Printf.printf "%s -> %s %s\n" s
      (show_int_kind (Typred.suffix_kind s))
      (show_float_kind (Typred.float_suffix_kind s))
  in
  show "i8";
  show "u64";
  show "usize";
  show "f32";
  show "";
  show "nonsense";
  [%expect
    {|
    i8 -> I8 F64
    u64 -> U64 F64
    usize -> Usize F64
    f32 -> I32 F32
     -> I32 F64
    nonsense -> I32 F64
    |}]

let%expect_test "typred: only integers and errors count as integer" =
  let show = pred "integer" Typred.is_integer in
  show (TInt I8);
  show (TInt Usize);
  show TError;
  show (TFloat F32);
  show TBool;
  show TChar;
  show (TAlias (qname 4 "Word", TInt I64));
  [%expect
    {|
    integer i8 = true
    integer usize = true
    integer <error> = true
    integer f32 = false
    integer bool = false
    integer char = false
    integer Word = true
    |}]

let%expect_test "typred: a signed literal is still a literal" =
  let show src =
    let wrapped = "func _f() { return " ^ src ^ " }" in
    match Pipeline.parse wrapped with
    | [ Ast.Func { body = [ Expr { desc = Return (Some e); _ } ]; _ } ] ->
        Printf.printf "%s = %b\n" src (Typred.is_num_literal e)
    | _ -> print_endline "<unexpected shape>"
  in
  show "1";
  show "1.5";
  show "-1";
  show "+1";
  show "- -1";
  show "x";
  show "1 + 1";
  show "true";
  [%expect
    {|
    1 = true
    1.5 = true
    -1 = true
    +1 = true
    - -1 = true
    x = false
    1 + 1 = false
    true = false
    |}]

let%expect_test "typred: a place is whatever can be written through" =
  let p = binding "p" (TPointer (TInt I32)) in
  let v = binding "v" point in
  place "a binding" v;
  place "a field" (field v 0 (TInt I32));
  place "an element" (index (binding "a" (TArray (TInt I32, 2))) (TInt I32));
  place "a deref" (deref p (TInt I32));
  place "a literal" (Tast.mk (TInt I32) (Tast.TInt 1L));
  [%expect
    {|
    a binding lvalue true root v (Point)
    a field lvalue true root v (Point)
    an element lvalue true root a ([2]i32)
    a deref lvalue true root none (none)
    a literal lvalue false root none (none)
    |}]

let%expect_test "typred: a walk through a pointer leaves the binding behind" =
  let v = binding "v" point in
  let p = binding "p" (TPointer point) in
  let s = binding "s" (TSlice (TInt I32)) in
  place "a nested field" (field (field v 0 point) 1 (TInt I32));
  place "a field on a pointer" (field p 0 (TInt I32));
  place "an element of a slice" (index s (TInt I32));
  place "a field under a deref" (field (deref p point) 0 (TInt I32));
  [%expect
    {|
    a nested field lvalue true root v (Point)
    a field on a pointer lvalue true root none (none)
    an element of a slice lvalue true root none (none)
    a field under a deref lvalue true root none (none)
    |}]
