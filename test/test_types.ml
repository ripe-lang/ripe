(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Types
open Fake

let point = struct_ty 1 "Point"
let word = alias_ty 2 "Word" (TInt I64)

let%expect_test "types: a type prints the way it is written" =
  let show t = print_endline (show_ty t) in
  show (TInt U8);
  show (TFloat F32);
  show (TPointer (TPointer TBool));
  show TOpaquePtr;
  show (TArray (TSlice TChar, 3));
  show point;
  show word;
  show (TFunc ([ TInt I32; TStr ], TBool, Ripe));
  show (TFunc ([], TUnit, C));
  show TError;
  show TNever;
  show TNull;
  [%expect
    {|
    u8
    f32
    **bool
    *opaque
    [3][]char
    Point
    Word
    func (i32, str) bool
    func () ()
    <error>
    never
    null
    |}]

let%expect_test "types: a name in the current module drops its path" =
  let vec = TStruct (qname ~path:[ "math" ] 3 "Vec", []) in
  Printf.printf "anywhere    %s\n" (show_ty vec);
  Printf.printf "from math   %s\n" (show_ty_in [ "math" ] vec);
  Printf.printf "from other  %s\n" (show_ty_in [ "other" ] vec);
  [%expect
    {|
    anywhere    math.Vec
    from math   Vec
    from other  math.Vec
    |}]

let%expect_test "types: resolving walks down a chain of aliases" =
  let deep = TAlias (qname 4 "Outer", word) in
  print_endline (show_ty (resolve_ty deep));
  print_endline (show_ty (resolve_ty (TInt I8)));
  [%expect {|
    i64
    i8
    |}]

let%expect_test "types: erasing aliases reaches inside a compound type" =
  let show t = print_endline (show_ty (erase_aliases t)) in
  show word;
  show (TPointer word);
  show (TArray (word, 2));
  show (TSlice word);
  show (TFunc ([ word ], word, Ripe));
  show (TStruct (qname 1 "Point", [ word ]));
  [%expect
    {|
    i64
    *i64
    [2]i64
    []i64
    func (i64) i64
    Point[i64]
    |}]

let%expect_test "types: an alias equals the type behind it" =
  let show = pred2 "equals" ty_equal in
  show word (TInt I64);
  show (TPointer word) (TPointer (TInt I64));
  show word (TInt I32);
  show TError (TInt I32);
  show (TInt I32) TError;
  show point (TStruct (qname 1 "Point", []));
  [%expect
    {|
    Word equals i64 = true
    *Word equals *i64 = true
    Word equals i32 = false
    <error> equals i32 = true
    i32 equals <error> = true
    Point equals Point = true
    |}]

let%expect_test "types: an error anywhere inside makes the type an error" =
  let show = pred "has_error" has_error in
  show TError;
  show (TPointer TError);
  show (TArray (TError, 2));
  show (TSlice TError);
  show (TAlias (qname 5 "Bad", TError));
  show (TFunc ([ TInt I32 ], TError, Ripe));
  show (TFunc ([ TError ], TUnit, Ripe));
  show (TStruct (qname 1 "Point", [ TError ]));
  show (TPointer (TInt I32));
  [%expect
    {|
    has_error <error> = true
    has_error *<error> = true
    has_error [2]<error> = true
    has_error []<error> = true
    has_error Bad = true
    has_error func (i32) <error> = true
    has_error func (<error>) () = true
    has_error Point[<error>] = true
    has_error *i32 = false
    |}]

let%expect_test "types: only some kinds are unsigned or float" =
  let tys = [ TInt I32; TInt U32; TInt Isize; TInt Usize; TFloat F64; word ] in
  List.iter (pred "unsigned" is_unsigned) tys;
  List.iter (pred "float" is_float) tys;
  [%expect
    {|
    unsigned i32 = false
    unsigned u32 = true
    unsigned isize = false
    unsigned usize = true
    unsigned f64 = false
    unsigned Word = false
    float i32 = false
    float u32 = false
    float isize = false
    float usize = false
    float f64 = true
    float Word = false
    |}]

let%expect_test "types: an aggregate is addressed by pointer" =
  let tys =
    [
      TInt I64;
      TStr;
      TSlice (TInt I8);
      TArray (TInt I8, 2);
      point;
      TPointer point;
      TUnit;
    ]
  in
  List.iter (pred "aggregate" is_aggregate) tys;
  [%expect
    {|
    aggregate i64 = false
    aggregate str = true
    aggregate []i8 = true
    aggregate [2]i8 = true
    aggregate Point = true
    aggregate *Point = false
    aggregate () = false
    |}]

let%expect_test "types: a const may only use a scalar" =
  let tys = [ TInt I8; TFloat F32; TBool; TChar; TError; TStr; point; TUnit ] in
  List.iter (pred "scalar" is_scalar) tys;
  [%expect
    {|
    scalar i8 = true
    scalar f32 = true
    scalar bool = true
    scalar char = true
    scalar <error> = true
    scalar str = false
    scalar Point = false
    scalar () = false
    |}]

let%expect_test "types: a wide value takes the full eight bytes" =
  let tys =
    [
      TInt I32;
      TInt I64;
      TInt Usize;
      TInt U32;
      TPointer TBool;
      TOpaquePtr;
      TNull;
      TCStr;
      TFunc ([], TUnit, Ripe);
      TFloat F64;
      word;
    ]
  in
  List.iter (pred "wide" is_wide_ty) tys;
  [%expect
    {|
    wide i32 = false
    wide i64 = true
    wide usize = true
    wide u32 = false
    wide *bool = true
    wide *opaque = true
    wide null = true
    wide cstr = true
    wide func () () = true
    wide f64 = false
    wide Word = true
    |}]

let%expect_test "types: a narrow signed divide needs the overflow check" =
  let tys =
    [ TInt I8; TInt I16; TInt I32; TInt I64; TInt Isize; TInt U32; TInt U64 ]
  in
  List.iter (pred "checked divide" div_int_needs_check) tys;
  [%expect
    {|
    checked divide i8 = false
    checked divide i16 = false
    checked divide i32 = true
    checked divide i64 = true
    checked divide isize = true
    checked divide u32 = false
    checked divide u64 = false
    |}]

let%expect_test "types: every integer kind reports its byte size" =
  let show k =
    Printf.printf "%s %d unsigned %b\n" (show_int_kind k) (int_kind_size k)
      (int_kind_unsigned k)
  in
  List.iter show int_kinds;
  [%expect
    {|
    I8 1 unsigned false
    I16 2 unsigned false
    I32 4 unsigned false
    I64 8 unsigned false
    U8 1 unsigned true
    U16 2 unsigned true
    U32 4 unsigned true
    U64 8 unsigned true
    Isize 8 unsigned false
    Usize 8 unsigned true
    |}]

let%expect_test "types: a kind is limited by what it can hold" =
  let show k =
    Printf.printf "%s pos %Ld neg %Ld\n" (show_int_kind k)
      (int_kind_pos_limit k) (int_kind_neg_limit k)
  in
  List.iter show int_kinds;
  [%expect
    {|
    I8 pos 127 neg 128
    I16 pos 32767 neg 32768
    I32 pos 2147483647 neg 2147483648
    I64 pos 9223372036854775807 neg -9223372036854775808
    U8 pos 255 neg 0
    U16 pos 65535 neg 0
    U32 pos 4294967295 neg 0
    U64 pos -1 neg 0
    Isize pos 9223372036854775807 neg -9223372036854775808
    Usize pos -1 neg 0
    |}]

let%expect_test "types: a float counts exactly up to its mantissa" =
  List.iter
    (fun k ->
      Printf.printf "%s %Ld size %d\n" (show_float_kind k)
        (float_kind_exact_limit k) (float_kind_size k))
    float_kinds;
  [%expect {|
    F32 16777216 size 4
    F64 9007199254740992 size 8
    |}]

let%expect_test "types: a name maps back to the kind it spells" =
  let show s =
    Printf.printf "%s int %s float %s\n" s
      (match int_kind_of_string s with
      | Some k -> show_int_kind k
      | None -> "none")
      (match float_kind_of_string s with
      | Some k -> show_float_kind k
      | None -> "none")
  in
  show "i8";
  show "usize";
  show "f32";
  show "bool";
  show "I32";
  [%expect
    {|
    i8 int I8 float none
    usize int Usize float none
    f32 int none float F32
    bool int none float none
    I32 int none float none
    |}]

let%expect_test "types: the builtin table covers every spelled out type" =
  let show (name, builtin) =
    Printf.printf "%s %s\n" name
      (match builtin with BTy t -> show_ty t | BOpaque -> "<opaque>")
  in
  List.iter show builtins;
  [%expect
    {|
    i8 i8
    i16 i16
    i32 i32
    i64 i64
    u8 u8
    u16 u16
    u32 u32
    u64 u64
    isize isize
    usize usize
    f32 f32
    f64 f64
    bool bool
    char char
    cstr cstr
    str str
    never never
    opaque <opaque>
    |}]

let%expect_test "types: only the two named abis parse" =
  let show s =
    Printf.printf "%s -> %s\n" s
      (match func_abi_of_name s with
      | Some abi -> show_func_abi abi
      | None -> "none")
  in
  show "Ripe";
  show "C";
  show "ripe";
  show "Rust";
  [%expect
    {|
    Ripe -> Ripe
    C -> C
    ripe -> none
    Rust -> none
    |}]
