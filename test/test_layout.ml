(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Types
open Fake

(* The table is shared so a field can name a struct defined before it *)
let define structs name fields =
  Layout.set_struct_fields structs (Qname.key name) fields;
  TStruct (name, [])

let measure structs t =
  Printf.printf "%s size %d align %d\n" (show_ty t) (Layout.ty_size structs t)
    (Layout.ty_align structs t)

let offsets structs name count =
  let at i = string_of_int (Layout.field_offset structs name i) in
  print_endline (String.concat " " (List.init count at))

let%expect_test "layout: scalars measure as wide as they align" =
  let structs = Layout.make_structs () in
  List.iter (measure structs)
    [
      TInt I8;
      TInt I16;
      TInt I32;
      TInt I64;
      TInt Usize;
      TFloat F32;
      TFloat F64;
      TBool;
      TChar;
      TPointer (TInt I32);
      TOpaquePtr;
      TCStr;
      TUnit;
      TNever;
    ];
  [%expect
    {|
    i8 size 1 align 1
    i16 size 2 align 2
    i32 size 4 align 4
    i64 size 8 align 8
    usize size 8 align 8
    f32 size 4 align 4
    f64 size 8 align 8
    bool size 1 align 1
    char size 4 align 4
    *i32 size 8 align 8
    *opaque size 8 align 8
    cstr size 8 align 8
    () size 0 align 1
    never size 0 align 1
    |}]

let%expect_test "layout: a str and a slice are a pointer and a length" =
  let structs = Layout.make_structs () in
  List.iter (measure structs) [ TStr; TSlice (TInt I8); TSlice (TFloat F64) ];
  [%expect
    {|
    str size 16 align 8
    []i8 size 16 align 8
    []f64 size 16 align 8
    |}]

let%expect_test "layout: a field pads out to its own alignment" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Pair" in
  let t = define structs name [ TInt I8; TInt I32 ] in
  measure structs t;
  offsets structs name 2;
  [%expect {|
    Pair size 8 align 4
    0 4
    |}]

let%expect_test "layout: trailing padding rounds the struct to its alignment" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Tail" in
  let t = define structs name [ TInt I64; TInt I8 ] in
  measure structs t;
  offsets structs name 2;
  [%expect {|
    Tail size 16 align 8
    0 8
    |}]

let%expect_test "layout: field order changes the size" =
  let structs = Layout.make_structs () in
  let loose = qname 1 "Loose" in
  let tight = qname 2 "Tight" in
  let a = define structs loose [ TInt I8; TInt I64; TInt I8 ] in
  let b = define structs tight [ TInt I8; TInt I8; TInt I64 ] in
  measure structs a;
  offsets structs loose 3;
  measure structs b;
  offsets structs tight 3;
  [%expect
    {|
    Loose size 24 align 8
    0 8 16
    Tight size 16 align 8
    0 1 8
    |}]

let%expect_test "layout: an empty struct takes no space" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Empty" in
  measure structs (define structs name []);
  [%expect {| Empty size 0 align 1 |}]

let%expect_test
    "layout: a nested struct keeps the alignment of its widest field" =
  let structs = Layout.make_structs () in
  let inner = qname 1 "Inner" in
  let outer = qname 2 "Outer" in
  let i = define structs inner [ TInt I8; TInt I32 ] in
  let o = define structs outer [ TInt I8; i ] in
  measure structs o;
  offsets structs outer 2;
  [%expect {|
    Outer size 12 align 4
    0 4
    |}]

let%expect_test "layout: an array is its stride times its length" =
  let structs = Layout.make_structs () in
  List.iter (measure structs)
    [
      TArray (TInt I8, 3);
      TArray (TInt I32, 3);
      TArray (TArray (TInt I16, 2), 4);
      TArray (TInt I32, 0);
    ];
  [%expect
    {|
    [3]i8 size 3 align 1
    [3]i32 size 12 align 4
    [4][2]i16 size 16 align 2
    [0]i32 size 0 align 4
    |}]

let%expect_test "layout: a padded element still strides by its full size" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Pair" in
  let t = define structs name [ TInt I8; TInt I32 ] in
  Printf.printf "stride %d\n" (Layout.stride structs t);
  measure structs (TArray (t, 3));
  [%expect {|
    stride 8
    [3]Pair size 24 align 4
    |}]

let%expect_test "layout: an alias measures as the type behind it" =
  let structs = Layout.make_structs () in
  measure structs (TAlias (qname 1 "Word", TInt I64));
  measure structs (TArray (TAlias (qname 1 "Word", TInt I64), 2));
  [%expect {|
    Word size 8 align 8
    [2]Word size 16 align 8
    |}]

let%expect_test "layout: redefining fields drops the cached measurement" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Grow" in
  let t = define structs name [ TInt I8 ] in
  measure structs t;
  ignore (define structs name [ TInt I8; TInt I64 ]);
  measure structs t;
  [%expect {|
    Grow size 1 align 1
    Grow size 16 align 8
    |}]

let%expect_test "layout: align_to rounds up to the next multiple" =
  let show n a = Printf.printf "%d -> %d\n" n (Layout.align_to n a) in
  show 0 4;
  show 1 4;
  show 4 4;
  show 5 4;
  show 7 8;
  show 8 1;
  [%expect
    {|
    0 -> 0
    1 -> 4
    4 -> 4
    5 -> 8
    7 -> 8
    8 -> 8
    |}]

let%expect_test "layout: the recorded fields come back in order" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Mixed" in
  ignore (define structs name [ TInt I8; TFloat F64; TBool ]);
  let fields = Layout.struct_fields structs (Qname.key name) in
  Iarray.iter (fun t -> print_string (show_ty t ^ " ")) fields;
  print_newline ();
  Printf.printf "%d\n" (Iarray.length fields);
  [%expect {|
    i8 f64 bool
    3
    |}]

let%expect_test "layout: an unknown struct has no fields" =
  let structs = Layout.make_structs () in
  Printf.printf "%d\n"
    (Iarray.length (Layout.struct_fields structs (Qname.key (qname 9 "Ghost"))));
  [%expect {| 0 |}]

let%expect_test "layout: a field type comes back by index" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Mixed" in
  ignore (define structs name [ TInt I8; TFloat F64; TBool ]);
  let at i = show_ty (Layout.struct_field_ty structs name i) in
  Printf.printf "%s %s %s\n" (at 0) (at 1) (at 2);
  [%expect {| i8 f64 bool |}]

let%expect_test "layout: a field index past the end is a compiler bug" =
  let structs = Layout.make_structs () in
  let name = qname 1 "Mixed" in
  ignore (define structs name [ TInt I8 ]);
  (try ignore (Layout.struct_field_ty structs name 3)
   with Diagnostic.Errors ds ->
     List.iter (fun d -> print_endline (Diagnostic.headline d)) ds);
  [%expect {| internal compiler error |}]
