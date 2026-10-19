(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Types
open Fake

(* The table is shared so a field can name a struct defined before it *)
let define structs name fields =
  Layout.set_variants structs (Keyname.key name) Struct [ fields ];
  TAdt (name, [])

let measure structs t =
  Printf.printf "%s size %d align %d\n" (show_ty t) (Layout.ty_size structs t)
    (Layout.ty_align structs t)

let offsets structs name count =
  let at i = string_of_int (Layout.field_offset structs name 0 i) in
  print_endline (String.concat " " (List.init count at))

let%expect_test "layout: scalars measure as wide as they align" =
  let structs = Layout.create () in
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
      TPtr;
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
    ptr size 8 align 8
    cstr size 8 align 8
    () size 0 align 1
    never size 0 align 1
    |}]

let%expect_test "layout: a str and a slice are a pointer and a length" =
  let structs = Layout.create () in
  List.iter (measure structs) [ TStr; TSlice (TInt I8); TSlice (TFloat F64) ];
  [%expect
    {|
    str size 16 align 8
    []i8 size 16 align 8
    []f64 size 16 align 8
    |}]

let%expect_test "layout: a field pads out to its own alignment" =
  let structs = Layout.create () in
  let name = keyname 1 "Pair" in
  let t = define structs name [ TInt I8; TInt I32 ] in
  measure structs t;
  offsets structs name 2;
  [%expect {|
    Pair size 8 align 4
    0 4
    |}]

let%expect_test "layout: trailing padding rounds the struct to its alignment" =
  let structs = Layout.create () in
  let name = keyname 1 "Tail" in
  let t = define structs name [ TInt I64; TInt I8 ] in
  measure structs t;
  offsets structs name 2;
  [%expect {|
    Tail size 16 align 8
    0 8
    |}]

let%expect_test "layout: field order changes the size" =
  let structs = Layout.create () in
  let loose = keyname 1 "Loose" in
  let tight = keyname 2 "Tight" in
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
  let structs = Layout.create () in
  let name = keyname 1 "Empty" in
  measure structs (define structs name []);
  [%expect {| Empty size 0 align 1 |}]

let%expect_test
    "layout: a nested struct keeps the alignment of its widest field" =
  let structs = Layout.create () in
  let inner = keyname 1 "Inner" in
  let outer = keyname 2 "Outer" in
  let i = define structs inner [ TInt I8; TInt I32 ] in
  let o = define structs outer [ TInt I8; i ] in
  measure structs o;
  offsets structs outer 2;
  [%expect {|
    Outer size 12 align 4
    0 4
    |}]

let%expect_test "layout: an array is its stride times its length" =
  let structs = Layout.create () in
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
  let structs = Layout.create () in
  let name = keyname 1 "Pair" in
  let t = define structs name [ TInt I8; TInt I32 ] in
  Printf.printf "stride %d\n" (Layout.stride structs t);
  measure structs (TArray (t, 3));
  [%expect {|
    stride 8
    [3]Pair size 24 align 4
    |}]

let%expect_test "layout: an alias measures as the type behind it" =
  let structs = Layout.create () in
  measure structs (TAlias (keyname 1 "Word", TInt I64));
  measure structs (TArray (TAlias (keyname 1 "Word", TInt I64), 2));
  [%expect {|
    Word size 8 align 8
    [2]Word size 16 align 8
    |}]

let%expect_test "layout: redefining fields drops the cached measurement" =
  let structs = Layout.create () in
  let name = keyname 1 "Grow" in
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
  let structs = Layout.create () in
  let name = keyname 1 "Mixed" in
  ignore (define structs name [ TInt I8; TFloat F64; TBool ]);
  let fields = Iarray.get (Layout.variants structs (Keyname.key name)) 0 in
  Iarray.iter (fun t -> print_string (show_ty t ^ " ")) fields;
  print_newline ();
  Printf.printf "%d\n" (Iarray.length fields);
  [%expect {|
    i8 f64 bool
    3
    |}]

let%expect_test "layout: an unknown type has no variants" =
  let structs = Layout.create () in
  Printf.printf "%d\n"
    (Iarray.length (Layout.variants structs (Keyname.key (keyname 9 "Ghost"))));
  [%expect {| 0 |}]

let%expect_test "layout: a field type comes back by index" =
  let structs = Layout.create () in
  let name = keyname 1 "Mixed" in
  ignore (define structs name [ TInt I8; TFloat F64; TBool ]);
  let at i = show_ty (Layout.field_ty structs name 0 i) in
  Printf.printf "%s %s %s\n" (at 0) (at 1) (at 2);
  [%expect {| i8 f64 bool |}]

let%expect_test "layout: a field index past the end is a compiler bug" =
  let structs = Layout.create () in
  let name = keyname 1 "Mixed" in
  ignore (define structs name [ TInt I8 ]);
  (try ignore (Layout.field_ty structs name 0 3)
   with Diagnostic.Errors ds ->
     List.iter (fun d -> print_endline (Diagnostic.headline d)) ds);
  [%expect {| internal compiler error |}]

let%expect_test "layout: an aggregate is addressed by pointer" =
  let structs = Layout.create () in
  let point = define structs (keyname 1 "Point") [ TInt I32 ] in
  let shape = keyname 2 "Shape" in
  let color = keyname 3 "Color" in
  Layout.set_variants structs (Keyname.key shape) Enum [ [ TInt I32 ]; [] ];
  Layout.set_variants structs (Keyname.key color) Enum [ []; [] ];
  let tys =
    [
      TInt I64;
      TStr;
      TSlice (TInt I8);
      TArray (TInt I8, 2);
      point;
      TPointer point;
      TUnit;
      TAdt (shape, []);
      TAdt (color, []);
    ]
  in
  List.iter
    (fun t ->
      Printf.printf "aggregate %s = %b\n" (show_ty t)
        (Layout.is_aggregate structs t))
    tys;
  [%expect
    {|
    aggregate i64 = false
    aggregate str = true
    aggregate []i8 = true
    aggregate [2]i8 = true
    aggregate Point = true
    aggregate *Point = false
    aggregate () = false
    aggregate Shape = true
    aggregate Color = false
    |}]

let%expect_test "layout: every variant starts right after the tag" =
  let structs = Layout.create () in
  let point = define structs (keyname 1 "Point") [ TInt I32; TInt I32 ] in
  let shape = keyname 2 "Shape" in
  Layout.set_variants structs (Keyname.key shape) Enum
    [ [ TInt I32 ]; [ point; TInt I32 ]; [] ];
  measure structs (TAdt (shape, []));
  let at variant i =
    string_of_int (Layout.field_offset structs shape variant i)
  in
  Printf.printf "%s | %s %s\n" (at 0 0) (at 1 0) (at 1 1);
  [%expect {|
    Shape size 16 align 4
    4 | 4 12
    |}]

let%expect_test "layout: the widest variant sets the alignment" =
  let structs = Layout.create () in
  let wide = keyname 1 "Wide" in
  Layout.set_variants structs (Keyname.key wide) Enum
    [ [ TInt U8 ]; [ TInt I64 ] ];
  measure structs (TAdt (wide, []));
  let at variant i =
    string_of_int (Layout.field_offset structs wide variant i)
  in
  Printf.printf "%s %s\n" (at 0 0) (at 1 0);
  [%expect {|
    Wide size 16 align 8
    4 8
    |}]

let%expect_test "layout: a unit only enum is a plain tag" =
  let structs = Layout.create () in
  let color = keyname 1 "Color" in
  Layout.set_variants structs (Keyname.key color) Enum [ []; []; [] ];
  measure structs (TAdt (color, []));
  Printf.printf "%b\n" (Layout.tagged structs (Keyname.key color));
  [%expect {|
    Color size 4 align 4
    false
    |}]
