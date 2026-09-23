(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let%expect_test "interner: a name comes back out as it went in" =
  let show s = print_endline (Interner.text (Interner.intern s)) in
  show "main";
  show "";
  show "a_very_long_identifier_name";
  show "x";
  [%expect {|
    main

    a_very_long_identifier_name
    x
    |}]

let%expect_test "interner: the same name is always the same id" =
  let a = Interner.intern "repeat" in
  let b = Interner.intern "repeat" in
  let c = Interner.intern "other" in
  Printf.printf "same %b different %b\n" (a = b) (a = c);
  [%expect {| same true different false |}]

let%expect_test "interner: two names that differ only in case stay apart" =
  let lower = Interner.intern "name" in
  let upper = Interner.intern "Name" in
  Printf.printf "%b %s %s\n" (lower = upper) (Interner.text lower)
    (Interner.text upper);
  [%expect {| false name Name |}]

let%expect_test "interner: an id prints as the text behind it" =
  Format.printf "%a@." Interner.pp (Interner.intern "printed");
  [%expect {| printed |}]
