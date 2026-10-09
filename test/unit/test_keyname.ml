(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let%expect_test "keyname: a name prints as written" =
  print_endline (Keyname.show (Fake.keyname 1 "Point"));
  [%expect {| Point |}]

let%expect_test "keyname: names and declaration keys stay separate" =
  let a = Fake.keyname 1 "Vec" in
  let b = Fake.keyname 1 "Different" in
  let c = Fake.keyname 2 "Vec" in
  Printf.printf "same key %b different id %b\n"
    (Keyname.key a = Keyname.key b)
    (Keyname.key a = Keyname.key c);
  [%expect {| same key true different id false |}]

let%expect_test "keyname: an unresolved name carries the unresolved key" =
  let ghost = Keyname.unresolved "Ghost" in
  Printf.printf "%s %b\n" (Keyname.show ghost)
    (Keyname.key ghost = Symbol.unresolved_key);
  [%expect {| Ghost true |}]
