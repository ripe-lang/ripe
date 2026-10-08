(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let%expect_test "qname: a name prints as written" =
  print_endline (Qname.show (Fake.qname 1 "Point"));
  [%expect {| Point |}]

let%expect_test "qname: only the key compares and the name is for reading" =
  let a = Fake.qname 1 "Vec" in
  let b = Fake.qname 1 "Different" in
  let c = Fake.qname 2 "Vec" in
  Printf.printf "same key %b different id %b\n"
    (Qname.key a = Qname.key b)
    (Qname.key a = Qname.key c);
  [%expect {| same key true different id false |}]

let%expect_test "qname: an unresolved name carries the unresolved key" =
  let ghost = Qname.unresolved "Ghost" in
  Printf.printf "%s %b\n" (Qname.show ghost)
    (Qname.key ghost = Symbol.unresolved_key);
  [%expect {| Ghost true |}]
