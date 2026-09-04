(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let make module_id id path base = Fake.qname ~module_id ~path id base

let%expect_test "qname: a name prints with the path it came from" =
  print_endline (Qname.show (make 0 1 [] "Point"));
  print_endline (Qname.show (make 0 1 [ "math" ] "Vec"));
  print_endline (Qname.show (make 0 1 [ "math"; "vector" ] "Unit"));
  [%expect {|
    Point
    math.Vec
    math.vector.Unit
    |}]

let%expect_test "qname: a reader in the same module sees only the base" =
  let vec = make 0 1 [ "math" ] "Vec" in
  let show current =
    Printf.printf "read from %-13s %s\n"
      (if List.is_empty current then "the root" else String.concat "." current)
      (Qname.show_in current vec)
  in
  show [ "math" ];
  show [];
  show [ "math"; "vector" ];
  show [ "other" ];
  [%expect
    {|
    read from math          Vec
    read from the root      math.Vec
    read from math.vector   math.Vec
    read from other         math.Vec
    |}]

let%expect_test "qname: a name at the root hides nothing" =
  let point = make 0 1 [] "Point" in
  Printf.printf "from the root %s\n" (Qname.show_in [] point);
  Printf.printf "from math     %s\n" (Qname.show_in [ "math" ] point);
  [%expect {|
    from the root Point
    from math     Point
    |}]

let%expect_test "qname: only the key compares and the path is for reading" =
  let a = make 0 1 [ "math" ] "Vec" in
  let b = make 0 1 [ "other" ] "Different" in
  let c = make 0 2 [ "math" ] "Vec" in
  Printf.printf "same key %b different id %b\n"
    (Qname.key a = Qname.key b)
    (Qname.key a = Qname.key c);
  [%expect {| same key true different id false |}]

let%expect_test "qname: an unresolved name carries the unresolved key" =
  let ghost = Qname.unresolved "Ghost" in
  Printf.printf "%s %b\n" (Qname.show ghost)
    (Qname.key ghost = Symbol.unresolved_key);
  [%expect {| Ghost true |}]
