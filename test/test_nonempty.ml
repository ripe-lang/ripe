(* SPDX-License-Identifier: Apache-2.0 *)

module N = Ripe.Nonempty

let show_ints values =
  values |> N.to_list |> List.map string_of_int |> String.concat ","

let%expect_test "nonempty: constructors and access" =
  let made = N.make 1 [ 2; 3 ] in
  let one = N.one 4 in
  let prepended = N.cons 0 made in
  Printf.printf "%d | %s | %s | %s" (N.hd made) (show_ints made) (show_ints one)
    (show_ints prepended);
  [%expect {| 1 | 1,2,3 | 4 | 0,1,2,3 |}]

let%expect_test "nonempty: transform head" =
  N.make 1 [ 2; 3 ]
  |> N.map_hd (fun value -> value + 9)
  |> show_ints |> print_endline;
  [%expect {| 10,2,3 |}]

let%expect_test "nonempty: split last" =
  let show value =
    let init, last = N.destruct_last value in
    Printf.printf "%s | %d\n"
      (String.concat "," (List.map string_of_int init))
      last
  in
  show (N.one 1);
  show (N.make 1 [ 2; 3 ]);
  [%expect {|
     | 1
    1,2 | 3
    |}]

let%expect_test "nonempty: find mapped value" =
  let find_even value = if value mod 2 = 0 then Some (value * 10) else None in
  let show value =
    N.find_map find_even value |> Option.map string_of_int
    |> Option.value ~default:"none"
    |> print_endline
  in
  show (N.make 2 [ 4 ]);
  show (N.make 1 [ 4 ]);
  show (N.make 1 [ 3 ]);
  [%expect {|
    20
    40
    none
    |}]

let%expect_test "nonempty: print" =
  Format.printf "%a" (N.pp Format.pp_print_int) (N.make 1 [ 2; 3 ]);
  [%expect {| [1; 2; 3] |}]
