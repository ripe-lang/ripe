(* SPDX-License-Identifier: Apache-2.0 *)

module N = Ripe.Nonempty

let show_ints values =
  values |> N.to_list |> List.map string_of_int |> String.concat ","

let%expect_test "nonempty: constructors and access" =
  let made = N.make 1 [ 2; 3 ] in
  Printf.printf "%d | %s" (N.hd made) (show_ints made);
  [%expect {| 1 | 1,2,3 |}]

let%expect_test "nonempty: split last" =
  let show value =
    let init, last = N.destruct_last value in
    Printf.printf "%s | %d\n"
      (String.concat "," (List.map string_of_int init))
      last
  in
  show (N.make 1 []);
  show (N.make 1 [ 2; 3 ]);
  [%expect {|
     | 1
    1,2 | 3
    |}]

let%expect_test "nonempty: print" =
  Format.printf "%a" (N.pp Format.pp_print_int) (N.make 1 [ 2; 3 ]);
  [%expect {| [1; 2; 3] |}]
