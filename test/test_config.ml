(* SPDX-License-Identifier: GPL-2.0-only *)

let show paths = print_endline (String.concat " | " paths)

let%expect_test "config: a unix list splits on colons" =
  show (Ripe.Config.split_paths ~sep:':' "/usr/lib/ripe:/home/me/libs");
  [%expect {| /usr/lib/ripe | /home/me/libs |}]

let%expect_test "config: a windows list splits on semicolons and keeps drives" =
  show (Ripe.Config.split_paths ~sep:';' "C:\\ripe\\std;D:\\libs");
  [%expect {| C:\ripe\std | D:\libs |}]

let%expect_test "config: empty entries drop out" =
  show (Ripe.Config.split_paths ~sep:':' ":/a::  :/b:");
  [%expect {| /a | /b |}]
