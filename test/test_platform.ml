(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe

let%expect_test "platform: a host reads as one system and one architecture" =
  let host = Platform.host () in
  let parts = String.split_on_char '-' host in
  let system = List.hd parts in
  Printf.printf "parts %d lowercase system %b no space %b not empty %b\n"
    (List.length parts)
    (system = String.lowercase_ascii system)
    (not (String.contains host ' '))
    (List.for_all (fun part -> not (String.is_empty part)) parts);
  [%expect {| parts 2 lowercase system true no space true not empty true |}]

let%expect_test "platform: asking twice gives the same answer" =
  Printf.printf "%b\n" (Platform.host () = Platform.host ());
  [%expect {| true |}]

let%expect_test "platform: the architecture is already the normal spelling" =
  (match String.split_on_char '-' (Platform.host ()) with
  | [ _; architecture ] ->
      Printf.printf "%b\n" (architecture <> "amd64" && architecture <> "aarch64")
  | _ -> print_endline "unexpected host shape");
  [%expect {| true |}]
