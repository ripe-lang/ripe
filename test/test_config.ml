(* SPDX-License-Identifier: Apache-2.0 *)

let show paths = print_endline (String.concat " | " paths)

(* The environment is process wide so each test puts back what it found *)
let with_env name value run =
  let saved = Option.value (Sys.getenv_opt name) ~default:"" in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () -> Unix.putenv name saved) run

let%expect_test "config: a unix list splits on colons" =
  show (Ripe.Config.split_paths ~sep:':' "/usr/lib/ripe:/home/me/libs");
  [%expect {| /usr/lib/ripe | /home/me/libs |}]

let%expect_test "config: a windows list splits on semicolons and keeps drives" =
  show (Ripe.Config.split_paths ~sep:';' "C:\\ripe\\std;D:\\libs");
  [%expect {| C:\ripe\std | D:\libs |}]

let%expect_test "config: empty entries drop out" =
  show (Ripe.Config.split_paths ~sep:':' ":/a::  :/b:");
  [%expect {| /a | /b |}]

let%expect_test "config: RIPE_PATH entries come before the standard roots" =
  with_env "RIPE_PATH" "/first:/second" (fun () ->
      let roots = Ripe.Config.search_roots () in
      show (List.filteri (fun i _ -> i < 2) roots));
  [%expect {| /first | /second |}]

let%expect_test "config: an unset RIPE_PATH leaves only the standard roots" =
  with_env "RIPE_PATH" "" (fun () ->
      let roots = Ripe.Config.search_roots () in
      Printf.printf "starts with a slash %b\n"
        (List.for_all (fun r -> String.starts_with ~prefix:"/" r) roots));
  [%expect {| starts with a slash true |}]

let%expect_test "config: RIPE_RUNTIME names the object outright" =
  with_env "RIPE_RUNTIME" "/dev/null" (fun () ->
      print_endline (Ripe.Config.runtime_object ()));
  [%expect {| /dev/null |}]

let%expect_test "config: a tool set in the environment wins" =
  with_env "QBE" "/bin/sh" (fun () ->
      Printf.printf "%b\n" (Sys.file_exists (Ripe.Config.qbe ())));
  [%expect {| true |}]

let%expect_test "config: a tool pointing at nothing says so" =
  with_env "QBE" "/nonexistent/qbe" (fun () ->
      try print_endline (Ripe.Config.qbe ())
      with Failure msg -> print_endline msg);
  [%expect {| QBE points to a missing tool: /nonexistent/qbe |}]

let%expect_test "config: each tool reads its own environment variable" =
  let show name value tool =
    with_env name value (fun () ->
        try Printf.printf "%s -> %b\n" name (Sys.file_exists (tool ()))
        with Failure msg -> print_endline msg)
  in
  show "RIPE_AS" "/nonexistent/as" Ripe.Config.assembler;
  show "RIPE_CC" "/nonexistent/cc" Ripe.Config.linker;
  show "RIPE_AS" "/bin/sh" Ripe.Config.assembler;
  show "RIPE_CC" "/bin/sh" Ripe.Config.linker;
  [%expect
    {|
    RIPE_AS points to a missing tool: /nonexistent/as
    RIPE_CC points to a missing tool: /nonexistent/cc
    RIPE_AS -> true
    RIPE_CC -> true
    |}]
