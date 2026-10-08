(* SPDX-License-Identifier: Apache-2.0 *)

let show paths = print_endline (String.concat " | " paths)

(* The environment is process wide so each test puts back what it found *)
let with_env name value run =
  let saved = Option.value (Sys.getenv_opt name) ~default:"" in
  Unix.putenv name value;
  Fun.protect ~finally:(fun () -> Unix.putenv name saved) run

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
