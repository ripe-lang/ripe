(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Spanutils
open Diag

let%expect_test "single caret from a zero-width span" =
  let src = "main() {\n    break\n}\n" in
  render src
    Diagnostic.(error "invalid break statement" |> at (point src "break"));
  [%expect
    {|
    error: invalid break statement
      at <test>:2:5
            break
            ^
    |}]

let%expect_test "wide caret over a span" =
  let src = "record 12345\n" in
  render src Diagnostic.(error "expected identifier" |> at (span src "12345"));
  [%expect
    {|
    error: expected identifier
      at <test>:1:8
        record 12345
               ^~~~~
    |}]

let%expect_test "inline label after the caret" =
  let src = "wrap() = needsInt(oops)\n" in
  render src
    Diagnostic.(
      error "type mismatch"
      |> at (span src "oops")
      |> label "expected Int, found Str");
  [%expect
    {|
    error: type mismatch
      at <test>:1:19
        wrap() = needsInt(oops)
                          ^~~~ expected Int, found Str
    |}]

let%expect_test "help suggestion line" =
  let src = "var x = 1 hi\n" in
  render src
    Diagnostic.(
      error "expected `;`" |> at (point src "hi") |> help "add `;` here");
  [%expect
    {|
    error: expected `;`
      at <test>:1:11
        var x = 1 hi
                  ^
    help: add `;` here
    |}]

let%expect_test "caret aligns past a leading tab" =
  let src = "main() {\n\tvar x = 1\n}\n" in
  render src Diagnostic.(error "bad" |> at (span src "x"));
  [%expect
    {|
    error: bad
      at <test>:2:13
                var x = 1
                    ^
    |}]

let%expect_test "warning renders with its own label" =
  let src = "var x = 1\n" in
  render src Diagnostic.(warning "unused variable" |> at (span src "x"));
  [%expect
    {|
    warning: unused variable
      at <test>:1:5
        var x = 1
            ^
    |}]

let%expect_test "a secondary span points at the earlier place" =
  let src = "func f() {}\nfunc f() {}\n" in
  let first = span src "func f" in
  render src
    Diagnostic.(
      error "redefinition"
      |> at (span "\nfunc f() {}\n" "func f")
      |> secondary first "first defined here");
  [%expect
    {|
    error: redefinition
      at <test>:1:2
        func f() {}
         ^~~~~~
      at <test>:1:1
        func f() {}
        ^~~~~~ first defined here
    |}]

let%expect_test "a detail block follows the caret" =
  let src = "import a\n" in
  render src
    Diagnostic.(
      error "import cycle"
      |> at (span src "import a")
      |> detail "  module a\n    imports b\n");
  [%expect
    {|
    error: import cycle
      at <test>:1:1
        import a
        ^~~~~~~~
      module a
        imports b
    |}]

let%expect_test "a label, a detail and a help stack in order" =
  let src = "let x = y\n" in
  render src
    Diagnostic.(
      error "unknown name"
      |> at (span src "y")
      |> label "not found"
      |> detail "  looked in this module\n"
      |> help "did you mean `x`?");
  [%expect
    {|
    error: unknown name
      at <test>:1:9
        let x = y
                ^ not found
      looked in this module
    help: did you mean `x`?
    |}]

let%expect_test "a diagnostic with no span still renders" =
  render "" Diagnostic.(error "no source to point at");
  [%expect {| error: no source to point at |}]

let%expect_test "the headline drops the severity and keeps the message" =
  print_endline (Diagnostic.headline (Diagnostic.error "bad thing"));
  print_endline (Diagnostic.headline (Diagnostic.warning "odd thing"));
  [%expect {|
    bad thing
    odd thing
    |}]

let%expect_test "the primary span and detail come back out" =
  let src = "abc\n" in
  let d =
    Diagnostic.(error "x" |> at (span src "abc") |> detail "the reason\n")
  in
  let show d =
    Printf.printf "%s %S\n"
      (match Diagnostic.primary d with
      | Some s -> Ripe.Span.show s
      | None -> "none")
      (Option.value (Diagnostic.detail_of d) ~default:"none")
  in
  show d;
  show (Diagnostic.error "y");
  [%expect {|
    (0,3) "the reason\n"
    none "none"
    |}]

let%expect_test "a sink counts errors but not warnings" =
  let sink = Diagnostic.sink () in
  Printf.printf "empty %b\n" (Diagnostic.has_errors sink);
  Diagnostic.emit sink (Diagnostic.warning "just a warning");
  Printf.printf "after warning %b\n" (Diagnostic.has_errors sink);
  Diagnostic.emit sink (Diagnostic.error "a real error");
  Printf.printf "after error %b\n" (Diagnostic.has_errors sink);
  [%expect
    {|
    empty false
    after warning false
    after error true
    |}]

let%expect_test "draining reads the sink without emptying it" =
  let sink = Diagnostic.sink () in
  Diagnostic.emit sink (Diagnostic.error "first");
  Diagnostic.emit sink (Diagnostic.error "second");
  let first = List.length (Diagnostic.drain sink) in
  let again = List.length (Diagnostic.drain sink) in
  Printf.printf "%d then %d\n" first again;
  [%expect {| 2 then 2 |}]

let%expect_test "taking the sink leaves it empty" =
  let sink = Diagnostic.sink () in
  Diagnostic.emit sink (Diagnostic.error "first");
  Diagnostic.emit sink (Diagnostic.error "second");
  let first = List.length (Diagnostic.take sink) in
  let again = List.length (Diagnostic.take sink) in
  Printf.printf "%d then %d\n" first again;
  [%expect {| 2 then 0 |}]

let%expect_test "spanless diagnostics keep the order they were made" =
  let sink = Diagnostic.sink () in
  Diagnostic.emit sink (Diagnostic.error "first");
  Diagnostic.emit sink (Diagnostic.warning "second");
  Diagnostic.emit sink (Diagnostic.error "third");
  List.iter
    (fun d -> print_endline (Diagnostic.headline d))
    (Diagnostic.drain sink);
  [%expect {|
    first
    second
    third
    |}]

let%expect_test "diagnostics come back sorted by where they point" =
  let src = "one two three\n" in
  let sink = Diagnostic.sink () in
  Diagnostic.emit_error_at sink (span src "three") "at three";
  Diagnostic.emit_error_at sink (span src "one") "at one";
  Diagnostic.emit_warn_at sink (span src "two") "at two";
  Diagnostic.emit sink (Diagnostic.error "nowhere");
  List.iter
    (fun d -> print_endline (Diagnostic.headline d))
    (Diagnostic.drain sink);
  [%expect {|
    nowhere
    at one
    at two
    at three
    |}]

let%expect_test "the rendered text picks up color when asked" =
  let src = "abc\n" in
  let colored =
    Diagnostic.render (ctx ~color:true src)
      Diagnostic.(error "boom" |> at (span src "abc"))
  in
  Printf.printf "%S\n" colored;
  [%expect
    {| "\027[1;31merror\027[0m: boom\n  at <test>:1:1\n    abc\n    \027[1;31m^~~\027[0m\n" |}]

let%expect_test "an ice raises instead of returning" =
  (try ignore (Diagnostic.ice "cannot continue")
   with Diagnostic.Errors ds ->
     List.iter (fun d -> print_endline (Diagnostic.headline d)) ds);
  [%expect {| internal compiler error |}]

let%expect_test "the severity word gains color only when asked" =
  let show sev =
    Printf.printf "%S %S\n"
      (Diagnostic.severity_label false sev)
      (Diagnostic.severity_label true sev)
  in
  show Diagnostic.Error;
  show Diagnostic.Warning;
  [%expect
    {|
    "error" "\027[1;31merror\027[0m"
    "warning" "\027[1;33mwarning\027[0m"
    |}]
