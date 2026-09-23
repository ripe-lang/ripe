(* SPDX-License-Identifier: Apache-2.0 *)

open Ripe
open Spanutils
open Diag

let%expect_test "single caret from a zero-width span" =
  let src = "main() {\n    break\n}\n" in
  render src Diagnostic.(error (point src "break") "invalid break statement");
  [%expect
    {|
    error: invalid break statement
      at <test>:2:5
            break
            ^
    |}]

let%expect_test "an unterminated string underlines the remaining source" =
  Pipeline.run_src "func main() i32 {\n  return \"unterminated";
  [%expect
    {|
    error: unterminated string
      at <test>:2:10
          return "unterminated
                 ^~~~~~~~~~~~~
    |}]

let%expect_test "a return type mismatch shows the expected and actual types" =
  Pipeline.run_src "func main() i32 {\n  return true\n}\n";
  [%expect
    {|
    error: type mismatch
      at <test>:2:10
          return true
                 ^~~~ expected i32, found bool
    |}]

let%expect_test "a parser error shows the token it found" =
  Pipeline.run_src "func main() i32 {\n  var = 1\n  return 0\n}\n";
  [%expect
    {|
    error: expected identifier
      at <test>:2:7
          var = 1
              ^ found =
    |}]

let%expect_test "an undefined name after a tab keeps its caret aligned" =
  Pipeline.run_src "func main() i32 {\n\treturn missing\n}\n";
  [%expect
    {|
    error: undefined variable
      at <test>:2:16
                return missing
                       ^~~~~~~
    |}]

let%expect_test "an unused variable warning includes the suggested name" =
  Pipeline.run_src "func main() i32 {\n  var value = 1\n  return 0\n}\n";
  [%expect
    {|
    warning: unused variable: value
      at <test>:2:7
          var value = 1
              ^~~~~
    help: prefix with an underscore: _value
    ok
    |}]

let%expect_test "a duplicate definition points to both declarations" =
  Pipeline.run_src
    {|func value() i32 { return 1 }
func value() i32 { return 2 }
func main() i32 { return value() }
|};
  [%expect
    {|
    error: already defined
      at <test>:2:6
        func value() i32 { return 2 }
             ^~~~~
      at <test>:1:6
        func value() i32 { return 1 }
             ^~~~~ previous definition here
    |}]

let%expect_test "an import cycle prints its path after the source" =
  let program, diags =
    Pipeline.load_tree
      [
        ("main.rp", "import a\nfunc main() i32 { return 0 }");
        ("a.rp", "import b\npub func fa() {}");
        ("b.rp", "import a\npub func fb() {}");
      ]
  in
  List.iter (render_in program) (Diagnostic.drain diags);
  [%expect
    {|
    error: import cycle
      at <test>:1:1
        import a
        ^~~~~~~~
      module a
        imports b from a.rp
        imports a from b.rp
    |}]

let%expect_test "an internal error prints its detail and reporting URL" =
  let src = "func main() i32 { return 0 }" in
  render src
    (Diagnostic.internal ~span:(span src "main") "test invariant failed");
  [%expect
    {|
    error: internal compiler error
      at <test>:1:6
        func main() i32 { return 0 }
             ^~~~
    test invariant failed
    help: this is a bug in ripec, please report it at https://github.com/ripe-lang/ripe/issues
    |}]

let%expect_test "a diagnostic with no span still renders" =
  render "" Diagnostic.(error_no_span "no source to point at");
  [%expect {| error: no source to point at |}]

let%expect_test "the headline drops the severity and keeps the message" =
  print_endline (Diagnostic.headline (Diagnostic.error_no_span "bad thing"));
  print_endline
    (Diagnostic.headline (Diagnostic.warning Span.dummy "odd thing"));
  [%expect {|
    bad thing
    odd thing
    |}]

let%expect_test "the primary span and detail come back out" =
  let src = "abc\n" in
  let d = Diagnostic.(error (span src "abc") "x" |> detail "the reason\n") in
  let show d =
    Printf.printf "%s %S\n"
      (match Diagnostic.primary d with
      | Some s -> Ripe.Span.show s
      | None -> "none")
      (Option.value (Diagnostic.detail_of d) ~default:"none")
  in
  show d;
  show (Diagnostic.error_no_span "y");
  [%expect {|
    (0,3) "the reason\n"
    none "none"
    |}]

let%expect_test "a sink counts errors but not warnings" =
  let sink = Diagnostic.sink () in
  Printf.printf "empty %b\n" (Diagnostic.has_errors sink);
  Diagnostic.emit sink (Diagnostic.warning Span.dummy "just a warning");
  Printf.printf "after warning %b\n" (Diagnostic.has_errors sink);
  Diagnostic.emit sink (Diagnostic.error_no_span "a real error");
  Printf.printf "after error %b\n" (Diagnostic.has_errors sink);
  [%expect
    {|
    empty false
    after warning false
    after error true
    |}]

let%expect_test "draining reads the sink without emptying it" =
  let sink = Diagnostic.sink () in
  Diagnostic.emit sink (Diagnostic.error_no_span "first");
  Diagnostic.emit sink (Diagnostic.error_no_span "second");
  let first = List.length (Diagnostic.drain sink) in
  let again = List.length (Diagnostic.drain sink) in
  Printf.printf "%d then %d\n" first again;
  [%expect {| 2 then 2 |}]

let%expect_test "taking the sink leaves it empty" =
  let sink = Diagnostic.sink () in
  Diagnostic.emit sink (Diagnostic.error_no_span "first");
  Diagnostic.emit sink (Diagnostic.error_no_span "second");
  let first = List.length (Diagnostic.take sink) in
  let again = List.length (Diagnostic.take sink) in
  Printf.printf "%d then %d\n" first again;
  [%expect {| 2 then 0 |}]

let%expect_test "spanless diagnostics keep the order they were made" =
  let sink = Diagnostic.sink () in
  Diagnostic.emit sink (Diagnostic.error_no_span "first");
  Diagnostic.emit sink (Diagnostic.warning Span.dummy "second");
  Diagnostic.emit sink (Diagnostic.error_no_span "third");
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
  Diagnostic.emit sink (Diagnostic.error (span src "three") "at three");
  Diagnostic.emit sink (Diagnostic.error (span src "one") "at one");
  Diagnostic.emit sink (Diagnostic.warning (span src "two") "at two");
  Diagnostic.emit sink (Diagnostic.error_no_span "nowhere");
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
      Diagnostic.(error (span src "abc") "boom")
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

let%expect_test "a mismatched delimiter points to the opening delimiter" =
  Pipeline.run_src "func main() i32 { return value( }";
  [%expect
    {|
    error: mismatched closing delimiter
      at <test>:1:33
        func main() i32 { return value( }
                                        ^ expected `)`
      at <test>:1:31
        func main() i32 { return value( }
                                      ^ to match this `(`
    |}]

let%expect_test "an undefined name after UTF text keeps its caret aligned" =
  Pipeline.run_src "func main() i32 {\n  var _s = \"é\"; return missing\n}\n";
  [%expect
    {|
    error: undefined variable
      at <test>:2:24
          var _s = "é"; return missing
                               ^~~~~~~
    |}]

let%expect_test "a long undefined name stops at the preview edge" =
  Pipeline.run_src
    {|func main() i32 {
  return a_name_that_is_much_longer_than_the_whole_source_preview_and_then_some_more_and_more_and_more
}
|};
  [%expect
    {|
    error: undefined variable
      at <test>:2:10
          return a_name_that_is_much_longer_than_the_whole_source_preview_and_then_some_more_and_more...
                 ^~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    |}]

let%expect_test
    "an undefined name near the end of a long expression stays visible" =
  Pipeline.run_src
    {|func main() i32 {
  return 1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + missing
}
|};
  [%expect
    {|
    error: undefined variable
      at <test>:2:101
        ... + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10 + 11 + 12 + 13 + 14 + 15 + 16 + 17 + 18 + 19 + 20 + missing
                                                                                                 ^~~~~~~
    |}]

let%expect_test "an unfinished function points to its opening brace" =
  Pipeline.run_src "func main() i32 {\n  return";
  [%expect
    {|
    error: unclosed delimiter
      at <test>:1:17
        func main() i32 {
                        ^
    |}]
