(* SPDX-License-Identifier: GPL-2.0-only *)

open Helpers

let%expect_test "lexer: semicolon inserted after expression newline" =
  dump_tokens "x\n";
  [%expect {|
    IDENT x
    SEMI
    EOF
    |}]

let%expect_test "lexer: no semicolon without trailing newline" =
  dump_tokens "x";
  [%expect {|
    IDENT x
    EOF
    |}]

let%expect_test "lexer: no semicolon inside parens" =
  dump_tokens "(\n1\n)\n";
  [%expect {|
    (
    INT 1
    )
    SEMI
    EOF
    |}]

let%expect_test "lexer: no semicolon inside brackets" =
  dump_tokens "[\n1\n]\n";
  [%expect {|
    [
    INT 1
    ]
    SEMI
    EOF
    |}]

let%expect_test "lexer: blank lines do not stack semicolons" =
  dump_tokens "x\n\n\ny\n";
  [%expect {|
    IDENT x
    SEMI
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: no semicolon after an operator" =
  dump_tokens "1 +\n2\n";
  [%expect {|
    INT 1
    +
    INT 2
    SEMI
    EOF
    |}]

let%expect_test "lexer: string is one token" =
  dump_tokens {|"hello"|};
  [%expect {|
    STRING hello
    EOF
    |}]

let%expect_test "lexer: empty string" =
  dump_tokens {|""|};
  [%expect {|
    STRING
    EOF
    |}]

let%expect_test "lexer: braces are literal in a string" =
  dump_tokens {|"a{x}b"|};
  [%expect {|
    STRING a{x}b
    EOF
    |}]

let%expect_test "lexer: escape sequences" =
  dump_tokens {|"a\nb\tc"|};
  [%expect {|
    STRING a\nb\tc
    EOF
    |}]

let%expect_test "lexer: line comment stripped to end of line" =
  dump_tokens "x # trailing comment\ny\n";
  [%expect {|
    IDENT x
    SEMI
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: comment only line" =
  dump_tokens "# just a comment\nx\n";
  [%expect {|
    IDENT x
    SEMI
    EOF
    |}]

let%expect_test "lexer: operators and compound assignment" =
  dump_tokens "+ - * / % += -= *= /= == != < > <= >= << >> && || & | ~ ^ !";
  [%expect
    {|
    +
    -
    *
    /
    %
    +=
    -=
    *=
    /=
    ==
    !=
    <
    >
    <=
    >=
    <<
    >>
    and
    or
    &
    |
    ~
    ^
    !
    EOF
    |}]

let%expect_test "lexer: punctuation" =
  dump_tokens "( ) { } [ ] , : . .. ..= ... ;";
  [%expect
    {|
    (
    )
    {
    }
    [
    ]
    ,
    :
    .
    ..
    ..=
    ...
    SEMI
    EOF
    |}]

let%expect_test "lexer: keyword versus identifier" =
  dump_tokens "for forth\n";
  [%expect {|
    KW for
    IDENT forth
    SEMI
    EOF
    |}]

let%expect_test "lexer: all keywords" =
  dump_tokens
    "const var return if elseif else while for in true false break continue as \
     sizeof null extern struct inline public func type newtype undefined\n";
  [%expect
    {|
    KW const
    KW var
    KW return
    KW if
    KW elseif
    KW else
    KW while
    KW for
    KW in
    KW true
    KW false
    KW break
    KW continue
    KW as
    KW sizeof
    KW null
    KW extern
    KW struct
    KW inline
    KW public
    KW func
    KW type
    KW newtype
    KW undefined
    SEMI
    EOF
    |}]

let%expect_test "lexer: i64 max literal" =
  dump_tokens "9223372036854775807\n";
  [%expect {|
    INT 9223372036854775807
    SEMI
    EOF
    |}]

let%expect_test "lexer: u64 max literal" =
  dump_tokens "18446744073709551615\n";
  [%expect {|
    INT -1
    SEMI
    EOF
    |}]

let%expect_test "lexer: literal above u64 is an error" =
  dump_tokens "99999999999999999999999\n";
  [%expect
    {|
    ERROR integer literal out of range
    INT 0
    SEMI
    EOF
    |}]

let%expect_test "lexer: float literal" =
  dump_tokens "3.14\n";
  [%expect {|
    FLOAT 3.14
    SEMI
    EOF
    |}]

let%expect_test "lexer: CRLF newline inserts one semicolon" =
  dump_tokens "x\r\ny\r\n";
  [%expect {|
    IDENT x
    SEMI
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: unterminated string yields an error token" =
  dump_tokens {|"abc|};
  [%expect {|
    STRING abc
    ERROR unterminated string
    EOF
    |}]

let%expect_test "lexer: unknown escape is an error" =
  dump_tokens {|"a\qb"|};
  [%expect {|
    ERROR unknown escape: \q
    STRING ab
    EOF
    |}]

let%expect_test "lexer: unexpected character is an error" =
  dump_tokens "@\n";
  [%expect {|
    ERROR unexpected character: @
    EOF
    |}]
