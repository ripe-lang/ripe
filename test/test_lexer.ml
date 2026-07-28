(* SPDX-License-Identifier: GPL-2.0-only *)

open Span_utils
open Dump

let%expect_test "lexer: semicolon inserted after expression newline" =
  dump_tokens "x\n";
  [%expect {|
    IDENT x
    SEMI
    EOF
    |}]

let%expect_test "lexer: semicolon inserted at eof" =
  dump_tokens "x";
  [%expect {|
    IDENT x
    SEMI
    EOF
    |}]

let%expect_test "lexer: semicolon inserted inside parens" =
  dump_tokens "(\n1\n)\n";
  [%expect {|
    (
    INT 1
    SEMI
    )
    SEMI
    EOF
    |}]

let%expect_test "lexer: semicolon inserted inside brackets" =
  dump_tokens "[\n1\n]\n";
  [%expect {|
    [
    INT 1
    SEMI
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
    SEMI
    EOF
    |}]

let%expect_test "lexer: empty string" =
  dump_tokens {|""|};
  [%expect {|
    STRING
    SEMI
    EOF
    |}]

let%expect_test "lexer: braces are literal in a string" =
  dump_tokens {|"a{x}b"|};
  [%expect {|
    STRING a{x}b
    SEMI
    EOF
    |}]

let%expect_test "lexer: escape sequences" =
  dump_tokens {|"a\nb\tc"|};
  [%expect {|
    STRING a\nb\tc
    SEMI
    EOF
    |}]

let%expect_test "lexer: line comment stripped to end of line" =
  dump_tokens "x // trailing comment\ny\n";
  [%expect {|
    IDENT x
    SEMI
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: comment only line" =
  dump_tokens "// just a comment\nx\n";
  [%expect {|
    IDENT x
    SEMI
    EOF
    |}]

let%expect_test "lexer: block comment stripped" =
  dump_tokens "x /* inline */ y\n";
  [%expect {|
    IDENT x
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: multiline block comment inserts semicolon" =
  dump_tokens "x /* one\ntwo */ y\n";
  [%expect {|
    IDENT x
    SEMI
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: nested block comment stripped" =
  dump_tokens "x /* a /* b */ c */ y\n";
  [%expect {|
    IDENT x
    IDENT y
    SEMI
    EOF
    |}]

let%expect_test "lexer: unterminated block comment errors" =
  dump_tokens "x /* never closes\n";
  [%expect {|
    IDENT x
    ERROR unterminated block comment
    EOF
    |}]

let%expect_test "lexer: operators and compound assignment" =
  dump_tokens
    "+ - * / % += -= *= /= %= &= |= ^= <<= >>= == != < > <= >= << >> && || & | \
     ~ ^ !";
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
    %=
    &=
    |=
    ^=
    <<=
    >>=
    ==
    !=
    <
    >
    <=
    >=
    <<
    >>
    &&
    ||
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
    "let var return if else while for in true false break continue as sizeof \
     null extern struct inline public func type newtype undefined\n\
     import\n";
  [%expect
    {|
    KW let
    KW var
    KW return
    KW if
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
    KW import
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

let%expect_test "lexer: hex literal" =
  dump_tokens "0xff\n";
  [%expect {|
    INT 255
    SEMI
    EOF
    |}]

let%expect_test "lexer: binary literal" =
  dump_tokens "0b1010\n";
  [%expect {|
    INT 10
    SEMI
    EOF
    |}]

let%expect_test "lexer: octal literal" =
  dump_tokens "0o17\n";
  [%expect {|
    INT 15
    SEMI
    EOF
    |}]

let%expect_test "lexer: uppercase base prefix" =
  dump_tokens "0XFF\n";
  [%expect {|
    INT 255
    SEMI
    EOF
    |}]

let%expect_test "lexer: hex arithmetic" =
  dump_tokens "0xff + 0b1\n";
  [%expect {|
    INT 255
    +
    INT 1
    SEMI
    EOF
    |}]

let%expect_test "lexer: exponent floats" =
  dump_tokens "1.5e3 1e3 1.5E3 2.0e-2\n";
  [%expect
    {|
    FLOAT 1500.
    FLOAT 1000.
    FLOAT 1500.
    FLOAT 0.02
    SEMI
    EOF
    |}]

let%expect_test "lexer: malformed hex is an error" =
  dump_tokens "0xg\n";
  [%expect {|
    ERROR invalid number literal: 0xg
    EOF
    |}]

let%expect_test "lexer: malformed binary is an error" =
  dump_tokens "0b12\n";
  [%expect {|
    ERROR invalid number literal: 0b12
    EOF
    |}]

let%expect_test "lexer: hex above u64 is an error" =
  dump_tokens "0xfffffffffffffffff\n";
  [%expect
    {|
    ERROR integer literal out of range
    INT 0
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
    STRING ab
    ERROR unknown escape: \q
    EOF
    |}]

let%expect_test "lexer: unexpected character is an error" =
  dump_tokens "@\n";
  [%expect {|
    ERROR unexpected character: @
    EOF
    |}]

let%expect_test "lexer: char literal is one code point" =
  dump_tokens "'A'\n";
  [%expect {|
    '\u{41}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: char escapes" =
  dump_tokens {|'\0' '\n' '\t' '\\' '\''|};
  [%expect
    {|
    '\u{0}'
    '\u{A}'
    '\u{9}'
    '\u{5C}'
    '\u{27}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: multibyte char literals decode to a scalar" =
  dump_tokens "'\xc3\xa9' '\xf0\x9f\x98\x80'\n";
  [%expect {|
    '\u{E9}'
    '\u{1F600}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: max scalar U+10FFFF" =
  dump_tokens "'\xf4\x8f\xbf\xbf'\n";
  [%expect {|
    '\u{10FFFF}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: empty char literal is an error" =
  dump_tokens "''\n";
  [%expect
    {|
    ERROR empty character literal
    '\u{0}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: two chars in a literal is an error" =
  dump_tokens "'ab'\n";
  [%expect
    {|
    ERROR character literal must be a single character
    '\u{0}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: two scalars in a literal is an error" =
  dump_tokens "'\xf0\x9f\x98\x80\xf0\x9f\x98\x80'\n";
  [%expect
    {|
    ERROR character literal must be a single character
    '\u{0}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: unknown char escape is an error" =
  dump_tokens {|'\q'|};
  [%expect
    {|
    ERROR unknown escape: '\q'
    '\u{0}'
    SEMI
    EOF
    |}]

let%expect_test "lexer: unterminated char literal is an error" =
  dump_tokens "'a\n";
  [%expect
    {|
    ERROR unterminated character literal
    '\u{0}'
    IDENT a
    SEMI
    EOF
    |}]
