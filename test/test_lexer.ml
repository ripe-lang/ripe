(* SPDX-License-Identifier: Apache-2.0 *)

open Spanutils
open Dump

let%expect_test "lexer: semicolon inserted after expression newline" =
  dump_tokens "x\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: semicolon inserted at eof" =
  dump_tokens "x";
  [%expect {|
    IDENT x
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: semicolon inserted inside parens" =
  dump_tokens "(\n1\n)\n";
  [%expect {|
    (
    INT 1
    AUTOSEMI
    )
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: semicolon inserted inside brackets" =
  dump_tokens "[\n1\n]\n";
  [%expect {|
    [
    INT 1
    AUTOSEMI
    ]
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: blank lines do not stack semicolons" =
  dump_tokens "x\n\n\ny\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    IDENT y
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: no semicolon after an operator" =
  dump_tokens "1 +\n2\n";
  [%expect {|
    INT 1
    +
    INT 2
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: string is one token" =
  dump_tokens {|"hello"|};
  [%expect {|
    STRING hello
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: empty string" =
  dump_tokens {|""|};
  [%expect {|
    STRING
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: braces are literal in a string" =
  dump_tokens {|"a{x}b"|};
  [%expect {|
    STRING a{x}b
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: escape sequences" =
  dump_tokens {|"a\nb\rc\td\\e\""|};
  [%expect {|
    STRING a\nb\rc\td\\e\"
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: line comment stripped to end of line" =
  dump_tokens "x // trailing comment\ny\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    IDENT y
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: comment only line" =
  dump_tokens "// just a comment\nx\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: block comment stripped" =
  dump_tokens "x /* inline */ y\n";
  [%expect {|
    IDENT x
    IDENT y
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: multiline block comment inserts semicolon" =
  dump_tokens "x /* one\ntwo */ y\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    IDENT y
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: nested block comment stripped" =
  dump_tokens "x /* a /* b */ c */ y\n";
  [%expect {|
    IDENT x
    IDENT y
    AUTOSEMI
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
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: all keywords" =
  dump_tokens
    "var const var return if else while for in true false break continue \
     sizeof bitcast null extern struct pub func type undefined\n\
     import module loop\n";
  [%expect
    {|
    KW var
    KW const
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
    KW sizeof
    KW bitcast
    KW null
    KW extern
    KW struct
    KW pub
    KW func
    KW type
    KW undefined
    AUTOSEMI
    KW import
    KW module
    KW loop
    EOF
    |}]

let%expect_test "lexer: i64 max literal" =
  dump_tokens "9223372036854775807\n";
  [%expect {|
    INT 9223372036854775807
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: u64 max literal" =
  dump_tokens "18446744073709551615\n";
  [%expect {|
    INT -1
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: literal above u64 is an error" =
  dump_tokens "99999999999999999999999\n";
  [%expect {|
    ERROR integer literal out of range
    EOF
    |}]

let%expect_test "lexer: float literal" =
  dump_tokens "3.14\n";
  [%expect {|
    FLOAT 3.14
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: hex literal" =
  dump_tokens "0xff\n";
  [%expect {|
    INT 255
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: binary literal" =
  dump_tokens "0b1010\n";
  [%expect {|
    INT 10
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: octal literal" =
  dump_tokens "0o17\n";
  [%expect {|
    INT 15
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: uppercase base prefix" =
  dump_tokens "0XFF\n";
  [%expect {|
    INT 255
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: hex arithmetic" =
  dump_tokens "0xff + 0b1\n";
  [%expect {|
    INT 255
    +
    INT 1
    AUTOSEMI
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
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: malformed hex is an error" =
  dump_tokens "0xg\n";
  [%expect {|
    ERROR invalid number literal
    EOF
    |}]

let%expect_test "lexer: malformed binary is an error" =
  dump_tokens "0b12\n";
  [%expect {|
    ERROR invalid number literal
    EOF
    |}]

let%expect_test "lexer: hex above u64 is an error" =
  dump_tokens "0xfffffffffffffffff\n";
  [%expect {|
    ERROR integer literal out of range
    EOF
    |}]

let%expect_test "lexer: CRLF newline inserts one semicolon" =
  dump_tokens "x\r\ny\r\n";
  [%expect {|
    IDENT x
    AUTOSEMI
    IDENT y
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: unterminated string yields an error token" =
  dump_tokens {|"abc|};
  [%expect {|
    ERROR unterminated string
    EOF
    |}]

let%expect_test "lexer: unknown escape is an error" =
  dump_tokens {|"a\qb"|};
  [%expect {|
    STRING ab
    ERROR unknown escape
    EOF
    |}]

let%expect_test "lexer: unexpected character is an error" =
  dump_tokens "@\n";
  [%expect {|
    ERROR unexpected character
    EOF
    |}]

let%expect_test "lexer: char literal is one code point" =
  dump_tokens "'A'\n";
  [%expect {|
    '\u{41}'
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: char escapes" =
  dump_tokens {|'\0' '\n' '\r' '\t' '\\' '\''|};
  [%expect
    {|
    '\u{0}'
    '\u{A}'
    '\u{D}'
    '\u{9}'
    '\u{5C}'
    '\u{27}'
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: multibyte char literals decode to a scalar" =
  dump_tokens "'\xc3\xa9' '\xf0\x9f\x98\x80'\n";
  [%expect {|
    '\u{E9}'
    '\u{1F600}'
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: max scalar U+10FFFF" =
  dump_tokens "'\xf4\x8f\xbf\xbf'\n";
  [%expect {|
    '\u{10FFFF}'
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: empty char literal is an error" =
  dump_tokens "''\n";
  [%expect {|
    ERROR empty character literal
    EOF
    |}]

let%expect_test "lexer: two chars in a literal is an error" =
  dump_tokens "'ab'\n";
  [%expect
    {|
    ERROR character literal must be a single character
    EOF
    |}]

let%expect_test "lexer: two scalars in a literal is an error" =
  dump_tokens "'\xf0\x9f\x98\x80\xf0\x9f\x98\x80'\n";
  [%expect
    {|
    ERROR character literal must be a single character
    EOF
    |}]

let%expect_test "lexer: unknown char escape is an error" =
  dump_tokens {|'\q'|};
  [%expect {|
    ERROR unknown escape: '\q'
    EOF
    |}]

let%expect_test "lexer: unterminated char literal is an error" =
  dump_tokens "'a\n";
  [%expect
    {|
    ERROR unterminated character literal
    IDENT a
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: leading UTF 8 BOM is ignored" =
  dump_tokens "\xEF\xBB\xBFfunc main() {}\n";
  [%expect
    {|
    KW func
    IDENT main
    (
    )
    {
    }
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: digit separators" =
  dump_tokens
    "1_000_000 1000_000 0xff_ff 0b1010_1010 0o1_7 1_000.000_1 1.5e1_0 2_5_5u8\n";
  [%expect
    {|
    INT 1000000
    INT 1000000
    INT 65535
    INT 170
    INT 15
    FLOAT 1000.0001
    FLOAT 15000000000.
    INT 255u8
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: separators anywhere after the first digit" =
  dump_tokens "1_0__0_ 1_u8\n";
  [%expect {|
    INT 100
    INT 1u8
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: separator right after a base prefix" =
  dump_tokens "0x_ff\n";
  [%expect {|
    ERROR invalid number literal
    EOF
    |}]

let%expect_test "lexer: a leading separator is a name" =
  dump_tokens "_1000\n";
  [%expect {|
    IDENT _1000
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: every suffix in decimal form" =
  dump_tokens "7i8 7i16 7i32 7i64 7isize 7u8 7u16 7u32 7u64 7usize\n";
  [%expect
    {|
    INT 7i8
    INT 7i16
    INT 7i32
    INT 7i64
    INT 7isize
    INT 7u8
    INT 7u16
    INT 7u32
    INT 7u64
    INT 7usize
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: every float suffix" =
  dump_tokens "1.5f32 1.5f64 1f32 1f64 1e3f32 1.5 1_0.5f64\n";
  [%expect
    {|
    FLOAT 1.5f32
    FLOAT 1.5f64
    FLOAT 1.f32
    FLOAT 1.f64
    FLOAT 1000.f32
    FLOAT 1.5
    FLOAT 10.5f64
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: every suffix in hex form" =
  dump_tokens
    "0x7i8 0x7i16 0x7i32 0x7i64 0x7isize 0x7u8 0x7u16 0x7u32 0x7u64 0x7usize\n";
  [%expect
    {|
    INT 7i8
    INT 7i16
    INT 7i32
    INT 7i64
    INT 7isize
    INT 7u8
    INT 7u16
    INT 7u32
    INT 7u64
    INT 7usize
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: every suffix in binary form" =
  dump_tokens
    "0b1i8 0b1i16 0b1i32 0b1i64 0b1isize 0b1u8 0b1u16 0b1u32 0b1u64 0b1usize\n";
  [%expect
    {|
    INT 1i8
    INT 1i16
    INT 1i32
    INT 1i64
    INT 1isize
    INT 1u8
    INT 1u16
    INT 1u32
    INT 1u64
    INT 1usize
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: every suffix in octal form" =
  dump_tokens
    "0o7i8 0o7i16 0o7i32 0o7i64 0o7isize 0o7u8 0o7u16 0o7u32 0o7u64 0o7usize\n";
  [%expect
    {|
    INT 7i8
    INT 7i16
    INT 7i32
    INT 7i64
    INT 7isize
    INT 7u8
    INT 7u16
    INT 7u32
    INT 7u64
    INT 7usize
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: enum and match keywords and the arrow" =
  dump_tokens "enum match => _\n";
  [%expect {|
    KW enum
    KW match
    =>
    _
    AUTOSEMI
    EOF
    |}]

let%expect_test "lexer: an identifier may start with an underscore" =
  dump_tokens "_ _x x_ _1\n";
  [%expect
    {|
    _
    IDENT _x
    IDENT x_
    IDENT _1
    AUTOSEMI
    EOF
    |}]
