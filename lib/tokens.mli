(* SPDX-License-Identifier: Apache-2.0 *)

type token =
  | INT of int64 * string option
  | FLOAT of float * string option
  | IDENT of string
  | STRING of string
  | CHAR of int
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | PERCENT
  | EQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  | LSHIFT
  | RSHIFT
  | AMP
  | PIPE
  | TILDE
  | AND
  | OR
  | BANG
  | ASSIGN
  | PLUS_ASSIGN
  | MINUS_ASSIGN
  | STAR_ASSIGN
  | SLASH_ASSIGN
  | PERCENT_ASSIGN
  | AMP_ASSIGN
  | PIPE_ASSIGN
  | CARET_ASSIGN
  | LSHIFT_ASSIGN
  | RSHIFT_ASSIGN
  | AND_ASSIGN
  | OR_ASSIGN
  | RETURN
  | IF
  | ELSE
  | WHILE
  | FOR
  | BREAK
  | CONTINUE
  | STRUCT
  | EXTERN
  | FUNC
  | CARET
  | TRUE
  | FALSE
  | NULL
  | SIZEOF
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | COMMA
  | COLON
  | DOTDOT
  | DOTDOTEQ
  | DOT
  | SEMI
  | EOF
  | ERROR of string
  | LOOP
  | ENUM
  | MATCH
  | FATARROW
  | UNDERSCORE
  | CAST
  | AT
  | ARROW

val keywords : (string * token) list
val find_keyword : string -> token option
val show_token : token -> string
val show_found_token : token -> string
