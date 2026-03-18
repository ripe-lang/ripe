(* SPDX-License-Identifier: GPL-2.0-only *)

type token =
  | INT of int
  | FLOAT of float
  | IDENT of string
  | STRING_PART of string
  | STRING_START
  | STRING_END
  | INTERP_START
  | INTERP_END
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
  | INCR
  | DECR
  | LET
  | VAR
  | RETURN
  | IF
  | ELSEIF
  | ELSE
  | WHILE
  | FOR
  | IN
  | BREAK
  | CONTINUE
  | STRUCT
  | EXTERN
  | INLINE
  | PUBLIC
  | CARET
  | AT
  | TRUE
  | FALSE
  | NULL
  | AS
  | SIZEOF
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | COLON
  | DOTDOT
  | DOT
  | SEMI
  | EOF
