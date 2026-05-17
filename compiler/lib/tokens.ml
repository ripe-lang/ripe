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
  | CONST
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
  | FUNC
  | CARET
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
  | DOTDOTEQ
  | DOT
  | SEMI
  | EOF

let show_token = function
  | INT n -> string_of_int n
  | FLOAT f -> string_of_float f
  | IDENT s -> s
  | STRING_PART s -> s
  | STRING_START -> "\"{"
  | STRING_END -> "\"}"
  | INTERP_START -> "${"
  | INTERP_END -> "}"
  | PLUS -> "+"
  | MINUS -> "-"
  | STAR -> "*"
  | SLASH -> "/"
  | PERCENT -> "%"
  | EQ -> "=="
  | NEQ -> "!="
  | LT -> "<"
  | GT -> ">"
  | LTE -> "<="
  | GTE -> ">="
  | LSHIFT -> "<<"
  | RSHIFT -> ">>"
  | AMP -> "&"
  | PIPE -> "|"
  | TILDE -> "~"
  | AND -> "and"
  | OR -> "or"
  | BANG -> "!"
  | ASSIGN -> "="
  | PLUS_ASSIGN -> "+="
  | MINUS_ASSIGN -> "-="
  | STAR_ASSIGN -> "*="
  | SLASH_ASSIGN -> "/="
  | CONST -> "const"
  | VAR -> "var"
  | RETURN -> "return"
  | IF -> "if"
  | ELSEIF -> "elseif"
  | ELSE -> "else"
  | WHILE -> "while"
  | FOR -> "for"
  | IN -> "in"
  | BREAK -> "break"
  | CONTINUE -> "continue"
  | STRUCT -> "struct"
  | EXTERN -> "extern"
  | INLINE -> "inline"
  | PUBLIC -> "public"
  | FUNC -> "func"
  | CARET -> "^"
  | TRUE -> "true"
  | FALSE -> "false"
  | NULL -> "null"
  | AS -> "as"
  | SIZEOF -> "sizeof"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | COMMA -> ","
  | COLON -> ":"
  | DOTDOT -> ".."
  | DOTDOTEQ -> "..="
  | DOT -> "."
  | SEMI -> ";"
  | EOF -> "<eof>"
