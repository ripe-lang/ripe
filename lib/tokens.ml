(* SPDX-License-Identifier: GPL-2.0-only *)

type token =
  | INT of int64 * string option
  | FLOAT of float
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
  | LET
  | COMPTIME
  | VAR
  | RETURN
  | IF
  | ELSE
  | WHILE
  | FOR
  | IN
  | BREAK
  | CONTINUE
  | STRUCT
  | EXTERN
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
  | LBRACKET
  | RBRACKET
  | COMMA
  | COLON
  | DOTDOT
  | DOTDOTEQ
  | ELLIPSIS
  | DOT
  | AUTOSEMI
  | SEMI
  | EOF
  | ERROR of string
  | TYPE
  | NEWTYPE
  | UNDEFINED
  | IMPORT
  | MODULE

let keywords =
  [
    ("let", LET);
    ("comptime", COMPTIME);
    ("var", VAR);
    ("return", RETURN);
    ("if", IF);
    ("else", ELSE);
    ("while", WHILE);
    ("for", FOR);
    ("in", IN);
    ("true", TRUE);
    ("false", FALSE);
    ("break", BREAK);
    ("continue", CONTINUE);
    ("as", AS);
    ("sizeof", SIZEOF);
    ("null", NULL);
    ("extern", EXTERN);
    ("struct", STRUCT);
    ("pub", PUBLIC);
    ("func", FUNC);
    ("type", TYPE);
    ("newtype", NEWTYPE);
    ("undefined", UNDEFINED);
    ("import", IMPORT);
    ("module", MODULE);
  ]

let lookup_keyword s = List.assoc_opt s keywords

let show_token = function
  | INT (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | FLOAT f -> string_of_float f
  | IDENT s -> s
  | STRING s -> "\"" ^ s ^ "\""
  | CHAR c -> Printf.sprintf "'\\u{%X}'" c
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
  | AND -> "&&"
  | OR -> "||"
  | BANG -> "!"
  | ASSIGN -> "="
  | PLUS_ASSIGN -> "+="
  | MINUS_ASSIGN -> "-="
  | STAR_ASSIGN -> "*="
  | SLASH_ASSIGN -> "/="
  | PERCENT_ASSIGN -> "%="
  | AMP_ASSIGN -> "&="
  | PIPE_ASSIGN -> "|="
  | CARET_ASSIGN -> "^="
  | LSHIFT_ASSIGN -> "<<="
  | RSHIFT_ASSIGN -> ">>="
  | CARET -> "^"
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | LBRACKET -> "["
  | RBRACKET -> "]"
  | COMMA -> ","
  | COLON -> ":"
  | DOTDOT -> ".."
  | DOTDOTEQ -> "..="
  | ELLIPSIS -> "..."
  | DOT -> "."
  | AUTOSEMI | SEMI -> ";"
  | EOF -> "<eof>"
  | ERROR s -> "<error: " ^ s ^ ">"
  | ( LET | COMPTIME | VAR | RETURN | IF | ELSE | WHILE | FOR | IN | TRUE
    | FALSE | BREAK | CONTINUE | AS | SIZEOF | NULL | EXTERN | STRUCT | PUBLIC
    | FUNC | TYPE | NEWTYPE | UNDEFINED | IMPORT | MODULE ) as t ->
      fst (List.find (fun (_, t') -> t' = t) keywords)

let show_found_token (token : token) : string =
  if List.exists (fun (_, keyword) -> keyword = token) keywords then
    "`" ^ show_token token ^ "`"
  else show_token token
