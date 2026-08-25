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
  | CONST
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
  | SIZEOF
  | BITCAST
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
  | UNDEFINED
  | IMPORT
  | MODULE
  | LOOP
  | ENUM
  | MATCH
  | FATARROW
  | UNDERSCORE

let keywords =
  [
    ("const", CONST);
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
    ("sizeof", SIZEOF);
    ("bitcast", BITCAST);
    ("null", NULL);
    ("extern", EXTERN);
    ("struct", STRUCT);
    ("pub", PUBLIC);
    ("func", FUNC);
    ("type", TYPE);
    ("undefined", UNDEFINED);
    ("import", IMPORT);
    ("module", MODULE);
    ("loop", LOOP);
    ("enum", ENUM);
    ("match", MATCH);
  ]

module Keyword_table = Hashtbl.Make (String)

let keyword_table =
  let table = Keyword_table.create (List.length keywords) in
  let add (name, token) = Keyword_table.replace table name token in
  List.iter add keywords;
  table

let lookup_keyword name = Keyword_table.find_opt keyword_table name

let show_token = function
  | INT (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | FLOAT (f, suf) -> string_of_float f ^ Option.value ~default:"" suf
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
  | FATARROW -> "=>"
  | UNDERSCORE -> "_"
  | AUTOSEMI | SEMI -> ";"
  | EOF -> "<eof>"
  | ERROR s -> "<error: " ^ s ^ ">"
  | ( CONST | VAR | RETURN | IF | ELSE | WHILE | FOR | IN | TRUE | FALSE | BREAK
    | CONTINUE | SIZEOF | BITCAST | NULL | EXTERN | STRUCT | PUBLIC | FUNC
    | TYPE | UNDEFINED | IMPORT | MODULE | LOOP | ENUM | MATCH ) as t ->
      fst (List.find (fun (_, t') -> t' = t) keywords)

let show_found_token token =
  if List.exists (fun (_, keyword) -> keyword = token) keywords then
    "`" ^ show_token token ^ "`"
  else show_token token
