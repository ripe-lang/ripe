(* SPDX-License-Identifier: GPL-2.0-only *)

type token =
  | INT of int64 * string option
  | FLOAT of float
  | IDENT of string
  | STRING of string
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
  | LET
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
  | LBRACKET
  | RBRACKET
  | COMMA
  | COLON
  | DOTDOT
  | DOTDOTEQ
  | ELLIPSIS
  | DOT
  | SEMI
  | EOF
  | ERROR of string
  | TYPE
  | NEWTYPE
  | UNDEFINED

let keywords =
  [
    ("let", LET);
    ("const", CONST);
    ("var", VAR);
    ("return", RETURN);
    ("if", IF);
    ("elseif", ELSEIF);
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
    (* FIXME(ee4a): I don't know if I like "inline" something feels weird *)
    ("inline", INLINE);
    ("public", PUBLIC);
    ("func", FUNC);
    ("type", TYPE);
    ("newtype", NEWTYPE);
    ("undefined", UNDEFINED);
  ]

let lookup_keyword s = List.assoc_opt s keywords

let show_token = function
  | INT (n, suf) -> Int64.to_string n ^ Option.value ~default:"" suf
  | FLOAT f -> string_of_float f
  | IDENT s -> s
  | STRING s -> "\"" ^ s ^ "\""
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
  | SEMI -> ";"
  | EOF -> "<eof>"
  | ERROR s -> "<error: " ^ s ^ ">"
  | ( LET | CONST | VAR | RETURN | IF | ELSEIF | ELSE | WHILE | FOR | IN | TRUE
    | FALSE | BREAK | CONTINUE | AS | SIZEOF | NULL | EXTERN | STRUCT | INLINE
    | PUBLIC | FUNC | TYPE | NEWTYPE | UNDEFINED ) as t ->
      fst (List.find (fun (_, t') -> t' = t) keywords)
