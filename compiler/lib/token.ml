type t =
  | INT of int
  | IDENT of string
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | COMMA
  | EOF

let to_string = function
  | INT n   -> Printf.sprintf "INT(%d)" n
  | IDENT s -> Printf.sprintf "IDENT(%s)" s
  | PLUS    -> "PLUS"
  | MINUS   -> "MINUS"
  | STAR    -> "STAR"
  | SLASH   -> "SLASH"
  | LPAREN  -> "LPAREN"
  | RPAREN  -> "RPAREN"
  | LBRACE  -> "LBRACE"
  | RBRACE  -> "RBRACE"
  | COMMA   -> "COMMA"
  | EOF     -> "EOF"
