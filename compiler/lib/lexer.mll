{
open Parser

exception SyntaxError of string

let next_line lexbuf =
  Lexing.new_line lexbuf
}

let digit  = ['0'-'9']
let alpha  = ['a'-'z' 'A'-'Z' '_']
let alnum  = alpha | digit
let white  = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; NEWLINE }
  | digit+ as n       { INT (int_of_string n) }
  | alpha alnum* as s {
      match s with
      | "let"    -> LET
      | "var"    -> VAR
      | "return" -> RETURN
      | _        -> IDENT s
    }
  | "=="     { EQ }
  | "!="     { NEQ }
  | "<="     { LTE }
  | ">="     { GTE }
  | "<<"     { LSHIFT }
  | ">>"     { RSHIFT }
  | '<'      { LT }
  | '>'      { GT }
  | "&&"     { AND }
  | "||"     { OR }
  | "++"     { INCR }
  | "--"     { DECR }
  | "+="     { PLUS_ASSIGN }
  | "-="     { MINUS_ASSIGN }
  | "*="     { STAR_ASSIGN }
  | "/="     { SLASH_ASSIGN }
  | '!'      { BANG }
  | '+'      { PLUS }
  | '-'      { MINUS }
  | '*'      { STAR }
  | '/'      { SLASH }
  | '%'      { PERCENT }
  | '&'      { AMP }
  | '|'      { PIPE }
  | '~'      { TILDE }
  | '='      { ASSIGN }
  | '('      { LPAREN }
  | ')'      { RPAREN }
  | '{'      { LBRACE }
  | '}'      { RBRACE }
  | ','      { COMMA }
  | eof      { EOF }
  | _        { raise (SyntaxError ("line "
               ^ string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
               ^ ": unexpected character: "
               ^ Lexing.lexeme lexbuf)) }
