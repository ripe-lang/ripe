{
open Parser

exception SyntaxError of string

let next_line lexbuf =
  Lexing.new_line lexbuf

let buf = Buffer.create 64 (* auto buffer resize *)
}

let digit   = ['0'-'9']
let alpha   = ['a'-'z' 'A'-'Z' '_']
let alnum   = alpha | digit
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | white              { read lexbuf }
  | '#' [^ '\n' '\r']* { read lexbuf }
  | newline            { next_line lexbuf; NEWLINE }
  | digit+ as n        { INT (int_of_string n) }
  | alpha alnum* as s  {
      match s with
      | "let"    -> LET
      | "var"    -> VAR
      | "return" -> RETURN
      | "if"     -> IF
      | "elseif" -> ELSEIF
      | "else"   -> ELSE
      | "while"  -> WHILE
      | "for"    -> FOR
      | "in"     -> IN
      | "true"     -> TRUE
      | "false"    -> FALSE
      | "break"    -> BREAK
      | "continue" -> CONTINUE
      | "as"       -> AS
      | "sizeof"   -> SIZEOF
      | "null"     -> NULL
      | "extern"   -> EXTERN
      | "struct"   -> STRUCT
      | _          -> IDENT s
    }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<<" { LSHIFT }
  | ">>" { RSHIFT }
  | '<'  { LT }
  | '>'  { GT }
  | "&&" { AND }
  | "||" { OR }
  | "++" { INCR }
  | "--" { DECR }
  | "+=" { PLUS_ASSIGN }
  | "-=" { MINUS_ASSIGN }
  | "*=" { STAR_ASSIGN }
  | "/=" { SLASH_ASSIGN }
  | '!'  { BANG }
  | '+'  { PLUS }
  | '-'  { MINUS }
  | '*'  { STAR }
  | '/'  { SLASH }
  | '%'  { PERCENT }
  | '&'  { AMP }
  | '|'  { PIPE }
  | '~'  { TILDE }
  | ".." { DOTDOT }
  | '.'  { DOT }
  | ';'  { SEMI }
  | '='  { ASSIGN }
  | '('  { LPAREN }
  | ')'  { RPAREN }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ':'  { COLON }
  | ','  { COMMA }
  | '^'  { CARET }
  | '@'  { AT }
  | '"'  { Buffer.clear buf; read_string lexbuf }
  | eof  { EOF }
  | _        { raise (SyntaxError ("line "
               ^ string_of_int lexbuf.Lexing.lex_curr_p.Lexing.pos_lnum
               ^ ": unexpected character: "
               ^ Lexing.lexeme lexbuf)) }

(* TODO: pass buffer as param instead of global and handle illegal escape *)
and read_string = parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string lexbuf }
  | '\\' '"'      { Buffer.add_char buf '"';  read_string lexbuf }
  | [^ '"' '\\']+  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string lexbuf }
  | eof           { raise (SyntaxError "unterminated string") }
