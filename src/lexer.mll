{
open Token
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z' '_']
let alnum = alpha | digit
let whitespace = [' ' '\t' '\n' '\r']

rule tokenize = parse
  | whitespace+    { tokenize lexbuf }
  | digit+ as n    { INT (int_of_string n) }
  | alpha alnum* as s { IDENT s }
  | '+'            { PLUS }
  | '-'            { MINUS }
  | '*'            { STAR }
  | '/'            { SLASH }
  | '('            { LPAREN }
  | ')'            { RPAREN }
  | eof            { EOF }
  | _ as c         { failwith (Printf.sprintf "Unexpected character: %c" c) }
