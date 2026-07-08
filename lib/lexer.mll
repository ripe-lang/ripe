(* SPDX-License-Identifier: GPL-2.0-only *)

{
open Tokens

let next_line lexbuf =
  Lexing.new_line lexbuf

(* per lex session state so multiple lexbufs can be live at once *)
type state = {
  buf : Buffer.t; (* auto buffer resize *)
  (* FIXME: fixes multiline expressions in () but
    new error w/ newlines forever on unclosed parens *)
  mutable paren_depth : int;
  token_queue : Tokens.token Queue.t;
  (* trying to emulate go semicolons *)
  mutable last_token : Tokens.token option;
}

let make_state () = {
  buf = Buffer.create 64;
  paren_depth = 0;
  token_queue = Queue.create ();
  last_token = None;
}

let can_end_stmt = function
  | IDENT _ | INT _ | FLOAT _ | STRING _
  | TRUE | FALSE | NULL | UNDEFINED
  | BREAK | CONTINUE | RETURN
  | RPAREN | RBRACE | RBRACKET -> true
  | _ -> false
}

let digit   = ['0'-'9']
let alpha   = ['a'-'z' 'A'-'Z' '_']
let alnum   = alpha | digit
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read st = parse
  | "" { let tok =
           if not (Queue.is_empty st.token_queue) then
             Queue.pop st.token_queue
           else
             read_main st lexbuf
         in
         st.last_token <- Some tok;
         tok }

and read_main st = parse
  | white              { read st lexbuf }
  | '#' [^ '\n' '\r']* { read st lexbuf }
  | newline            { next_line lexbuf;
                         if st.paren_depth > 0 then read st lexbuf
                         else match st.last_token with
                              | Some t when can_end_stmt t -> SEMI
                              | _ -> read st lexbuf }
  | digit+ '.' digit+  as f { FLOAT (float_of_string f) }
  | digit+ as n        { match Int64.of_string_opt ("0u" ^ n) with
                         | Some v -> INT v
                         | None ->
                             (* the zero keeps the parser from raising a second error *)
                             Queue.push (INT 0L) st.token_queue;
                             ERROR "integer literal out of range" }
  | alpha alnum* as s  {
      match lookup_keyword s with
      | Some t -> t
      | None -> IDENT s
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
  | "..." { ELLIPSIS }
  | "..=" { DOTDOTEQ }
  | ".." { DOTDOT }
  | '.'  { DOT }
  | ';'  { SEMI }
  | '='  { ASSIGN }
  | '('  { st.paren_depth <- st.paren_depth + 1; LPAREN }
  | ')'  { st.paren_depth <- max 0 (st.paren_depth - 1); RPAREN }
  (* brackets bump paren_depth so array literals can span lines *)
  | '['  { st.paren_depth <- st.paren_depth + 1; LBRACKET }
  | ']'  { st.paren_depth <- max 0 (st.paren_depth - 1); RBRACKET }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ':'  { COLON }
  | ','  { COMMA }
  | '^'  { CARET }
  (* TODO: char literal *)
  | '"'  { Buffer.clear st.buf; read_string st lexbuf }
  | eof  { EOF }
  | _    { ERROR ("unexpected character: " ^ Lexing.lexeme lexbuf) }


and read_string st = parse
  | '"'  { let s = Buffer.contents st.buf in
           Buffer.clear st.buf;
           Queue.push (STRING s) st.token_queue;
           Queue.pop st.token_queue }
  | '\\' 'n'      { Buffer.add_char st.buf '\n'; read_string st lexbuf }
  | '\\' 't'      { Buffer.add_char st.buf '\t'; read_string st lexbuf }
  | '\\' '\\'     { Buffer.add_char st.buf '\\'; read_string st lexbuf }
  | '\\' '"'      { Buffer.add_char st.buf '"';  read_string st lexbuf }
  (* skip the bad escape and keep lexing so the string still closes *)
  | '\\' _        { Queue.push (ERROR ("unknown escape: " ^ Lexing.lexeme lexbuf)) st.token_queue;
                    read_string st lexbuf }
  (* FIXME allow raw newlines for now, revisit them in the future *)
  | newline { next_line lexbuf; Buffer.add_string st.buf (Lexing.lexeme lexbuf); read_string st lexbuf }
  | [^ '"' '\\' '\r' '\n']+  { Buffer.add_string st.buf (Lexing.lexeme lexbuf); read_string st lexbuf }
  (* recover so the parser sees a closed string plus an error *)
  | eof  { let s = Buffer.contents st.buf in
           Buffer.clear st.buf;
           Queue.push (STRING s) st.token_queue;
           Queue.push (ERROR "unterminated string") st.token_queue;
           Queue.pop st.token_queue }
