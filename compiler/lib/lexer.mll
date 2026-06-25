(* SPDX-License-Identifier: GPL-2.0-only *)

{
open Tokens

let next_line lexbuf =
  Lexing.new_line lexbuf

let buf = Buffer.create 64 (* auto buffer resize *)

(* FIXME: fixes multiline expressions in () but
  new error w/ newlines forever on unclosed parens *)
let paren_depth = ref 0

(* string interpolation state *)
let in_string = ref false
let in_interp = ref false
let interp_brace_depth = ref 0
let token_queue : Tokens.token Queue.t = Queue.create ()

(* trying to emulate go semicolons *)
let last_token : Tokens.token option ref = ref None

let can_end_stmt = function
  | IDENT _ | INT _ | FLOAT _ | STRING_END
  | TRUE | FALSE | NULL
  | BREAK | CONTINUE | RETURN
  | RPAREN | RBRACE -> true
  | _ -> false

let reset () =
  paren_depth := 0;
  in_string := false;
  in_interp := false;
  interp_brace_depth := 0;
  Queue.clear token_queue;
  last_token := None
}

let digit   = ['0'-'9']
let alpha   = ['a'-'z' 'A'-'Z' '_']
let alnum   = alpha | digit
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | "" { let tok =
           if not (Queue.is_empty token_queue) then
             Queue.pop token_queue
           else if !in_string && not !in_interp then
             read_string lexbuf
           else
             read_main lexbuf
         in
         last_token := Some tok;
         tok }

and read_main = parse
  | white              { read lexbuf }
  | '#' [^ '\n' '\r']* { read lexbuf }
  | newline            { next_line lexbuf;
                         if !paren_depth > 0 then read lexbuf
                         else match !last_token with
                              | Some t when can_end_stmt t -> SEMI
                              | _ -> read lexbuf }
  | digit+ '.' digit+  as f { FLOAT (float_of_string f) }
  | digit+ as n        { INT (int_of_string n) }
  | alpha alnum* as s  {
      match s with
      | "const"  -> CONST
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
      (* FIXME: I don't know if I like "inline" something feels weird *)
      | "inline"   -> INLINE
      | "public"   -> PUBLIC
      | "func"     -> FUNC
      | "type"     -> TYPE
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
  | '('  { incr paren_depth; LPAREN }
  | ')'  { decr paren_depth; RPAREN }
  | '{'  { if !in_interp then incr interp_brace_depth;
           LBRACE }
  | '}'  { if !in_interp && !interp_brace_depth = 0 then begin
             in_interp := false;
             INTERP_END
           end else begin
             if !in_interp then decr interp_brace_depth;
             RBRACE
           end }
  | ':'  { COLON }
  | ','  { COMMA }
  | '^'  { CARET }
  (* TODO: char literal *)
  | '"'  { Buffer.clear buf;
           in_string := true;
           Queue.push STRING_START token_queue;
           read_string lexbuf }
  | eof  { EOF }
  | _    { ERROR ("unexpected character: " ^ Lexing.lexeme lexbuf) }


(* TODO: pass buffer as param instead of global and handle illegal escape *)
and read_string = parse
  | '"'  { if Buffer.length buf > 0 then begin
             Queue.push (STRING_PART (Buffer.contents buf)) token_queue;
             Buffer.clear buf
           end;
           in_string := false;
           Queue.push STRING_END token_queue;
           Queue.pop token_queue }
  | "{{" { Buffer.add_char buf '{'; read_string lexbuf }
  | "}}" { Buffer.add_char buf '}'; read_string lexbuf }
  | '{'  { if Buffer.length buf > 0 then begin
              Queue.push (STRING_PART (Buffer.contents buf)) token_queue;
              Buffer.clear buf
            end;
            Queue.push INTERP_START token_queue;
            in_interp := true;
            interp_brace_depth := 0;
            Queue.pop token_queue }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string lexbuf }
  | '\\' '"'      { Buffer.add_char buf '"';  read_string lexbuf }
  | [^ '"' '\\' '{' '}']+  { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string lexbuf }
  (* recover so the parser sees a closed string plus an error *)
  | eof  { if Buffer.length buf > 0 then begin
             Queue.push (STRING_PART (Buffer.contents buf)) token_queue;
             Buffer.clear buf
           end;
           in_string := false;
           in_interp := false;
           interp_brace_depth := 0;
           Queue.push STRING_END token_queue;
           Queue.push (ERROR "unterminated string") token_queue;
           Queue.pop token_queue }
