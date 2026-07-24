(* SPDX-License-Identifier: GPL-2.0-only *)

{
open Tokens

let next_line lexbuf =
  Lexing.new_line lexbuf

(* Per lex session state so multiple lexbufs can be live at once *)
type state = {
  buf : Buffer.t; (* Auto buffer resize *)
  (* FIXME(641c): fixes multiline expressions in () but
    new error w/ newlines forever on unclosed parens *)
  mutable paren_depth : int;
  token_queue : (Tokens.token * Span.t) Queue.t;
  (* Trying to emulate go semicolons *)
  mutable last_token : Tokens.token option;
}

let make_state () = {
  buf = Buffer.create 64;
  paren_depth = 0;
  token_queue = Queue.create ();
  last_token = None;
}

let start_pos lexbuf = lexbuf.Lexing.lex_start_p.Lexing.pos_cnum
let end_pos lexbuf = lexbuf.Lexing.lex_curr_p.Lexing.pos_cnum
let lexbuf_span lexbuf = { Span.lo = start_pos lexbuf; hi = end_pos lexbuf }

let int_token st lexbuf ?suf text =
  match Int64.of_string_opt text with
  | Some v -> INT (v, suf)
  | None ->
      (* The zero keeps the parser from raising a second error *)
      Queue.push (INT (0L, suf), lexbuf_span lexbuf) st.token_queue;
      ERROR "integer literal out of range"

(* The replacement char keeps the parser from raising a second error *)
let bad_char st lexbuf msg =
  Queue.push (CHAR 0, lexbuf_span lexbuf) st.token_queue;
  ERROR msg

let char_token st lexbuf inner =
  let d = String.get_utf_8_uchar inner 0 in
  if not (Uchar.utf_decode_is_valid d) then
    bad_char st lexbuf "invalid character literal"
  else if Uchar.utf_decode_length d <> String.length inner then
    bad_char st lexbuf "character literal must be a single character"
  else CHAR (Uchar.to_int (Uchar.utf_decode_uchar d))

let can_end_stmt = function
  | IDENT _ | INT _ | FLOAT _ | STRING _ | CHAR _
  | TRUE | FALSE | NULL | UNDEFINED
  | BREAK | CONTINUE | RETURN
  | RPAREN | RBRACE | RBRACKET -> true
  | _ -> false
}

let digit   = ['0'-'9']
let hexdig  = ['0'-'9' 'a'-'f' 'A'-'F']
let bindig  = ['0'-'1']
let octdig  = ['0'-'7']
let exp     = ['e' 'E'] ['+' '-']? digit+
let alpha   = ['a'-'z' 'A'-'Z' '_']
let alnum   = alpha | digit
let intsuf  = ('i' | 'u') ("8" | "16" | "32" | "64" | "size")
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_main st = parse
  | white              { read_main st lexbuf }
  | "//" [^ '\n' '\r']* { read_main st lexbuf }
  | "/*"               { read_block_comment st 0 lexbuf }
  | newline            { next_line lexbuf;
                         if st.paren_depth > 0 then read_main st lexbuf
                         else match st.last_token with
                              | Some t when can_end_stmt t -> SEMI
                              | _ -> read_main st lexbuf }
  | ('0' ['x' 'X'] hexdig+ as n) (intsuf as suf)  { int_token st lexbuf ~suf n }
  | ('0' ['b' 'B'] bindig+ as n) (intsuf as suf)  { int_token st lexbuf ~suf n }
  | ('0' ['o' 'O'] octdig+ as n) (intsuf as suf)  { int_token st lexbuf ~suf n }
  | (digit+ as n) (intsuf as suf)  { int_token st lexbuf ~suf ("0u" ^ n) }
  | ('0' ['x' 'X'] hexdig+) as n  { int_token st lexbuf n }
  | ('0' ['b' 'B'] bindig+) as n  { int_token st lexbuf n }
  | ('0' ['o' 'O'] octdig+) as n  { int_token st lexbuf n }
  | '0' ['x' 'X' 'b' 'B' 'o' 'O'] alnum* as n
      { ERROR ("invalid number literal: " ^ n) }
  | digit+ '.' digit+ exp? as f { FLOAT (float_of_string f) }
  | digit+ exp as f    { FLOAT (float_of_string f) }
  | digit+ as n        { int_token st lexbuf ("0u" ^ n) }
  | alpha alnum* as s  {
      match lookup_keyword s with
      | Some t -> t
      | None -> IDENT s
    }
  | "==" { EQ }
  | "!=" { NEQ }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<<=" { LSHIFT_ASSIGN }
  | ">>=" { RSHIFT_ASSIGN }
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
  | "%=" { PERCENT_ASSIGN }
  | "&=" { AMP_ASSIGN }
  | "|=" { PIPE_ASSIGN }
  | "^=" { CARET_ASSIGN }
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
  (* Brackets bump paren_depth so array literals can span lines *)
  | '['  { st.paren_depth <- st.paren_depth + 1; LBRACKET }
  | ']'  { st.paren_depth <- max 0 (st.paren_depth - 1); RBRACKET }
  | '{'  { LBRACE }
  | '}'  { RBRACE }
  | ':'  { COLON }
  | ','  { COMMA }
  | '^'  { CARET }
  | "'\\0'"  { CHAR 0 }
  | "'\\n'"  { CHAR (Char.code '\n') }
  | "'\\t'"  { CHAR (Char.code '\t') }
  | "'\\\\'" { CHAR (Char.code '\\') }
  | "'\\''"  { CHAR (Char.code '\'') }
  | '\'' '\\' _ '\''  { bad_char st lexbuf ("unknown escape: " ^ Lexing.lexeme lexbuf) }
  | '\'' ([^ '\'' '\\' '\r' '\n']+ as inner) '\''  { char_token st lexbuf inner }
  | "''"  { bad_char st lexbuf "empty character literal" }
  | '\''  { bad_char st lexbuf "unterminated character literal" }
  | '"'  { let str_start = lexbuf.Lexing.lex_start_p in
           Buffer.clear st.buf;
           let tok = read_string st lexbuf in
           (* The string token spans the whole literal with quotes included *)
           lexbuf.Lexing.lex_start_p <- str_start;
           tok }
  | eof  { EOF }
  | _    { ERROR ("unexpected character: " ^ Lexing.lexeme lexbuf) }


and read_string st = parse
  | '"'  { let s = Buffer.contents st.buf in
           Buffer.clear st.buf;
           STRING s }
  | '\\' 'n'      { Buffer.add_char st.buf '\n'; read_string st lexbuf }
  | '\\' 't'      { Buffer.add_char st.buf '\t'; read_string st lexbuf }
  | '\\' '\\'     { Buffer.add_char st.buf '\\'; read_string st lexbuf }
  | '\\' '"'      { Buffer.add_char st.buf '"';  read_string st lexbuf }
  (* Skip the bad escape and keep lexing so the string still closes *)
  | '\\' _        { let span = { Span.lo = start_pos lexbuf + 1; hi = end_pos lexbuf } in
                    Queue.push
                      (ERROR ("unknown escape: " ^ Lexing.lexeme lexbuf), span)
                      st.token_queue;
                    read_string st lexbuf }
  (* FIXME(2151): allow raw newlines for now, revisit them in the future *)
  | newline { next_line lexbuf; Buffer.add_string st.buf (Lexing.lexeme lexbuf); read_string st lexbuf }
  | [^ '"' '\\' '\r' '\n']+  { Buffer.add_string st.buf (Lexing.lexeme lexbuf); read_string st lexbuf }
  (* Recover so the parser sees a closed string plus an error *)
  | eof  { let s = Buffer.contents st.buf in
           Buffer.clear st.buf;
           let here = end_pos lexbuf in
           Queue.push
             (ERROR "unterminated string", { Span.lo = here; hi = here })
             st.token_queue;
           STRING s }

and read_block_comment st depth = parse
  | "/*"    { read_block_comment st (depth + 1) lexbuf }
  | "*/"    { if depth = 0 then read_main st lexbuf
              else read_block_comment st (depth - 1) lexbuf }
  | newline { next_line lexbuf; read_block_comment st depth lexbuf }
  | eof     { ERROR "unterminated block comment" }
  | _       { read_block_comment st depth lexbuf }

{
let read st lexbuf =
  let tok, span =
    if not (Queue.is_empty st.token_queue) then Queue.pop st.token_queue
    else
      let t = read_main st lexbuf in
      (t, lexbuf_span lexbuf)
  in
  st.last_token <- Some tok;
  (tok, span)
}
