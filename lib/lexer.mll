(* SPDX-License-Identifier: Apache-2.0 *)

{
open Tokens

(* The state stays local to each lex session *)
type state = {
  base : int;
  buf : Buffer.t;
  token_queue : (Tokens.token * Span.t * int) Queue.t;
  mutable last_token : Tokens.token;
  mutable line : int;
  mutable token_line : int;
  mutable string_resume : (int * int) option;
}

let make_state base = {
  base;
  buf = Buffer.create 64;
  token_queue = Queue.create ();
  last_token = EOF;
  line = 1;
  token_line = 1;
  string_resume = None;
}

let next_line st = st.line <- st.line + 1

let max_intsuf_len = String.length "isize"
let floatsuf_len = String.length "f32"

(* The line tracker avoids per token positions *)
let lexbuf_of_string src =
  let lexbuf = Lexing.from_string src in
  lexbuf.Lexing.lex_curr_p <- Lexing.dummy_pos;
  lexbuf

let start_pos lexbuf = lexbuf.Lexing.lex_start_pos
let end_pos lexbuf = lexbuf.Lexing.lex_curr_pos
let lexbuf_span st lexbuf =
  Span.make (st.base + start_pos lexbuf) (st.base + end_pos lexbuf)

let int_token _st _lexbuf ?suf text =
  match Int64.of_string_opt text with
  | Some v -> INT (v, suf)
  | None -> ERROR "integer literal out of range"

(* The suffix parser stays OUT of the lexer rule *)
let split_int_suffix text =
  let start = max 0 (String.length text - max_intsuf_len) in
  let is_suffix_start = function 'i' | 'u' -> true | _ -> false in
  match String.find_first_index is_suffix_start ~start text with
  | None -> (text, None)
  | Some i ->
      let body, suffix = String.cut_first i text in
      (body, Some suffix)

let radix_int_token st lexbuf text =
  let body, suf = split_int_suffix text in
  int_token st lexbuf ?suf body

let decimal_int_token st lexbuf text =
  let body, suf = split_int_suffix text in
  int_token st lexbuf ?suf ("0u" ^ body)

let float_token text =
  let len = String.length text in
  let start = len - floatsuf_len in
  if start > 0 && text.[start] = 'f' then
    let body, suffix = String.cut_first start text in
    FLOAT (float_of_string body, Some suffix)
  else FLOAT (float_of_string text, None)

let bad_char _st _lexbuf msg = ERROR msg

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
  | RPAREN | RBRACE | RBRACKET | UNDERSCORE -> true
  | _ -> false
}

let digit   = ['0'-'9']
let hexdig  = ['0'-'9' 'a'-'f' 'A'-'F']
let bindig  = ['0'-'1']
let octdig  = ['0'-'7']
let decimals = digit (digit | '_')*
let hexdigs = hexdig (hexdig | '_')*
let bindigs = bindig (bindig | '_')*
let octdigs = octdig (octdig | '_')*
let exp     = ['e' 'E'] ['+' '-']? decimals
let alpha   = ['a'-'z' 'A'-'Z' '_']
let alnum   = alpha | digit
let intsuf  = ('i' | 'u') ("8" | "16" | "32" | "64" | "size")
let floatsuf = 'f' ("32" | "64")
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_main st = parse
  | "\xEF\xBB\xBF" {
      if start_pos lexbuf = 0 then read_main st lexbuf
      else ERROR "unexpected character"
    }
  | white { read_main st lexbuf }
  | "//" [^ '\n' '\r']* { read_main st lexbuf }
  | "/*" { read_block_comment st 0 false lexbuf }
  | newline {
      next_line st;
      if can_end_stmt st.last_token then AUTOSEMI
      else read_main st lexbuf
    }
  | ('0' ['x' 'X'] hexdigs intsuf?) as n { radix_int_token st lexbuf n }
  | ('0' ['b' 'B'] bindigs intsuf?) as n { radix_int_token st lexbuf n }
  | ('0' ['o' 'O'] octdigs intsuf?) as n { radix_int_token st lexbuf n }
  | '0' ['x' 'X' 'b' 'B' 'o' 'O'] alnum*
      { ERROR "invalid number literal" }
  | decimals '.' decimals exp? floatsuf? as f { float_token f }
  | decimals exp floatsuf? as f { float_token f }
  | decimals floatsuf as f { float_token f }
  | (decimals intsuf?) as n { decimal_int_token st lexbuf n }
  | '_' { UNDERSCORE }
  | alpha alnum* as s {
      match lookup_keyword s with
      | Some t -> t
      | None -> IDENT s
    }
  | "==" { EQ }
  | "=>" { FATARROW }
  | "!=" { NEQ }
  | "<=" { LTE }
  | ">=" { GTE }
  | "<<=" { LSHIFT_ASSIGN }
  | ">>=" { RSHIFT_ASSIGN }
  | "<<" { LSHIFT }
  | ">>" { RSHIFT }
  | '<' { LT }
  | '>' { GT }
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
  | '!' { BANG }
  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { STAR }
  | '/' { SLASH }
  | '%' { PERCENT }
  | '&' { AMP }
  | '|' { PIPE }
  | '~' { TILDE }
  | "..." { ELLIPSIS }
  | "..=" { DOTDOTEQ }
  | ".." { DOTDOT }
  | '.' { DOT }
  | ';' { SEMI }
  | '=' { ASSIGN }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | '{' { LBRACE }
  | '}' { RBRACE }
  | ':' { COLON }
  | ',' { COMMA }
  | '^' { CARET }
  | "'\\0'" { CHAR 0 }
  | "'\\n'" { CHAR (Char.code '\n') }
  | "'\\r'" { CHAR (Char.code '\r') }
  | "'\\t'" { CHAR (Char.code '\t') }
  | "'\\\\'" { CHAR (Char.code '\\') }
  | "'\\''" { CHAR (Char.code '\'') }
  | '\'' '\\' newline '\''  {
      next_line st;
      bad_char st lexbuf ("unknown escape: " ^ Lexing.lexeme lexbuf)
    }
  | '\'' '\\' _ '\''  {
      bad_char st lexbuf ("unknown escape: " ^ Lexing.lexeme lexbuf)
    }
  | '\'' [^ '\'' '\\' '\r' '\n']+ '\''  {
      let inner =
        Lexing.sub_lexeme lexbuf
          (lexbuf.Lexing.lex_start_pos + 1)
          (lexbuf.Lexing.lex_curr_pos - 1)
      in
      char_token st lexbuf inner
    }
  | "''" { bad_char st lexbuf "empty character literal" }
  | '\'' { bad_char st lexbuf "unterminated character literal" }
  | '"' {
      let str_start = lexbuf.Lexing.lex_start_pos in
      let str_line = st.line in
      Buffer.clear st.buf;
      st.string_resume <- None;
      let tok = read_string st lexbuf in
      (* The span includes quotes *)
      lexbuf.Lexing.lex_start_pos <- str_start;
      st.token_line <- str_line;
      tok
    }
  | eof {
      if can_end_stmt st.last_token then AUTOSEMI else EOF
    }
  | _ { ERROR "unexpected character" }


and read_string st = parse
  | '"' {
      let s = Buffer.contents st.buf in
      Buffer.clear st.buf;
      STRING s
    }
  | '\\' 'n' { Buffer.add_char st.buf '\n'; read_string st lexbuf }
  | '\\' 'r' { Buffer.add_char st.buf '\r'; read_string st lexbuf }
  | '\\' 't' { Buffer.add_char st.buf '\t'; read_string st lexbuf }
  | '\\' '\\' { Buffer.add_char st.buf '\\'; read_string st lexbuf }
  | '\\' '"' { Buffer.add_char st.buf '"'; read_string st lexbuf }
  (* The lexer continues until the string closes *)
  | '\\' _        {
      let span =
        Span.make (st.base + start_pos lexbuf + 1) (st.base + end_pos lexbuf)
      in
      Queue.push
        (ERROR "unknown escape", span, st.line)
        st.token_queue;
      read_string st lexbuf
    }
  (* FIXME(2151): raw newlines stay for now *)
  | newline {
      if st.string_resume = None then
        st.string_resume <- Some (start_pos lexbuf, st.line);
      next_line st;
      Buffer.add_string st.buf (Lexing.lexeme lexbuf);
      read_string st lexbuf
    }
  | [^ '"' '\\' '\r' '\n']+  {
      Buffer.add_string st.buf (Lexing.lexeme lexbuf);
      read_string st lexbuf
    }
  (* A quote that never closed only ever meant the line it was opened on *)
  | eof {
      Buffer.clear st.buf;
      (match st.string_resume with
       | Some (pos, line) ->
           lexbuf.Lexing.lex_curr_pos <- pos;
           st.line <- line
       | None -> ());
      ERROR "unterminated string"
    }

and read_block_comment st depth saw_newline = parse
  | "/*" { read_block_comment st (depth + 1) saw_newline lexbuf }
  | "*/"    {
      if depth = 0 then
        if saw_newline && can_end_stmt st.last_token then AUTOSEMI
        else read_main st lexbuf
      else read_block_comment st (depth - 1) saw_newline lexbuf
    }
  | newline {
      next_line st;
      read_block_comment st depth true lexbuf
    }
  | eof { ERROR "unterminated block comment" }
  | _ { read_block_comment st depth saw_newline lexbuf }

{
let read st lexbuf =
  let tok, span, line =
    if not (Queue.is_empty st.token_queue) then Queue.pop st.token_queue
    else begin
      st.token_line <- 0;
      let t = read_main st lexbuf in
      (* A token gets its own line only when it spans lines *)
      let line = if st.token_line = 0 then st.line else st.token_line in
      (t, lexbuf_span st lexbuf, line)
    end
  in
  st.last_token <- tok;
  (tok, span, line)
}
