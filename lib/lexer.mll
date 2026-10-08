(* SPDX-License-Identifier: Apache-2.0 *)

{
open Tokens

(* The state stays local to each lex session *)
type state = {
  base : int;
  strbuf : Buffer.t;
}

let make_state base = {
  base;
  strbuf = Buffer.create 64;
}

let max_intsuf_len = String.length "isize"
let max_floatsuf_len = String.length "f32"

(* Spans are plain offsets so the lexbuf needs no positions *)
let lexbuf_of_string src =
  Lexing.from_string ~with_positions:false src

let lexbuf_span st lexbuf =
  Span.make
    (st.base + lexbuf.Lexing.lex_start_pos)
    (st.base + lexbuf.Lexing.lex_curr_pos)

let int_token ?suf text =
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

let radix_int_token text =
  let body, suf = split_int_suffix text in
  int_token ?suf body

(* The unsigned prefix lets literals past the signed max still parse *)
let decimal_int_token text =
  let body, suf = split_int_suffix text in
  int_token ?suf ("0u" ^ body)

let float_token text =
  let len = String.length text in
  let start = len - max_floatsuf_len in
  if start > 0 && text.[start] = 'f' then
    let body, suffix = String.cut_first start text in
    FLOAT (float_of_string body, Some suffix)
  else FLOAT (float_of_string text, None)

let char_token inner =
  let d = String.get_utf_8_uchar inner 0 in
  if not (Uchar.utf_decode_is_valid d) then
    ERROR "invalid character literal"
  else if Uchar.utf_decode_length d <> String.length inner then
    ERROR "character literal must be a single character"
  else CHAR (Uchar.to_int (Uchar.utf_decode_uchar d))
}

let decdig  = ['0'-'9']
let hexdig  = ['0'-'9' 'a'-'f' 'A'-'F']
let bindig  = ['0'-'1']
let octdig  = ['0'-'7']
let decdigs = decdig (decdig | '_')*
let hexdigs = hexdig (hexdig | '_')*
let bindigs = bindig (bindig | '_')*
let octdigs = octdig (octdig | '_')*
let exp     = ['e' 'E'] ['+' '-']? decdigs
let alpha   = ['a'-'z' 'A'-'Z' '_']
let alnum   = alpha | decdig
let intsuf  = ('i' | 'u') ("8" | "16" | "32" | "64" | "size")
let floatsuf = 'f' ("32" | "64")
let white   = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read_token st = parse
  | "\xEF\xBB\xBF" {
      if lexbuf.Lexing.lex_start_pos = 0 then read_token st lexbuf
      else ERROR "unexpected character"
    }
  | white { read_token st lexbuf }
  | "//" [^ '\n' '\r']* { read_token st lexbuf }
  | "/*" { read_block_comment st 0 lexbuf }
  | newline { read_token st lexbuf }
  | ('0' ['x' 'X'] hexdigs intsuf?) as n { radix_int_token n }
  | ('0' ['b' 'B'] bindigs intsuf?) as n { radix_int_token n }
  | ('0' ['o' 'O'] octdigs intsuf?) as n { radix_int_token n }
  | '0' ['x' 'X' 'b' 'B' 'o' 'O'] alnum*
      { ERROR "invalid number literal" }
  | decdigs '.' decdigs exp? floatsuf? as n { float_token n }
  | decdigs exp floatsuf? as n { float_token n }
  | decdigs floatsuf as n { float_token n }
  | (decdigs intsuf?) as n { decimal_int_token n }
  | '_' { UNDERSCORE }
  | alpha alnum* as n {
      match lookup_keyword n with
      | Some t -> t
      | None -> IDENT n
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
  (* TODO(ede5): The lexer should take \u{...} escapes in chars and strings *)
  | "'\\0'" { CHAR 0 }
  | "'\\n'" { CHAR (Char.code '\n') }
  | "'\\r'" { CHAR (Char.code '\r') }
  | "'\\t'" { CHAR (Char.code '\t') }
  | "'\\\\'" { CHAR (Char.code '\\') }
  | "'\\''" { CHAR (Char.code '\'') }
  | '\'' '\\' (newline | [^ '\r' '\n']) '\''  {
      ERROR ("unknown escape: " ^ Lexing.lexeme lexbuf)
    }
  | '\'' [^ '\'' '\\' '\r' '\n']+ '\''  {
      let inner =
        Lexing.sub_lexeme lexbuf
          (lexbuf.Lexing.lex_start_pos + 1)
          (lexbuf.Lexing.lex_curr_pos - 1)
      in
      char_token inner
    }
  | "''" { ERROR "empty character literal" }
  | '\'' ('\\' [^ '\r' '\n']?)? [^ '\'' '\\' '\r' '\n' ' ' '\t' '(' ')' '[' ']' '{' '}' ',' ';']* {
      ERROR "unterminated character literal"
    }
  | '"' {
      let start = lexbuf.Lexing.lex_start_pos in
      Buffer.clear st.strbuf;
      let t = read_string st lexbuf in
      (* The span includes quotes *)
      lexbuf.Lexing.lex_start_pos <- start;
      t
    }
  | eof { EOF }
  | _ { ERROR "unexpected character" }


and read_string st = parse
  | '"' { STRING (Buffer.contents st.strbuf) }
  | '\\' 'n' { Buffer.add_char st.strbuf '\n'; read_string st lexbuf }
  | '\\' 'r' { Buffer.add_char st.strbuf '\r'; read_string st lexbuf }
  | '\\' 't' { Buffer.add_char st.strbuf '\t'; read_string st lexbuf }
  | '\\' '\\' { Buffer.add_char st.strbuf '\\'; read_string st lexbuf }
  | '\\' '"' { Buffer.add_char st.strbuf '"'; read_string st lexbuf }
  | '\\' '0' { Buffer.add_char st.strbuf '\000'; read_string st lexbuf }
  (* The lexer continues until the string closes *)
  | '\\' [^ '\r' '\n'] {
      let span =
        Span.make
          (st.base + lexbuf.Lexing.lex_start_pos + 1)
          (st.base + lexbuf.Lexing.lex_curr_pos)
      in
      Diagnostic.emit (Diagnostic.error span "unknown escape");
      read_string st lexbuf
    }
  | '\\' { read_string st lexbuf }
  (* The newline goes back so the next line lexes as code *)
  | newline {
      lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_start_pos;
      ERROR "unterminated string"
    }
  | [^ '"' '\\' '\r' '\n']+  {
      Buffer.add_string st.strbuf (Lexing.lexeme lexbuf);
      read_string st lexbuf
    }
  | eof { ERROR "unterminated string" }

and read_block_comment st depth = parse
  | "/*" { read_block_comment st (depth + 1) lexbuf }
  | "*/"    {
      if depth = 0 then read_token st lexbuf
      else read_block_comment st (depth - 1) lexbuf
    }
  | eof { ERROR "unterminated block comment" }
  | _ { read_block_comment st depth lexbuf }

{
let read st lexbuf =
  let t = read_token st lexbuf in
  let span = lexbuf_span st lexbuf in
  begin match t with
  | ERROR msg -> Diagnostic.emit (Diagnostic.error span "%s" msg)
  | _ -> ()
  end;
  (t, span)
}
