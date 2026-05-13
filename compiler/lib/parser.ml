(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens
open Ast

exception ParseError of string

type state = {
  mutable tok : token;
  lexbuf : Lexing.lexbuf;
  read : Lexing.lexbuf -> token;
}

let advance st = st.tok <- st.read st.lexbuf
let at st t = st.tok = t

(* newlines lex as SEMI *)
let skip_semi st =
  while st.tok = SEMI do
    advance st
  done

let expect_ident st =
  match st.tok with
  | IDENT s ->
      advance st;
      s
  | _ -> raise (ParseError "Expected identifier")

let expect st t =
  if st.tok <> t then
    raise
      (ParseError
         (Printf.sprintf "expected %s"
            (match t with
            | LBRACE -> "'{'"
            | RBRACE -> "'}'"
            | LPAREN -> "'('"
            | RPAREN -> "')'"
            | COLON -> "':'"
            | COMMA -> "','"
            | SEMI -> "';'"
            | ASSIGN -> "'='"
            | IN -> "'in'"
            | _ -> "token")))
  else advance st

(* i32 *)
let rec parse_typ st =
  match st.tok with
  | IDENT name ->
      advance st;
      Named name
  | _ -> raise (ParseError "expected type")

(* x: i32 *)
let parse_fields st =
  let fields = ref [] in
  while st.tok <> RBRACE do
    (* TODO(9ee0): parse modifiers *)
    let name = expect_ident st in
    expect st COLON;
    let t = parse_typ st in
    fields := ({ name; typ = t; modifiers = [] } : field) :: !fields;
    if st.tok = COMMA then advance st;
    skip_semi st
  done;
  List.rev !fields

(* struct point { x: i32, y: i32 } *)
let parse_struct st mods =
  advance st;
  (* STRUCT *)
  let name = expect_ident st in
  skip_semi st;
  expect st LBRACE;
  let fields = parse_fields st in
  expect st RBRACE;
  skip_semi st;
  Struct { name; fields; modifiers = mods }

(* (a: i32, b: i32) *)
let parse_params st =
  expect st LPAREN;
  let params = ref [] in
  if st.tok <> RPAREN then begin
    let name = expect_ident st in
    expect st COLON;
    let t = parse_typ st in
    params := [ ({ name; typ = t } : param) ];
    while st.tok = COMMA do
      advance st;
      let name = expect_ident st in
      expect st COLON;
      let t = parse_typ st in
      params := ({ name; typ = t } : param) :: !params
    done
  end;
  expect st RPAREN;
  List.rev !params

(* : i32 *)
let parse_ret_type st =
  if at st COLON then begin
    advance st;
    if st.tok = LPAREN || st.tok = SEMI then None else Some (parse_typ st)
  end
  else None

(* { return a + b } *)
let rec parse_block st =
  expect st LBRACE;
  let body = parse_stmts st in
  expect st RBRACE;
  skip_semi st;
  body

and parse_stmts st =
  let stmts = ref [] in
  skip_semi st;
  while st.tok <> RBRACE && st.tok <> EOF do
    let s = parse_stmt st in
    stmts := s :: !stmts;
    skip_semi st
  done;
  List.rev !stmts

and parse_stmt st = raise (ParseError "TODO: parse_stmt not yet implemented")

(* add(a: i32, b: i32): i32 { return a + b } *)
let parse_func st mods =
  let name = expect_ident st in
  let params = parse_params st in
  let ret = parse_ret_type st in
  skip_semi st;
  let body = parse_block st in
  Func { name; params; ret; body; modifiers = mods }

(* extern add(a: i32, b: i32): i32 *)
(* No modifiers needed *)
let parse_extern st =
  advance st;
  (* EXTERN *)
  let name = expect_ident st in
  let params = parse_params st in
  let ret = parse_ret_type st in
  skip_semi st;
  Extern { name; params; ret; body = []; modifiers = [] }

let parse_decl st =
  match st.tok with
  | EXTERN -> parse_extern st
  | STRUCT -> parse_struct st []
  | _ -> parse_func st []

let parse_program st =
  let decls = ref [] in
  skip_semi st;
  while st.tok <> EOF do
    decls := parse_decl st :: !decls (* skip_semi st *)
  done;
  List.rev !decls

let parse (read : Lexing.lexbuf -> Tokens.token) (lexbuf : Lexing.lexbuf) :
    Ast.decl list =
  let st = { tok = read lexbuf; lexbuf; read } in
  parse_program st
