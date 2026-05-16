(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens
open Ast

exception ParseError of Lexing.position * string

type state = {
  mutable tok : token;
  lexbuf : Lexing.lexbuf;
  read : Lexing.lexbuf -> token;
}

type assoc = Left | Right | NonAssoc

let advance st = st.tok <- st.read st.lexbuf
let at st t = st.tok = t

(* start of the current lookahead token *)
let cur_pos st = st.lexbuf.Lexing.lex_start_p.pos_cnum
let cur_lex_pos st = st.lexbuf.Lexing.lex_start_p

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
  | _ -> raise (ParseError (cur_lex_pos st, "expected identifier"))

let expect st t =
  if st.tok <> t then
    raise
      (ParseError
         ( cur_lex_pos st,
           Printf.sprintf "expected %s"
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
             | _ -> "token") ))
  else advance st

let mk lo st desc =
  let hi = cur_pos st in
  { desc; span = { lo; hi } }

let mks lo st sdesc =
  let hi = cur_pos st in
  { sdesc; span = { lo; hi } }

let mkt lo st tdesc =
  let hi = cur_pos st in
  { tdesc; span = { lo; hi } }

(* i32 *)
let rec parse_typ st =
  let lo = cur_pos st in
  match st.tok with
  | CARET ->
      advance st;
      mkt lo st (Pointer (parse_typ st))
  | IDENT name ->
      advance st;
      mkt lo st (Named name)
  | _ -> raise (ParseError (cur_lex_pos st, "expected type"))

let parse_modifiers st =
  let mods = ref [] in
  let cont = ref true in
  while !cont do
    match st.tok with
    | INLINE ->
        advance st;
        mods := Ast.Inline :: !mods
    (* TODO(74d8): not entirely sure yet. static? public? *)
    | PUBLIC ->
        advance st;
        mods := Ast.Pub :: !mods
    | _ -> cont := false
  done;
  List.rev !mods

(* x: i32 *)
let parse_fields st =
  let fields = ref [] in
  while st.tok <> RBRACE do
    (* TODO(9ee0): parse modifiers *)
    (* let mods = parse_modifiers st in *)
    let lo = cur_pos st in
    let name = expect_ident st in
    expect st COLON;
    let t = parse_typ st in
    let hi = cur_pos st in
    (* Replace modifiers with modifiers = mods *)
    fields :=
      ({ name; typ = t; modifiers = []; span = { lo; hi } } : field) :: !fields;
    if st.tok = COMMA then advance st;
    skip_semi st
  done;
  List.rev !fields

(* struct point { x: i32, y: i32 } *)
let parse_struct st mods =
  let lo = cur_pos st in
  advance st;
  (* STRUCT *)
  let name = expect_ident st in
  skip_semi st;
  expect st LBRACE;
  let fields = parse_fields st in
  expect st RBRACE;
  let hi = cur_pos st in
  skip_semi st;
  Struct { name; fields; modifiers = mods; span = { lo; hi } }

(* (a: i32, b: i32) *)
let parse_params st =
  expect st LPAREN;
  let params = ref [] in
  let parse_one () =
    let lo = cur_pos st in
    let name = expect_ident st in
    expect st COLON;
    let t = parse_typ st in
    let hi = cur_pos st in
    ({ name; typ = t; span = { lo; hi } } : param)
  in
  if st.tok <> RPAREN then begin
    params := [ parse_one () ];
    while st.tok = COMMA do
      advance st;
      params := parse_one () :: !params
    done
  end;
  expect st RPAREN;
  List.rev !params

(* : i32 *)
let parse_ret_type st =
  if at st COLON then begin
    advance st;
    if st.tok = LBRACE || st.tok = SEMI then None else Some (parse_typ st)
  end
  else None

(* Postfix binds tighter than any infix: a.b + c means (a.b) + c. *)

let prec_of = function
  | ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | STAR_ASSIGN | SLASH_ASSIGN ->
      Some (1, Right)
  | DOTDOT | DOTDOTEQ -> Some (2, NonAssoc)
  | OR -> Some (3, Left)
  | AND -> Some (4, Left)
  | EQ | NEQ | LT | GT | LTE | GTE -> Some (5, NonAssoc)
  | PIPE -> Some (6, Left)
  | AMP -> Some (7, Left)
  | LSHIFT | RSHIFT -> Some (8, Left)
  | PLUS | MINUS -> Some (9, Left)
  | STAR | SLASH | PERCENT -> Some (10, Left)
  | AS -> Some (11, Left)
  (* TODO(4893): add XOR once symbol is decided *)
  | _ -> None

let binop_of = function
  | PLUS -> Add
  | MINUS -> Sub
  | STAR -> Mul
  | SLASH -> Div
  | PERCENT -> Mod
  | EQ -> Eq
  | NEQ -> Neq
  | LT -> Lt
  | GT -> Gt
  | LTE -> Lte
  | GTE -> Gte
  | AND -> And
  | OR -> Or
  | AMP -> BitAnd
  | PIPE -> BitOr
  | TILDE -> BitXor (* TODO add XOR once symbol is decided *)
  | LSHIFT -> Lshift
  | RSHIFT -> Rshift
  | ASSIGN -> Assign
  | PLUS_ASSIGN -> AddAssign
  | MINUS_ASSIGN -> SubAssign
  | STAR_ASSIGN -> MulAssign
  | SLASH_ASSIGN -> DivAssign
  | _ -> failwith "not a binary operator"

let rec parse_expr st min_prec =
  let lo = cur_pos st in
  let lhs = ref (parse_prefix st) in
  lhs := parse_postfix st !lhs;

  (* precedence climbing for infix ops *)
  let loop = ref true in
  while !loop do
    match prec_of st.tok with
    | None -> loop := false
    | Some (prec, _) when prec < min_prec -> loop := false
    | Some (prec, assoc) ->
        let op_tok = st.tok in
        advance st;
        let next_min_prec =
          match assoc with Left | NonAssoc -> prec + 1 | Right -> prec
        in
        if op_tok = AS then begin
          let ty = parse_typ st in
          lhs := mk lo st (Cast (!lhs, ty))
        end
        else if op_tok = DOTDOT then begin
          let rhs = parse_expr st next_min_prec in
          lhs := mk lo st (Range (!lhs, rhs))
        end
        else if op_tok = DOTDOTEQ then begin
          let rhs = parse_expr st next_min_prec in
          lhs := mk lo st (RangeInclusive (!lhs, rhs))
        end
        else begin
          let rhs = parse_expr st next_min_prec in
          let op = binop_of op_tok in
          lhs := mk lo st (BinOp (op, !lhs, rhs))
        end;
        lhs := parse_postfix st !lhs
  done;
  !lhs

(* -x *)
and parse_prefix st =
  let lo = cur_pos st in
  match st.tok with
  | BANG ->
      advance st;
      mk lo st (UnOp (Not, parse_prefix st))
  | MINUS ->
      advance st;
      mk lo st (UnOp (Neg, parse_prefix st))
  | TILDE ->
      advance st;
      mk lo st (UnOp (BitNot, parse_prefix st))
  | INCR ->
      advance st;
      mk lo st (UnOp (PreInc, parse_prefix st))
  | DECR ->
      advance st;
      mk lo st (UnOp (PreDec, parse_prefix st))
  (* TODO: I need to add IDENT before testing *)
  | AT ->
      advance st;
      mk lo st (UnOp (AddressOf, parse_prefix st))
  | _ -> parse_primary st

(* x^, x.field *)
and parse_postfix st lhs = match st.tok with _ -> lhs

(* 1, x, "str", foo(a, b) *)
and parse_primary st =
  let lo = cur_pos st in
  match st.tok with
  | INT n ->
      advance st;
      mk lo st (Int n)
  | FLOAT f ->
      advance st;
      mk lo st (Float f)
  | TRUE ->
      advance st;
      mk lo st (Bool true)
  | FALSE ->
      advance st;
      mk lo st (Bool false)
  | NULL ->
      advance st;
      mk lo st Null
  | LPAREN ->
      advance st;
      let e = parse_expr st 1 in
      expect st RPAREN;
      e
  (* sizeof(x) *)
  | SIZEOF ->
      advance st;
      expect st LPAREN;
      let t = parse_typ st in
      expect st RPAREN;
      mk lo st (SizeOf t)
  | _ -> raise (ParseError (cur_lex_pos st, "expected expression"))

and parse_simple_stmt st =
  let lo = cur_pos st in
  match st.tok with
  (* let x: i32 = 42 *)
  | LET ->
      advance st;
      let name = expect_ident st in
      (* optional type annotation since the typechecker can infer it *)
      let ann =
        if at st COLON then (
          advance st;
          Some (parse_typ st))
        else None
      in
      (* unlike var let always requires a value *)
      expect st ASSIGN;
      let e = parse_expr st 1 in
      mks lo st (Let (name, ann, e))
  (* var x: i32 / var x = 42 / var x *)
  | VAR ->
      advance st;
      let name = expect_ident st in
      let ann =
        if at st COLON then (
          advance st;
          Some (parse_typ st))
        else None
      in
      let e =
        if at st ASSIGN then (
          advance st;
          Some (parse_expr st 1))
        else None
      in
      mks lo st (Var (name, ann, e))
  | BREAK ->
      advance st;
      mks lo st Break
  | CONTINUE ->
      advance st;
      mks lo st Continue
  | RETURN ->
      (* return with no value ends at a newline or closing brace *)
      advance st;
      if st.tok = SEMI || st.tok = RBRACE || st.tok = EOF then
        mks lo st (Return None)
      else
        let e = parse_expr st 1 in
        mks lo st (Return (Some e))
  | _ ->
      let e = parse_expr st 1 in
      mks lo st (Expr e)

(* { return a + b } *)
and parse_block st =
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

and parse_stmt st =
  let lo = cur_pos st in
  match st.tok with
  | IF -> parse_if st
  | WHILE -> parse_while st
  | FOR -> parse_for st
  | LBRACE ->
      let body = parse_block st in
      mks lo st (Block body)
  | _ ->
      let s = parse_simple_stmt st in
      (* ends with a semicolon, clean up *)
      if st.tok = SEMI then advance st;
      s

(* if x < 0 { return lo } elseif x > 0 { 1 } else { 0 } *)
and parse_if st =
  let lo = cur_pos st in
  advance st;
  (* IF *)
  let cond = parse_expr st 1 in
  skip_semi st;
  let body = parse_block st in
  let elseifs = ref [] in
  while st.tok = ELSEIF do
    advance st;
    let c = parse_expr st 1 in
    skip_semi st;
    let b = parse_block st in
    (* collecting elseifs in order *)
    elseifs := (c, b) :: !elseifs
  done;
  let else_body =
    if st.tok = ELSE then begin
      advance st;
      skip_semi st;
      parse_block st
    end
    else [] (* no else branch, uniform with body type *)
  in
  mks lo st (If ((cond, body) :: List.rev !elseifs, else_body))

(* while i < len { } *)
and parse_while st =
  let lo = cur_pos st in
  advance st;
  (* WHILE *)
  let cond = parse_expr st 1 in
  skip_semi st;
  let body = parse_block st in
  mks lo st (While (cond, body))

(* for i in 0..len { } *)
and parse_for st =
  let lo = cur_pos st in
  advance st;
  (* FOR *)
  let name = expect_ident st in
  expect st IN;
  let iter = parse_expr st 1 in
  skip_semi st;
  let body = parse_block st in
  mks lo st (For (name, iter, body))

(* add(a: i32, b: i32): i32 { return a + b } *)
let parse_func st mods =
  let lo = cur_pos st in
  let pos = cur_lex_pos st in
  let name = expect_ident st in
  (match st.tok with
  | IDENT _ ->
      raise (ParseError (pos, Printf.sprintf "unknown modifier '%s'" name))
  | _ -> ());
  let params = parse_params st in
  let ret = parse_ret_type st in
  skip_semi st;
  let body = parse_block st in
  let hi = cur_pos st in
  Func { name; params; ret; body; modifiers = mods; span = { lo; hi } }

(* extern add(a: i32, b: i32): i32 *)
(* No modifiers needed *)
let parse_extern st =
  let lo = cur_pos st in
  advance st;
  (* EXTERN *)
  let name = expect_ident st in
  let params = parse_params st in
  let ret = parse_ret_type st in
  let hi = cur_pos st in
  skip_semi st;
  Extern { name; params; ret; body = []; modifiers = []; span = { lo; hi } }

let parse_decl st =
  match st.tok with
  | EXTERN -> parse_extern st
  | STRUCT -> parse_struct st []
  | PUBLIC | INLINE -> (
      let mods = parse_modifiers st in
      match st.tok with
      | STRUCT -> parse_struct st mods
      | _ -> parse_func st mods)
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
