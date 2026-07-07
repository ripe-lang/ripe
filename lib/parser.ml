(* SPDX-License-Identifier: GPL-2.0-only *)
(* errors: "expected X but found Y" *)
(* TODO(97b3): accumulate multiple parse errors instead of failing on the first *)

open Tokens
open Ast

exception ParseError of Diagnostic.t

type state = {
  mutable tok : token;
  lexbuf : Lexing.lexbuf;
  read : Lexing.lexbuf -> token;
  diags : Diagnostic.sink;
  mutable no_struct_lit : bool;
  mutable prev_end : int;
}

type assoc = Left | Right | NonAssoc

(* span of the current lookahead token so the caret lands under it *)
let cur_span st =
  {
    lo = st.lexbuf.Lexing.lex_start_p.pos_cnum;
    hi = st.lexbuf.Lexing.lex_curr_p.pos_cnum;
  }

let rec advance st =
  st.prev_end <- st.lexbuf.Lexing.lex_curr_p.pos_cnum;
  st.tok <- st.read st.lexbuf;
  match st.tok with
  (* TODO(5689): cap at 20 errors then bail, with a flag to list the rest *)
  | ERROR msg ->
      Diagnostic.error_at st.diags (cur_span st) msg;
      advance st
  | _ -> ()

let at st t = st.tok = t

(* start of the current lookahead token *)
let cur_pos st = st.lexbuf.Lexing.lex_start_p.pos_cnum

let fail st headline =
  raise (ParseError Diagnostic.(error headline |> at (cur_span st)))

let fail_found st headline =
  raise
    (ParseError
       Diagnostic.(
         error headline
         |> at (cur_span st)
         |> label (Printf.sprintf "found %s" (show_token st.tok))))

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
  | _ -> fail_found st "expected identifier"

let expect_ident_span st =
  let lo = st.lexbuf.Lexing.lex_start_p.pos_cnum in
  let hi = st.lexbuf.Lexing.lex_curr_p.pos_cnum in
  let name = expect_ident st in
  (name, { lo; hi })

let expect st t =
  if st.tok <> t then
    fail_found st (Printf.sprintf "expected %s" (show_token t))
  else advance st

(* item (, item)* with an optional trailing comma before stop *)
let comma_sep st stop parse_one =
  let items = ref [] in
  if st.tok <> stop then begin
    items := [ parse_one () ];
    while st.tok = COMMA do
      advance st;
      if st.tok <> stop then items := parse_one () :: !items
    done
  end;
  List.rev !items

let mk lo st desc = { desc; span = { lo; hi = st.prev_end } }
let mks lo st sdesc = { sdesc; span = { lo; hi = st.prev_end } }
let mkt lo st tdesc = { tdesc; span = { lo; hi = st.prev_end } }

(* i32, *i32, (i32, i32) i32 *)
let rec parse_typ st =
  let lo = cur_pos st in
  match st.tok with
  | STAR ->
      advance st;
      mkt lo st (Pointer (parse_typ st))
  | IDENT name ->
      advance st;
      mkt lo st (Named name)
  (* [N]T fixed-size array, []T slice *)
  | LBRACKET ->
      advance st;
      if st.tok = RBRACKET then (
        advance st;
        mkt lo st (Slice (parse_typ st)))
      else
        let n =
          match st.tok with
          | INT n ->
              advance st;
              Int64.to_int n
          | _ -> fail_found st "expected array size"
        in
        expect st RBRACKET;
        mkt lo st (Array (n, parse_typ st))
  | LPAREN ->
      advance st;
      let params = comma_sep st RPAREN (fun () -> parse_typ st) in
      expect st RPAREN;
      let ret =
        match st.tok with
        | IDENT _ | STAR | LPAREN | LBRACKET -> Some (parse_typ st)
        | _ -> None
      in
      mkt lo st (FuncPtr (params, ret))
  | _ -> fail_found st "expected type"

let parse_modifiers st =
  let rec go acc =
    match st.tok with
    | INLINE ->
        advance st;
        go (Ast.Inline :: acc)
    (* TODO(74d8): not entirely sure yet. static? public? *)
    | PUBLIC ->
        advance st;
        go (Ast.Pub :: acc)
    | _ -> List.rev acc
  in
  go []

(* x: i32 *)
let parse_fields st =
  let fields = ref [] in
  while st.tok <> RBRACE do
    (* TODO(9ee0): parse modifiers *)
    let name, nspan = expect_ident_span st in
    expect st COLON;
    let t = parse_typ st in
    fields :=
      ({ name; typ = t; modifiers = []; span = nspan } : field) :: !fields;
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
  let hi = st.prev_end in
  skip_semi st;
  Struct { name; fields; modifiers = mods; span = { lo; hi } }

(* (a: i32, b: i32) or (fmt: cstr, ...) returns (params, variadic) *)
let parse_params st =
  expect st LPAREN;
  let params = ref [] in
  let variadic = ref false in
  let parse_one () =
    let lo = cur_pos st in
    let name = expect_ident st in
    expect st COLON;
    let t = parse_typ st in
    let hi = st.prev_end in
    ({ name; typ = t; span = { lo; hi } } : param)
  in
  if st.tok <> RPAREN then begin
    params := [ parse_one () ];
    while st.tok = COMMA && not !variadic do
      advance st;
      if st.tok = ELLIPSIS then (
        advance st;
        variadic := true)
      else params := parse_one () :: !params
    done
  end;
  if !variadic && st.tok = COMMA then fail st "`...` must be the last parameter";
  expect st RPAREN;
  (List.rev !params, !variadic)

(* i32 *)
let parse_ret_type st =
  match st.tok with
  | LBRACE | SEMI | EOF | ASSIGN -> None
  | _ -> Some (parse_typ st)

(* Postfix binds tighter than any infix: a.b + c means (a.b) + c. *)

let prec_of = function
  | ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | STAR_ASSIGN | SLASH_ASSIGN ->
      Some (1, Right)
  | DOTDOT | DOTDOTEQ -> Some (2, NonAssoc)
  | OR -> Some (3, Left)
  | AND -> Some (4, Left)
  | EQ | NEQ | LT | GT | LTE | GTE -> Some (5, NonAssoc)
  | PIPE -> Some (6, Left)
  | CARET -> Some (7, Left)
  | AMP -> Some (8, Left)
  | LSHIFT | RSHIFT -> Some (9, Left)
  | PLUS | MINUS -> Some (10, Left)
  | STAR | SLASH | PERCENT -> Some (11, Left)
  | AS -> Some (12, Left)
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
  | CARET -> BitXor
  | LSHIFT -> Lshift
  | RSHIFT -> Rshift
  | ASSIGN -> Assign
  | PLUS_ASSIGN -> AddAssign
  | MINUS_ASSIGN -> SubAssign
  | STAR_ASSIGN -> MulAssign
  | SLASH_ASSIGN -> DivAssign
  | _ -> failwith "not a binary operator"

let in_brackets st f =
  let saved = st.no_struct_lit in
  st.no_struct_lit <- false;
  let r = f () in
  st.no_struct_lit <- saved;
  r

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
        (* reject a < b < c, 0..5..10, etc *)
        (* TODO(a300): better message, point at both operators *)
        (match (assoc, prec_of st.tok) with
        | NonAssoc, Some (p, _) when p = prec ->
            fail st
              (Printf.sprintf "cannot chain non-associative operator %s"
                 (show_token st.tok))
        | _ -> ());
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
  | AMP ->
      advance st;
      mk lo st (UnOp (AddressOf, parse_prefix st))
  | STAR ->
      advance st;
      mk lo st (UnOp (Deref, parse_prefix st))
  | _ -> parse_primary st

(* x.field, arr[i] *)
and parse_postfix st lhs =
  let lo = lhs.span.lo in
  match st.tok with
  | DOT ->
      advance st;
      let name = expect_ident st in
      parse_postfix st (mk lo st (FieldAccess (lhs, name)))
  | LBRACKET ->
      advance st;
      let idx = in_brackets st (fun () -> parse_expr st 1) in
      expect st RBRACKET;
      parse_postfix st (mk lo st (Index (lhs, idx)))
  | _ -> lhs

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
      let e = in_brackets st (fun () -> parse_expr st 1) in
      expect st RPAREN;
      e
  (* [1, 2, 3] array literal *)
  | LBRACKET ->
      advance st;
      let elems = parse_comma_list st RBRACKET in
      expect st RBRACKET;
      mk lo st (ArrayLit elems)
  (* sizeof(x) *)
  | SIZEOF ->
      advance st;
      expect st LPAREN;
      let t = parse_typ st in
      expect st RPAREN;
      mk lo st (SizeOf t)
  | IDENT name ->
      let nspan = { lo; hi = st.lexbuf.Lexing.lex_curr_p.pos_cnum } in
      advance st;
      if at st LPAREN then begin
        advance st;
        let args = parse_comma_list st RPAREN in
        expect st RPAREN;
        mk lo st (Call (name, args))
      end
      else if at st LBRACE && not st.no_struct_lit then begin
        advance st;
        let fields = parse_struct_lit_fields st in
        expect st RBRACE;
        mk lo st (StructLit (name, nspan, fields))
      end
      else mk lo st (Ident name)
  (* "hello {name}!" *)
  | STRING_START ->
      advance st;
      let parts = ref [] in
      while st.tok <> STRING_END do
        match st.tok with
        | STRING_PART s ->
            (* plain text chunk *)
            advance st;
            parts := Lit s :: !parts
        | INTERP_START ->
            (* {expr} *)
            advance st;
            let e = in_brackets st (fun () -> parse_expr st 1) in
            expect st INTERP_END;
            parts := Interp e :: !parts
        | _ -> assert false (* should be unreachable *)
      done;
      expect st STRING_END;
      mk lo st (InterpString (List.rev !parts))
  | UNDEFINED ->
      advance st;
      mk lo st Undefined
  | _ -> fail_found st "expected expression"

and parse_comma_list st stop = comma_sep st stop (fun () -> parse_expr st 1)

(* x: 3, y: 4 *)
and parse_struct_lit_fields st =
  in_brackets st (fun () ->
      skip_semi st;
      let fields = ref [] in
      while st.tok <> RBRACE do
        let name, nspan = expect_ident_span st in
        expect st COLON;
        let e = parse_expr st 1 in
        fields := (name, nspan, e) :: !fields;
        if st.tok = COMMA then advance st;
        skip_semi st
      done;
      List.rev !fields)

(* IDENT { in an if/while/for header is the body, not a struct literal *)
and parse_header_expr st =
  st.no_struct_lit <- true;
  let e = parse_expr st 1 in
  st.no_struct_lit <- false;
  e

and parse_simple_stmt st =
  let lo = cur_pos st in
  match st.tok with
  (* const x: i32 = 42 *)
  | CONST ->
      advance st;
      let name, nspan = expect_ident_span st in
      (* optional type annotation since the typechecker can infer it *)
      let ann =
        if at st COLON then (
          advance st;
          Some (parse_typ st))
        else None
      in
      (* const always requires a value *)
      expect st ASSIGN;
      let e = parse_expr st 1 in
      mks lo st (Const (name, nspan, ann, e))
  (* var x: i32 / var x = 42 / var x *)
  | VAR ->
      advance st;
      let name, nspan = expect_ident_span st in
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
      mks lo st (Var (name, nspan, ann, e))
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
  let cond = parse_header_expr st in
  skip_semi st;
  let body = parse_block st in
  let rec parse_elseifs acc =
    if st.tok = ELSEIF then begin
      advance st;
      let c = parse_header_expr st in
      skip_semi st;
      let b = parse_block st in
      parse_elseifs ((c, b) :: acc)
    end
    else List.rev acc
  in
  let elseifs = parse_elseifs [] in
  let else_body =
    if st.tok = ELSE then begin
      advance st;
      if st.tok = IF then
        raise
          (ParseError
             Diagnostic.(
               error "expected block after else"
               |> at (cur_span st)
               |> label "found if"
               |> help "the keyword is elseif, one word"));
      skip_semi st;
      parse_block st
    end
    else [] (* no else branch, uniform with body type *)
  in
  mks lo st (If ((cond, body) :: elseifs, else_body))

(* while i < len { } *)
and parse_while st =
  let lo = cur_pos st in
  advance st;
  (* WHILE *)
  let cond = parse_header_expr st in
  skip_semi st;
  let body = parse_block st in
  mks lo st (While (cond, body))

(* for i in 0..len { } *)
and parse_for st =
  let lo = cur_pos st in
  advance st;
  (* FOR *)
  let name, nspan = expect_ident_span st in
  expect st IN;
  let iter = parse_header_expr st in
  skip_semi st;
  let body = parse_block st in
  mks lo st (For (name, nspan, iter, body))

(* func NAME(params) ret *)
let parse_signature st =
  expect st FUNC;
  let name = expect_ident st in
  let params, variadic = parse_params st in
  let ret = parse_ret_type st in
  (name, params, ret, variadic)

(* func add(a: i32, b: i32) i32 { ... } *)
let parse_func st mods =
  let lo = cur_pos st in
  let name, params, ret, variadic = parse_signature st in
  let body =
    if st.tok = ASSIGN then begin
      let slo = cur_pos st in
      advance st;
      let e = parse_expr st 1 in
      [ mks slo st (Return (Some e)) ]
    end
    else begin
      skip_semi st;
      parse_block st
    end
  in
  let hi = st.prev_end in
  Func
    { name; params; ret; body; modifiers = mods; variadic; span = { lo; hi } }

(* const PAGE_SIZE: i32 = 4096 / var n: i32 = 0 / var flag: bool *)
let parse_global st =
  let lo = cur_pos st in
  let is_const = st.tok = CONST in
  advance st;
  let name = expect_ident st in
  expect st COLON;
  let typ = parse_typ st in
  let init =
    if at st ASSIGN then (
      advance st;
      Some (parse_expr st 1))
    else None
  in
  let hi = st.prev_end in
  skip_semi st;
  Global { name; typ; init; is_const; span = { lo; hi } }

(* type binop = (i32, i32) i32 *)
let parse_type_alias st =
  let lo = cur_pos st in
  advance st;
  (* TYPE *)
  let name = expect_ident st in
  expect st ASSIGN;
  let typ = parse_typ st in
  let hi = st.prev_end in
  skip_semi st;
  Ast.TypeAlias { name; typ; span = { lo; hi } }

(* newtype Celsius = f32 *)
let parse_newtype st =
  let lo = cur_pos st in
  advance st;
  (* NEWTYPE *)
  let name = expect_ident st in
  expect st ASSIGN;
  let typ = parse_typ st in
  let hi = st.prev_end in
  skip_semi st;
  Ast.Newtype { name; typ; span = { lo; hi } }

(* extern func add(a: i32, b: i32) i32 *)
let parse_extern st =
  let lo = cur_pos st in
  advance st;
  (* EXTERN *)
  let name, params, ret, variadic = parse_signature st in
  let hi = st.prev_end in
  skip_semi st;
  Extern
    {
      name;
      params;
      ret;
      body = [];
      modifiers = [];
      variadic;
      span = { lo; hi };
    }

let parse_decl st =
  let err () = fail_found st "expected declaration" in
  let mods = parse_modifiers st in
  match (mods, st.tok) with
  | _, STRUCT -> parse_struct st mods
  | _, FUNC -> parse_func st mods
  | [], EXTERN -> parse_extern st
  | [], (CONST | VAR) -> parse_global st
  | [], TYPE -> parse_type_alias st
  | [], NEWTYPE -> parse_newtype st
  | _ -> err ()

(* TODO(fa20): finer recovery inside blocks, sync to next stmt boundary *)
let rec sync_to_decl st =
  match st.tok with
  | EOF | FUNC | CONST | VAR | EXTERN | STRUCT | INLINE | PUBLIC | TYPE
  | NEWTYPE ->
      ()
  | _ ->
      advance st;
      sync_to_decl st

let parse_program st =
  let decls = ref [] in
  skip_semi st;
  while st.tok <> EOF do
    try
      decls := parse_decl st :: !decls;
      skip_semi st
    with ParseError d ->
      Diagnostic.emit st.diags d;
      sync_to_decl st
  done;
  List.rev !decls

let parse (read : Lexing.lexbuf -> Tokens.token) (lexbuf : Lexing.lexbuf) :
    Ast.decl list =
  let st =
    {
      tok = EOF;
      lexbuf;
      read;
      diags = Diagnostic.sink ();
      no_struct_lit = false;
      prev_end = 0;
    }
  in
  advance st;
  let decls = parse_program st in
  match Diagnostic.drain st.diags with
  | [] -> decls
  | ds -> raise (Diagnostic.Errors ds)
