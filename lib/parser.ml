(* SPDX-License-Identifier: GPL-2.0-only *)

open Tokens
open Ast

exception ParseError of Diagnostic.t

type token_info = { token : token; span : span; line : int; depth : int }

type state = {
  mutable tok : token;
  mutable tok_span : span;
  mutable tok_line : int;
  mutable tok_depth : int;
  read : unit -> token_info;
  diags : Diagnostic.sink;
  mutable no_struct_lit : bool;
  mutable prev_end : int;
  mutable recovered : bool;
}

type chain = Comparison | Range
type assoc = Left | Right
type infix = { prec : int; assoc : assoc; chain : chain option }

(* Span of the current lookahead token so the caret lands under it *)
let cur_span st = st.tok_span

let advance st =
  st.prev_end <- st.tok_span.hi;
  let info = st.read () in
  st.tok <- info.token;
  st.tok_span <- info.span;
  st.tok_line <- info.line;
  st.tok_depth <- info.depth

let at st t = st.tok = t

(* Start of the current lookahead token *)
let cur_pos st = st.tok_span.lo

let fail st headline =
  raise (ParseError (Diagnostic.error headline |> Diagnostic.at (cur_span st)))

let fail_found st headline =
  raise
    (ParseError
       (Diagnostic.error headline
       |> Diagnostic.at (cur_span st)
       |> Diagnostic.label (Printf.sprintf "found %s" (show_token st.tok))))

let is_expr_start (tok : token) : bool =
  match tok with
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | TRUE | FALSE | NULL | SIZEOF | LPAREN | LBRACKET | UNDEFINED
    ->
      true
  | _ -> false

let require_expr_start st tok span =
  if not (is_expr_start st.tok) then
    raise (ParseError (Error.expected_expression_after span (show_token tok)))

let is_type_start (tok : token) : bool =
  match tok with IDENT _ | STAR | LPAREN | LBRACKET -> true | _ -> false

let expect_type_after st op span =
  if not (is_type_start st.tok) then
    raise (ParseError (Error.expected_type_after span op))

let parse_cast_kind st op_span =
  match st.tok with
  | BANG ->
      let bang_span = cur_span st in
      advance st;
      (Checked, Span.make op_span.file op_span.lo bang_span.hi)
  | _ -> (Normal, op_span)

let is_postfix_tok = function DOT | LBRACKET | LPAREN -> true | _ -> false

(* Newlines lex as SEMI *)
let skip_semi st =
  while st.tok = SEMI do
    advance st
  done

let is_stmt_start (tok : token) : bool =
  match tok with
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | LET | COMPTIME | VAR | RETURN | IF | WHILE | FOR | BREAK
  | CONTINUE | TRUE | FALSE | NULL | SIZEOF | LPAREN | LBRACE | LBRACKET
  | UNDEFINED ->
      true
  | _ -> false

let is_item_start (tok : token) : bool =
  match tok with
  | FUNC | EXTERN | STRUCT | INLINE | PUBLIC | TYPE | NEWTYPE | IMPORT | LET
  | COMPTIME | VAR ->
      true
  | _ -> false

let rec sync_to_stmt (st : state) (depth : int) (line : int) (after_semi : bool)
    : unit =
  match st.tok with
  | EOF -> ()
  | RBRACE when st.tok_depth = depth -> ()
  | _
    when st.tok_depth = depth && is_stmt_start st.tok
         && (after_semi || st.tok_line > line) ->
      ()
  | SEMI when st.tok_depth = depth ->
      advance st;
      sync_to_stmt st depth line true
  | _ ->
      advance st;
      sync_to_stmt st depth line after_semi

let expect_ident st =
  match st.tok with
  | IDENT s ->
      advance st;
      s
  | _ -> fail_found st "expected identifier"

let expect_ident_span st =
  let span = st.tok_span in
  let name = expect_ident st in
  (name, span)

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
    done;
    if st.tok = SEMI then fail st "missing `,` before newline"
  end;
  List.rev !items

let make_span st lo hi = Span.make st.tok_span.file lo hi
let mk lo st desc = { desc; span = make_span st lo st.prev_end }
let mkt lo st tdesc = { tdesc; span = make_span st lo st.prev_end }

let recovery_span (st : state) (d : Diagnostic.t) : span =
  Option.value d.Diagnostic.primary ~default:(cur_span st)

let error_expr (st : state) (d : Diagnostic.t) : expr =
  { desc = ErrorExpr; span = recovery_span st d }

let error_typ (st : state) (d : Diagnostic.t) : typ =
  { tdesc = ErrorType; span = recovery_span st d }

let rec sync_to_depth_token (st : state) (depth : int) (line : int)
    (stops : token list) : unit =
  if
    st.tok = EOF
    || st.tok_depth = depth
       && (List.mem st.tok stops || st.tok = SEMI || st.tok = RBRACE)
    || st.tok_depth = depth && st.tok_line > line
       && if depth = 0 then is_item_start st.tok else is_stmt_start st.tok
  then ()
  else (
    advance st;
    sync_to_depth_token st depth line stops)

let recover_expr_to (st : state) (depth : int) (stops : token list)
    (parse : unit -> expr) : expr =
  let line = st.tok_line in
  try parse ()
  with ParseError d ->
    Diagnostic.emit st.diags d;
    sync_to_depth_token st depth line stops;
    st.recovered <- true;
    error_expr st d

let recover_typ_to (st : state) (depth : int) (stops : token list)
    (parse : unit -> typ) : typ =
  let line = st.tok_line in
  try parse ()
  with ParseError d ->
    Diagnostic.emit st.diags d;
    sync_to_depth_token st depth line stops;
    st.recovered <- true;
    error_typ st d

let expect_field_sep st =
  (match st.tok with
  | COMMA -> advance st
  | SEMI | RBRACE -> ()
  | _ -> fail_found st "expected `,` or newline between fields");
  skip_semi st

let expect_literal_field_sep st =
  match st.tok with
  | COMMA -> advance st
  | SEMI -> fail st "missing `,` before newline"
  | RBRACE -> ()
  | _ -> fail_found st "expected `,` between fields"

let left prec = Some { prec; assoc = Left; chain = None }
let right prec = Some { prec; assoc = Right; chain = None }
let nonassoc prec chain = Some { prec; assoc = Left; chain = Some chain }

let prec_of = function
  | ASSIGN | PLUS_ASSIGN | MINUS_ASSIGN | STAR_ASSIGN | SLASH_ASSIGN
  | PERCENT_ASSIGN | AMP_ASSIGN | PIPE_ASSIGN | CARET_ASSIGN | LSHIFT_ASSIGN
  | RSHIFT_ASSIGN ->
      right 1
  | DOTDOT | DOTDOTEQ -> nonassoc 2 Range
  | OR -> left 3
  | AND -> left 4
  | EQ | NEQ | LT | GT | LTE | GTE -> nonassoc 5 Comparison
  | PIPE -> left 6
  | CARET -> left 7
  | AMP -> left 8
  | LSHIFT | RSHIFT -> left 9
  | PLUS | MINUS -> left 10
  | STAR | SLASH | PERCENT -> left 11
  | AS -> left 12
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
  | PERCENT_ASSIGN -> ModAssign
  | AMP_ASSIGN -> BitAndAssign
  | PIPE_ASSIGN -> BitOrAssign
  | CARET_ASSIGN -> BitXorAssign
  | LSHIFT_ASSIGN -> LshiftAssign
  | RSHIFT_ASSIGN -> RshiftAssign
  | _ -> failwith "not a binary operator"

let with_struct_lit (st : state) (no_struct_lit : bool) (f : unit -> 'a) : 'a =
  let saved = st.no_struct_lit in
  st.no_struct_lit <- no_struct_lit;
  Fun.protect f ~finally:(fun () -> st.no_struct_lit <- saved)

let in_brackets (st : state) (f : unit -> 'a) : 'a = with_struct_lit st false f

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
        let n = parse_expr st 1 in
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

and parse_modifiers st =
  let rec go acc =
    match st.tok with
    | INLINE ->
        advance st;
        go (Ast.Inline :: acc)
    | PUBLIC ->
        advance st;
        go (Ast.Pub :: acc)
    | _ -> List.rev acc
  in
  go []

(* x: i32 *)
and parse_fields st =
  let fields = ref [] in
  let depth = st.tok_depth in
  while st.tok <> RBRACE do
    let name, nspan = expect_ident_span st in
    let t =
      recover_typ_to st depth [ COMMA ] (fun () ->
          expect st COLON;
          parse_typ st)
    in
    fields :=
      ({ name; typ = t; modifiers = []; span = nspan } : field) :: !fields;
    expect_field_sep st
  done;
  List.rev !fields

(* struct point { x: i32, y: i32 } *)
and parse_struct st mods =
  let lo = cur_pos st in
  advance st;
  (* STRUCT *)
  let name = expect_ident st in
  expect st LBRACE;
  let fields = parse_fields st in
  expect st RBRACE;
  let hi = st.prev_end in
  Struct { name; fields; modifiers = mods; span = make_span st lo hi }

(* (a: i32, b: i32) or (fmt: cstr, ...) returns (params, variadic) *)
and parse_params st =
  expect st LPAREN;
  let depth = st.tok_depth in
  let params = ref [] in
  let variadic = ref false in
  let parse_one () =
    let lo = cur_pos st in
    let name = expect_ident st in
    let t =
      recover_typ_to st depth [ COMMA; RPAREN ] (fun () ->
          expect st COLON;
          parse_typ st)
    in
    let hi = st.prev_end in
    ({ name; typ = t; span = make_span st lo hi } : param)
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
  if st.tok = SEMI then fail st "missing `,` before newline";
  expect st RPAREN;
  (List.rev !params, !variadic)

(* i32 *)
and parse_ret_type st =
  match st.tok with
  | LBRACE | SEMI | EOF | ASSIGN -> None
  | _ ->
      let depth = st.tok_depth in
      Some (recover_typ_to st depth [ LBRACE; ASSIGN ] (fun () -> parse_typ st))

(* Postfix binds tighter than infix so a.b + c means (a.b) + c *)

and parse_expr st min_prec =
  let lo = cur_pos st in
  let lhs = ref (parse_prefix st) in

  (* Precedence climbing for infix ops *)
  let loop = ref true in
  while !loop do
    match prec_of st.tok with
    | None -> loop := false
    | Some op when op.prec < min_prec -> loop := false
    | Some op -> (
        let op_tok = st.tok in
        let op_span = cur_span st in
        advance st;
        if op_tok <> AS then require_expr_start st op_tok op_span;
        let next_min_prec =
          match op.assoc with Left -> op.prec + 1 | Right -> op.prec
        in
        if op_tok = AS then begin
          let kind, cast_op_span = parse_cast_kind st op_span in
          expect_type_after st (show_cast_op kind) cast_op_span;
          let ty = parse_typ st in
          lhs := mk lo st (Cast (!lhs, ty, kind));
          if is_postfix_tok st.tok then
            raise
              (ParseError
                 (Diagnostic.error "postfix operator applied to a cast"
                 |> Diagnostic.at (cur_span st)
                 |> Diagnostic.help "parenthesize the cast: `(x as T)[...]`"))
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
        match (op.chain, prec_of st.tok) with
        | Some chain, Some next when next.chain = op.chain ->
            let name, help =
              match chain with
              | Comparison ->
                  ( "comparison",
                    "split the chain into separate comparisons joined with `&&`"
                  )
              | Range -> ("range", "parenthesize a range if nesting is intended")
            in
            raise
              (ParseError
                 (Diagnostic.error
                    (Printf.sprintf "%s operators cannot be chained" name)
                 |> Diagnostic.at (cur_span st)
                 |> Diagnostic.label (Printf.sprintf "second %s operator" name)
                 |> Diagnostic.secondary op_span
                      (Printf.sprintf "first %s operator" name)
                 |> Diagnostic.help help))
        | _ -> ())
  done;
  !lhs

(* -x *)
and parse_prefix st =
  let lo = cur_pos st in
  let unary op desc =
    let span = cur_span st in
    advance st;
    require_expr_start st op span;
    mk lo st (UnOp (desc, parse_prefix st))
  in
  match st.tok with
  | BANG -> unary BANG Not
  | PLUS -> unary PLUS Pos
  | MINUS -> unary MINUS Neg
  | TILDE -> unary TILDE BitNot
  | AMP -> unary AMP AddressOf
  | STAR -> unary STAR Deref
  | _ -> parse_postfix st (parse_primary st)

(* x.field, arr[i], f(args) *)
and parse_postfix st (lhs : expr) =
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
  | LPAREN ->
      advance st;
      let args = parse_comma_list st RPAREN in
      expect st RPAREN;
      parse_postfix st (mk lo st (Call (lhs, args)))
  | _ -> lhs

(* 1, x, "str", foo(a, b) *)
and parse_primary st =
  let lo = cur_pos st in
  match st.tok with
  | INT (n, suf) ->
      advance st;
      mk lo st (Int (n, suf))
  | CHAR c ->
      advance st;
      mk lo st (Char c)
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
      let nspan = st.tok_span in
      advance st;
      if at st LBRACE && not st.no_struct_lit then begin
        advance st;
        let fields = parse_struct_lit_fields st in
        expect st RBRACE;
        mk lo st (StructLit (name, nspan, fields))
      end
      else mk lo st (Ident name)
  | STRING s ->
      advance st;
      mk lo st (String s)
  | UNDEFINED ->
      advance st;
      mk lo st Undefined
  | ELSE | ELSEIF ->
      raise
        (ParseError
           (Diagnostic.error
              (Printf.sprintf "`%s` without a matching `if`" (show_token st.tok))
           |> Diagnostic.at (cur_span st)
           |> Diagnostic.label (Printf.sprintf "found %s" (show_token st.tok))
           |> Diagnostic.help "an `if` used as a value closes at its `}`"))
  | _ -> fail_found st "expected expression"

and parse_comma_list st stop = comma_sep st stop (fun () -> parse_expr st 1)

(* If and block are values too and where they sit decides if the value is
   used *)
and parse_value st =
  let lo = cur_pos st in
  match st.tok with
  | IF -> parse_if st
  | LBRACE -> mk lo st (Block (parse_block st))
  | _ -> parse_expr st 1

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
        expect_literal_field_sep st
      done;
      List.rev !fields)

(* IDENT { in an if/while/for header is the body, not a struct literal *)
and parse_header_expr st = with_struct_lit st true (fun () -> parse_expr st 1)

and parse_simple_stmt st =
  let lo = cur_pos st in
  match st.tok with
  (* let x: i32 = 42 / comptime N: i32 = 4 / var x: i32 / var x = 42 / var x *)
  | (LET | COMPTIME | VAR) as tok ->
      let depth = st.tok_depth in
      advance st;
      let kind =
        match tok with
        | LET -> Ast.Let
        | COMPTIME -> Ast.Comptime
        | _ -> Ast.Var
      in
      let name, nspan = expect_ident_span st in
      (* Optional type annotation since the typechecker can infer it *)
      let ann =
        if at st COLON then (
          advance st;
          Some (recover_typ_to st depth [ ASSIGN ] (fun () -> parse_typ st)))
        else None
      in
      (* Only var may omit the value *)
      let e =
        if kind <> Ast.Var then (
          expect st ASSIGN;
          Some (recover_expr_to st depth [] (fun () -> parse_value st)))
        else if at st ASSIGN then (
          advance st;
          Some (recover_expr_to st depth [] (fun () -> parse_value st)))
        else None
      in
      mk lo st (Binding (kind, name, nspan, ann, e))
  | BREAK ->
      advance st;
      mk lo st Break
  | CONTINUE ->
      advance st;
      mk lo st Continue
  | RETURN ->
      (* Return with no value ends at a newline or closing brace *)
      advance st;
      if st.tok = SEMI || st.tok = RBRACE || st.tok = EOF then
        mk lo st (Return None)
      else
        let e = parse_value st in
        mk lo st (Return (Some e))
  | _ ->
      let first = parse_expr st 1 in
      if at st COMMA then parse_pair_assign st lo first else first

(* a, b = b, a *)
and parse_pair_assign (state : state) (lo : int) (ft : expr) : expr =
  expect state COMMA;
  let st = parse_expr state 2 in
  if at state COMMA then
    fail state "pair assignment requires exactly two targets";
  expect state ASSIGN;
  let fv = parse_expr state 1 in
  expect state COMMA;
  let sv = parse_expr state 1 in
  if at state COMMA then
    fail state "pair assignment requires exactly two values";
  mk lo state (PairAssign (ft, st, fv, sv))

(* { return a + b } *)
and parse_block st =
  expect st LBRACE;
  let body = parse_stmts st in
  expect st RBRACE;
  body

and parse_stmts st =
  let stmts = ref [] in
  let depth = st.tok_depth in
  skip_semi st;
  while st.tok <> RBRACE && st.tok <> EOF do
    let line = st.tok_line in
    try
      let s = parse_stmt st in
      stmts := s :: !stmts;
      if st.recovered then (
        st.recovered <- false;
        skip_semi st)
      else if st.tok = SEMI then skip_semi st
      else if st.tok <> RBRACE && st.tok <> EOF then
        fail_found st "expected `;`"
    with ParseError d ->
      Diagnostic.emit st.diags d;
      sync_to_stmt st depth line false;
      stmts := error_expr st d :: !stmts;
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
      mk lo st (Block body)
  | _ -> parse_simple_stmt st

(* if x < 0 { return lo } elseif x > 0 { 1 } else { 0 } *)
and parse_if st =
  let lo = cur_pos st in
  advance st;
  (* IF *)
  let cond = parse_header_expr st in
  let body = parse_block st in
  let rec parse_elseifs acc =
    if st.tok = ELSEIF then begin
      advance st;
      let c = parse_header_expr st in
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
             (Diagnostic.error "expected block after else"
             |> Diagnostic.at (cur_span st)
             |> Diagnostic.label "found if"
             |> Diagnostic.help "the keyword is elseif, one word"));
      Some (parse_block st)
    end
    else None
  in
  mk lo st (If ((cond, body) :: elseifs, else_body))

(* while i < len { } *)
and parse_while st =
  let lo = cur_pos st in
  advance st;
  (* WHILE *)
  let cond = parse_header_expr st in
  let body = parse_block st in
  mk lo st (While (cond, body))

(* for i in 0..len { } *)
and parse_for st =
  let lo = cur_pos st in
  advance st;
  (* FOR *)
  let name, nspan = expect_ident_span st in
  expect st IN;
  let iter = parse_header_expr st in
  let body = parse_block st in
  mk lo st (For (name, nspan, iter, body))

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
      [ mk slo st (Return (Some e)) ]
    end
    else parse_block st
  in
  let hi = st.prev_end in
  Func
    {
      name;
      params;
      ret;
      body;
      modifiers = mods;
      variadic;
      span = make_span st lo hi;
    }

(* let PAGE_SIZE: i32 = 4096 / var n: i32 = 0 / var flag: bool *)
let parse_global st =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  let kind =
    match st.tok with LET -> Ast.Let | COMPTIME -> Ast.Comptime | _ -> Ast.Var
  in
  advance st;
  let name = expect_ident st in
  let typ =
    recover_typ_to st depth [ ASSIGN ] (fun () ->
        expect st COLON;
        parse_typ st)
  in
  let init =
    if at st ASSIGN then (
      advance st;
      Some (recover_expr_to st depth [] (fun () -> parse_expr st 1)))
    else None
  in
  let hi = st.prev_end in
  Global { name; typ; init; kind; span = make_span st lo hi }

(* type binop = (i32, i32) i32 *)
let parse_type_alias st =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  advance st;
  (* TYPE *)
  let name = expect_ident st in
  let typ =
    recover_typ_to st depth [] (fun () ->
        expect st ASSIGN;
        parse_typ st)
  in
  let hi = st.prev_end in
  Ast.TypeAlias { name; typ; span = make_span st lo hi }

(* newtype Celsius = f32 *)
let parse_newtype st =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  advance st;
  (* NEWTYPE *)
  let name = expect_ident st in
  let typ =
    recover_typ_to st depth [] (fun () ->
        expect st ASSIGN;
        parse_typ st)
  in
  let hi = st.prev_end in
  Ast.Newtype { name; typ; span = make_span st lo hi }

(* extern func add(a: i32, b: i32) i32 *)
let parse_extern st =
  let lo = cur_pos st in
  advance st;
  (* EXTERN *)
  let name, params, ret, variadic = parse_signature st in
  let hi = st.prev_end in
  Extern
    {
      name;
      params;
      ret;
      body = [];
      modifiers = [];
      variadic;
      span = make_span st lo hi;
    }

let parse_decl st =
  let err () = fail_found st "expected declaration" in
  let mods = parse_modifiers st in
  match (mods, st.tok) with
  | _, STRUCT -> parse_struct st mods
  | _, FUNC -> parse_func st mods
  | [], EXTERN -> parse_extern st
  | [], (LET | COMPTIME | VAR) -> parse_global st
  | [], TYPE -> parse_type_alias st
  | [], NEWTYPE -> parse_newtype st
  | _ -> err ()

(* import math.vector *)
let parse_import st =
  let lo = cur_pos st in
  advance st;
  let path = ref [ expect_ident st ] in
  while st.tok = DOT do
    advance st;
    path := expect_ident st :: !path
  done;
  let hi = st.prev_end in
  { path = List.rev !path; span = make_span st lo hi }

let rec sync_to_item (st : state) : unit =
  match st.tok with
  | EOF -> ()
  | _ when st.tok_depth = 0 && is_item_start st.tok -> ()
  | _ ->
      advance st;
      sync_to_item st

let parse_module st =
  let imports = ref [] in
  let decls = ref [] in
  skip_semi st;
  while st.tok <> EOF do
    try
      if st.tok = IMPORT then imports := parse_import st :: !imports
      else decls := parse_decl st :: !decls;
      if st.recovered then (
        st.recovered <- false;
        skip_semi st)
      else if st.tok = SEMI then skip_semi st
      else if st.tok <> EOF then fail_found st "expected `;`"
    with ParseError d ->
      Diagnostic.emit st.diags d;
      sync_to_item st
  done;
  { imports = List.rev !imports; decls = List.rev !decls }

(* TODO(5689): cap at 20 errors then bail with a flag to list the rest *)
let tokenize_all read lexbuf diags =
  let toks = ref [] in
  let rec scan stack depth =
    match read lexbuf with
    | ERROR msg, sp ->
        Diagnostic.error_at diags sp msg;
        scan stack depth
    | t, sp -> (
        let info =
          {
            token = t;
            span = sp;
            line = lexbuf.Lexing.lex_start_p.Lexing.pos_lnum;
            depth;
          }
        in
        toks := info :: !toks;
        match Bracket_check.step diags stack t sp with
        | Bracket_check.Done -> ()
        | Bracket_check.Stray | Bracket_check.Other -> scan stack depth
        | Bracket_check.Open ->
            scan ((t, sp) :: stack) (depth + 1);
            scan stack depth)
  in
  scan [] 0;
  Array.of_list (List.rev !toks)

let replay_of (tokens : token_info array) =
  let last = Array.length tokens - 1 in
  let idx = ref 0 in
  fun () ->
    let pair = tokens.(!idx) in
    if !idx < last then incr idx;
    pair

let parse ~(diags : Diagnostic.sink)
    (read : Lexing.lexbuf -> Tokens.token * Ast.span) (lexbuf : Lexing.lexbuf) :
    Ast.module_ =
  let tokens = tokenize_all read lexbuf diags in
  let st =
    {
      tok = EOF;
      tok_span = dummy_span;
      tok_line = 1;
      tok_depth = 0;
      read = replay_of tokens;
      diags;
      no_struct_lit = false;
      prev_end = 0;
      recovered = false;
    }
  in
  advance st;
  parse_module st
