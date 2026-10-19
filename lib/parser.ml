(* SPDX-License-Identifier: Apache-2.0 *)

open Tokens
open Ast

exception ParserError of Diagnostic.t
exception Failed

type token_info = { token : token; span : span }

type state = {
  read : unit -> token_info;
  mutable current : token_info;
  mutable ahead : token_info list;
  mutable prev_end : int;
  mutable opens : (token * span) list;
}

type chain = Comparison | Range
type assoc = Left | Right | Chain of chain

type infix = {
  prec : int;
  assoc : assoc;
  build : span -> expr -> expr -> expr_desc;
}

type field_form = NamedField | PositionalField
type binding_ctx = LocalBinding | GlobalBinding
type expr_ctx = NormalExpression | HeaderExpression

let cur_token st = st.current.token
let cur_span st = st.current.span
let cur_pos st = Span.lo (cur_span st)
let span_from lo st = Span.make lo st.prev_end
let mk lo st desc = { edesc = desc; espan = span_from lo st }
let closer_of = function LPAREN -> RPAREN | LBRACKET -> RBRACKET | _ -> RBRACE

let is_expr_start = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | MINUS | AMP | TILDE | BANG
  | TRUE | FALSE | NULL | SIZEOF | CAST | LPAREN | LBRACKET | IF | LBRACE | LOOP
  | WHILE | FOR | MATCH | FUNC | STRUCT | ENUM | DOT ->
      true
  | _ -> false

let is_type_start = function
  | IDENT _ | STAR | LBRACKET | EXTERN | LPAREN | UNDERSCORE -> true
  | _ -> false

let ends_in_block e =
  match e.edesc with
  | If _ | Match _ | For _ | Loop _ | Block _ -> true
  | _ -> false

let next_token st last = match last.token with EOF -> last | _ -> st.read ()

let advance st =
  st.prev_end <- Span.hi st.current.span;
  begin match st.current.token with
  | LPAREN | LBRACKET | LBRACE ->
      st.opens <- (st.current.token, st.current.span) :: st.opens
  | RPAREN | RBRACKET | RBRACE ->
      begin match st.opens with _ :: rest -> st.opens <- rest | [] -> ()
      end
  | _ -> ()
  end;
  match st.ahead with
  | info :: rest ->
      st.ahead <- rest;
      st.current <- info
  | [] -> st.current <- next_token st st.current

let peek_nth st n =
  let rec go ahead last n =
    match ahead with
    | info :: rest -> if n = 0 then info else go rest info (n - 1)
    | [] ->
        let info = next_token st last in
        st.ahead <- st.ahead @ [ info ];
        if n = 0 then info else go [] info (n - 1)
  in
  go st.ahead st.current n

let peek_token st = (peek_nth st 0).token
let at st t = cur_token st == t
let found st d = Diagnostic.found (show_found_token (cur_token st)) d
let fail d = raise (ParserError d)

let require_expr_start st span =
  if not (is_expr_start (cur_token st)) then
    Diagnostic.error span "expected expression" |> fail

let starts_member st =
  match (cur_token st, peek_token st) with IDENT _, COLON -> true | _ -> false

let at_value_end st = at st SEMI || at st RBRACE || at st EOF || at st COMMA

let skip_semi st =
  while at st SEMI do
    advance st
  done

let has_arm st = not (at st RBRACE || at st EOF)

let expect st t =
  if at st t then advance st
  else
    let d =
      Diagnostic.error (cur_span st) "expected `%s`" (show_token t) |> found st
    in
    match st.opens with
    | (opener, span) :: _ when closer_of opener == t ->
        Diagnostic.secondary span
          (Printf.sprintf "to match this `%s`" (show_token opener))
          d
        |> fail
    | _ -> fail d

let expect_ident st =
  match cur_token st with
  | IDENT s ->
      advance st;
      Interner.intern s
  | _ ->
      Diagnostic.error (cur_span st) "expected identifier" |> found st |> fail

let expect_ident_span st =
  let span = cur_span st in
  spanned (expect_ident st) span

let expect_decl_sep st what =
  match cur_token st with
  | COMMA -> advance st
  | RBRACE | EOF -> ()
  | _ ->
      Diagnostic.error (cur_span st) "expected `,` between %ss" what
      |> found st |> fail

let expect_lit_field_sep st =
  match cur_token st with
  | COMMA -> advance st
  | RBRACE -> ()
  | _ ->
      Diagnostic.error (cur_span st) "expected `,` between fields"
      |> found st |> fail

(* item (, item)* with an optional trailing comma before stop *)
let comma_sep st stop parse_one =
  let[@tail_mod_cons] rec rest () =
    if at st COMMA then begin
      advance st;
      if at st stop then rest ()
      else
        let item = parse_one st in
        item :: rest ()
    end
    else begin
      if at st SEMI then expect st stop;
      []
    end
  in
  if at st stop then []
  else
    let first = parse_one st in
    first :: rest ()

let left prec build = Some { prec; assoc = Left; build }
let right prec build = Some { prec; assoc = Right; build }
let chain prec c build = Some { prec; assoc = Chain c; build }
let bin op span l r = BinOp (spanned op span, l, r)
let assign op span l r = Assign (spanned op span, l, r)

let infix_of = function
  | ASSIGN -> right 1 (assign None)
  | PLUS_ASSIGN -> right 1 (assign (Some Add))
  | MINUS_ASSIGN -> right 1 (assign (Some Sub))
  | STAR_ASSIGN -> right 1 (assign (Some Mul))
  | SLASH_ASSIGN -> right 1 (assign (Some Div))
  | PERCENT_ASSIGN -> right 1 (assign (Some Mod))
  | AMP_ASSIGN -> right 1 (assign (Some BitAnd))
  | PIPE_ASSIGN -> right 1 (assign (Some BitOr))
  | CARET_ASSIGN -> right 1 (assign (Some BitXor))
  | LSHIFT_ASSIGN -> right 1 (assign (Some Lshift))
  | RSHIFT_ASSIGN -> right 1 (assign (Some Rshift))
  | AND_ASSIGN -> right 1 (assign (Some And))
  | OR_ASSIGN -> right 1 (assign (Some Or))
  | DOTDOT -> chain 2 Range (fun _ l r -> Range (l, r))
  | DOTDOTEQ -> chain 2 Range (fun _ l r -> RangeInclusive (l, r))
  | OR -> left 3 (bin Or)
  | AND -> left 4 (bin And)
  | EQ -> chain 5 Comparison (bin Eq)
  | NEQ -> chain 5 Comparison (bin Neq)
  | LT -> chain 5 Comparison (bin Lt)
  | GT -> chain 5 Comparison (bin Gt)
  | LTE -> chain 5 Comparison (bin Lte)
  | GTE -> chain 5 Comparison (bin Gte)
  | PIPE -> left 6 (bin BitOr)
  | CARET -> left 7 (bin BitXor)
  | AMP -> left 8 (bin BitAnd)
  | LSHIFT -> left 9 (bin Lshift)
  | RSHIFT -> left 9 (bin Rshift)
  | PLUS -> left 10 (bin Add)
  | MINUS -> left 10 (bin Sub)
  | STAR -> left 11 (bin Mul)
  | SLASH -> left 11 (bin Div)
  | PERCENT -> left 11 (bin Mod)
  | _ -> None

(* The missing type belongs to the preceding separator *)
let rec typ_after st tok =
  let span = cur_span st in
  expect st tok;
  if starts_member st then Diagnostic.error span "expected type" |> fail;
  parse_typ st

and binding_name st ctx =
  match cur_token st with
  | UNDERSCORE when ctx = LocalBinding ->
      let span = cur_span st in
      advance st;
      spanned (Interner.intern "_") span
  | _ -> expect_ident_span st

(* i32, *i32, []i32, (i32) -> i32, _ *)
and parse_typ st =
  if not (is_type_start (cur_token st)) then
    Diagnostic.error (cur_span st) "expected type" |> found st |> fail;
  parse_prefix st NormalExpression

(* x : i32, x : i32 = 42, x : _ = 42, x := 42, N :: 5, N : i32 : 5, N : _ : 5 *)
and parse_binding st ctx =
  let lo = cur_pos st in
  let name = binding_name st ctx in
  expect st COLON;
  let typ =
    match cur_token st with
    | COLON | ASSIGN -> { edesc = Hole; espan = name.span }
    | _ -> parse_typ st
  in
  let binder = if at st COLON then CompileTime else RunTime in
  let form =
    match (typ.edesc, cur_token st) with
    | Hole, (COLON | ASSIGN) ->
        advance st;
        Inferred (parse_expr st)
    | _, (COLON | ASSIGN) ->
        advance st;
        Typed (typ, parse_expr st)
    | _ -> Declared typ
  in
  { bind_name = name; binder; bind_form = form; bind_span = span_from lo st }

(* "C" *)
and parse_abi st =
  match cur_token st with
  | STRING name ->
      let span = cur_span st in
      advance st;
      spanned name span
  | _ -> Diagnostic.error (cur_span st) "expected ABI name" |> found st |> fail

(* x: i32, y: i32 *)
and parse_fields st =
  let fields = ref [] in
  while not (at st RBRACE || at st EOF) do
    let name = expect_ident_span st in
    let typ = typ_after st COLON in
    fields := { field_name = name; field_typ = typ } :: !fields;
    expect_decl_sep st "field"
  done;
  List.rev !fields

(* Red, Circle(i32), Rectangle(Point, Point) *)
and parse_variants st =
  let variants = ref [] in
  while not (at st RBRACE || at st EOF) do
    variants := expect_ident_span st :: !variants;
    expect_decl_sep st ~what:"variant"
  done;
  List.rev !variants

(* x: i32, x: _, x *)
and parse_param st =
  let lo = cur_pos st in
  let name = expect_ident_span st in
  let typ =
    if at st COLON then typ_after st COLON
    else { edesc = Hole; espan = name.span }
  in
  { param_name = name; param_typ = typ; param_span = span_from lo st }

(* (a: i32, b: i32) *)
and parse_params st =
  expect st LPAREN;
  let params = ref [] in
  while not (at st RPAREN || at st EOF) do
    params := parse_param st :: !params;
    match cur_token st with
    | COMMA -> advance st
    | RPAREN | EOF -> ()
    | _ ->
        Diagnostic.error (cur_span st) "expected `,` between parameters"
        |> found st |> fail
  done;
  expect st RPAREN;
  List.rev !params

(* fn(a: i32) -> i32 => a, fn { }, extern "C" fn(p: Pair) -> i32 { p.a } *)
and parse_lambda st abi lo =
  let params = if at st LPAREN then parse_params st else [] in
  let ret =
    if at st ARROW then begin
      advance st;
      Some (parse_typ st)
    end
    else None
  in
  let body =
    if at st FATARROW then begin
      advance st;
      [ parse_expr st ]
    end
    else (parse_block st).value
  in
  {
    lambda_params = params;
    lambda_ret = ret;
    lambda_body = body;
    lambda_abi = abi;
    lambda_span = span_from lo st;
  }

(* a + b * c *)
and parse_expr st = parse_infix st NormalExpression 1

(* The b * c of a + b * c *)
and parse_infix st ctx min_prec =
  let lo = cur_pos st in
  (* This is precedence climbing for infix ops *)
  let rec infix lhs =
    match infix_of (cur_token st) with
    | Some op when op.prec >= min_prec -> (
        let op_span = cur_span st in
        advance st;
        require_expr_start st op_span;
        let next_min_prec =
          match op.assoc with Right -> op.prec | Left | Chain _ -> op.prec + 1
        in
        let rhs = parse_infix st ctx next_min_prec in
        let lhs = mk lo st (op.build op_span lhs rhs) in
        match (op.assoc, infix_of (cur_token st)) with
        | Chain chain, Some { assoc = Chain next; _ } when chain = next ->
            let name, help =
              match chain with
              | Comparison -> ("comparison", "join them with `&&`")
              | Range -> ("range", "parenthesize the inner range")
            in
            Diagnostic.error (cur_span st) "chained %s" name
            |> Diagnostic.label "second %s operator" name
            |> Diagnostic.secondary op_span
                 (Printf.sprintf "first %s operator" name)
            |> Diagnostic.help help |> fail
        | _ -> infix lhs)
    | _ -> lhs
  in
  infix (parse_prefix st ctx)

(* Point.{ x: 1 } *)
and parse_struct_lit st lo name =
  expect st LBRACE;
  let fields = parse_struct_lit_fields st in
  expect st RBRACE;
  mk lo st (StructLit (name, fields))

(* -x *)
and parse_prefix st ctx =
  let lo = cur_pos st in
  let unop desc =
    let span = cur_span st in
    advance st;
    require_expr_start st span;
    let operand = parse_prefix st ctx in
    mk lo st (UnOp (desc, operand))
  in
  match cur_token st with
  | BANG -> unop Not
  | MINUS -> unop Neg
  | TILDE -> unop BitNot
  | AMP -> unop AddressOf
  | STAR ->
      advance st;
      mk lo st (PointerType (parse_prefix st ctx))
  | _ ->
      let lhs = parse_primary st ctx in
      let lhs =
        match lhs.edesc with Ident head -> parse_path st lhs head | _ -> lhs
      in
      parse_postfix st lhs

(* point, Color.Red, Point.{ x: 1 } *)
and parse_path st lhs head =
  let lo = Span.lo lhs.espan in
  if at st DOT && peek_token st == LBRACE then begin
    advance st;
    parse_struct_lit st lo (Some (spanned head lhs.espan))
  end
  else if at st DOT && peek_token st != STAR then begin
    advance st;
    let rec segments owners member =
      if at st DOT && peek_token st != STAR then begin
        advance st;
        segments (member :: owners) (expect_ident_span st)
      end
      else (List.rev owners, member)
    in
    (* The last name is the member *)
    let owners, member = segments [] (expect_ident_span st) in
    path_expr { owner = Nonempty.make (spanned head lhs.espan) owners; member }
  end
  else lhs

(* x.field, p.*, arr[i], f(args) *)
and parse_postfix st lhs =
  let lo = Span.lo lhs.espan in
  match cur_token st with
  | DOT when peek_token st == STAR ->
      advance st;
      advance st;
      parse_postfix st (mk lo st (UnOp (Deref, lhs)))
  | DOT ->
      advance st;
      let name = expect_ident_span st in
      parse_postfix st (mk lo st (FieldAccess (lhs, name)))
  | LBRACKET ->
      advance st;
      let idx = parse_index_arg st in
      expect st RBRACKET;
      parse_postfix st (mk lo st (Index (lhs, idx)))
  | LPAREN ->
      advance st;
      let args = parse_comma_list st RPAREN in
      expect st RPAREN;
      parse_postfix st (mk lo st (Call (lhs, args)))
  | _ -> lhs

(* (), (x), (i32, *u8) -> i32, extern "C" (i32) -> i32, (cstr, ..) -> i32 *)
and parse_paren st lo abi =
  expect st LPAREN;
  let elems =
    comma_sep st RPAREN (fun st ->
        if at st DOTDOT then begin
          let dots_lo = cur_pos st in
          advance st;
          mk dots_lo st RangeFull
        end
        else parse_expr st)
  in
  expect st RPAREN;
  match (abi, elems, cur_token st) with
  | _, _, ARROW ->
      advance st;
      let ret = parse_typ st in
      mk lo st (FuncType (abi, elems, ret))
  | NoAbi, [], _ -> mk lo st Unit
  | NoAbi, [ e ], _ -> e
  | _ -> Diagnostic.error (cur_span st) "expected `->`" |> found st |> fail

(* 1, x, "str", foo(a, b) *)
and parse_primary st ctx =
  let lo = cur_pos st in
  match cur_token st with
  | INT (n, suf) ->
      advance st;
      mk lo st (Int (n, suf))
  | CHAR c ->
      advance st;
      mk lo st (Char c)
  | FLOAT (f, suf) ->
      advance st;
      mk lo st (Float (f, suf))
  | TRUE ->
      advance st;
      mk lo st (Bool true)
  | FALSE ->
      advance st;
      mk lo st (Bool false)
  | NULL ->
      advance st;
      mk lo st Null
  | LPAREN -> parse_paren st
  (* [1, 2, 3] array literal *)
  | LBRACKET ->
      advance st;
      let elems = parse_comma_list st RBRACKET in
      expect st RBRACKET;
      match (elems, is_type_start (cur_token st)) with
      | [], true -> mk lo st (SliceType (parse_typ st))
      | [ n ], true -> mk lo st (ArrayType (n, parse_typ st))
      | _ -> mk lo st (ArrayLit elems))
  | FUNC ->
      advance st;
      mk lo st (Lambda (parse_lambda st NoAbi lo))
  | STRUCT ->
      advance st;
      expect st LBRACE;
      let fields = parse_fields st in
      expect st RBRACE;
      mk lo st (StructType fields)
  | ENUM ->
      advance st;
      expect st LBRACE;
      let variants = parse_variants st in
      expect st RBRACE;
      mk lo st (EnumType variants)
  (* sizeof(x) *)
  | SIZEOF ->
      advance st;
      expect st LPAREN;
      let t = parse_typ st in
      expect st RPAREN;
      mk lo st (SizeOf t)
  (* cast(i64, x) *)
  | CAST ->
      advance st;
      expect st LPAREN;
      let t = parse_typ st in
      expect st COMMA;
      let e = parse_expr st in
      expect st RPAREN;
      mk lo st (Cast (t, e))
  | IDENT name ->
      let name = Interner.intern name in
      let nspan = cur_span st in
      advance st;
      { edesc = Ident name; espan = nspan }
  | STRING s ->
      advance st;
      mk lo st (String s)
  | IF -> parse_if st
  | MATCH -> parse_match st
  | LBRACE when ctx = HeaderExpression ->
      Diagnostic.error (cur_span st) "expected expression" |> found st |> fail
  | LBRACE ->
      let body = (parse_block st).value in
      mk lo st (Block body)
  | LOOP -> parse_loop st
  | WHILE -> parse_while st
  | FOR -> parse_for st
  | ELSE ->
      Diagnostic.error (cur_span st) "`else` without `if`"
      |> found st
      |> Diagnostic.help "an `if` value ends at its `}`"
      |> fail
  | _ ->
      Diagnostic.error (cur_span st) "expected expression" |> found st |> fail

(* i, 1..3, 1.., ..3, .. *)
and parse_index_arg st =
  let lo = cur_pos st in
  (* The endpoints parse above `..` so a missing one leaves the loop alone *)
  let endpoint () = parse_infix st NormalExpression 3 in
  match cur_token st with
  | DOTDOT ->
      advance st;
      if at st RBRACKET then mk lo st RangeFull
      else mk lo st (RangeTo (endpoint ()))
  | DOTDOTEQ ->
      advance st;
      mk lo st (RangeToInclusive (endpoint ()))
  | _ -> (
      let first = endpoint () in
      match cur_token st with
      | DOTDOT ->
          advance st;
          if at st RBRACKET then mk lo st (RangeFrom first)
          else mk lo st (Range (first, endpoint ()))
      | DOTDOTEQ ->
          advance st;
          mk lo st (RangeInclusive (first, endpoint ()))
      | _ -> first)

(* a, b, c *)
and parse_comma_list st stop = comma_sep st stop (fun st -> parse_expr st)

(* x = 3, y = 4 or 3, 4 *)
and parse_struct_lit_fields st =
  skip_semi st;
  let field_form () =
    match (cur_token st, peek_token st) with
    | IDENT _, ASSIGN -> NamedField
    | _ -> PositionalField
  in
  let form = field_form () in
  let wanted =
    match form with NamedField -> "named" | PositionalField -> "positional"
  in
  let parse_field () =
    (* A token that starts no field at all gets the normal parse error *)
    if is_expr_start (cur_token st) && field_form () <> form then
      Diagnostic.error (cur_span st) "mixed struct fields"
      |> Diagnostic.label "expected a %s field" wanted
      |> fail;
    match form with
    | NamedField ->
        let name = expect_ident_span st in
        expect st ASSIGN;
        (spanned (Some name.value) name.span, parse_expr st)
    | PositionalField ->
        let e = parse_expr st in
        (spanned None e.espan, e)
  in
  let[@tail_mod_cons] rec go () =
    if at st RBRACE then []
    else
      let field = parse_field () in
      expect_lit_field_sep st;
      field :: go ()
  in
  go ()

(* The x < len of if x < len { } *)
and parse_header_expr st = parse_infix st HeaderExpression 1

(* { return a + b }  *)
and parse_block st =
  let lo = cur_pos st in
  expect st LBRACE;
  let body = parse_stmts st in
  expect st RBRACE;
  spanned body (span_from lo st)

(* stmt; stmt *)
and parse_stmts st =
  let[@tail_mod_cons] rec go () =
    if at st EOF || at st RBRACE then []
    else
      let s = parse_stmt st in
      if not (at st SEMI || at st RBRACE || at st EOF || ends_in_block s) then
        Diagnostic.error (cur_span st) "expected `;`" |> found st |> fail;
      skip_semi st;
      s :: go ()
  in
  skip_semi st;
  go ()

(* n := 1, if c { }, return x *)
and parse_stmt st =
  let lo = cur_pos st in
  match cur_token st with
  | IF -> parse_if st
  | MATCH -> parse_match st
  | WHILE -> parse_while st
  | FOR -> parse_for st
  | LOOP -> parse_loop st
  | LBRACE ->
      let body = (parse_block st).value in
      mk lo st (Block body)
  | (IDENT _ | UNDERSCORE) when peek_token st == COLON ->
      let b = parse_binding st LocalBinding in
      mk lo st (Binding b)
  | BREAK ->
      advance st;
      let target = parse_loop_target st in
      if at_value_end st then mk lo st (Break (target, None))
      else mk lo st (Break (target, Some (parse_expr st)))
  | CONTINUE ->
      advance st;
      mk lo st (Continue (parse_loop_target st))
  | RETURN ->
      advance st;
      if at_value_end st then mk lo st (Return None)
      else mk lo st (Return (Some (parse_expr st)))
  | _ -> parse_expr st

(* if x < 0 { return lo } else if x > 0 { 1 } else { 0 } *)
and parse_if st =
  let lo = cur_pos st in
  expect st IF;
  let cond = parse_header_expr st in
  let body = parse_block st in
  let elseifs, else_body = parse_elseifs st [] in
  mk lo st (If ((cond, body) :: elseifs, else_body))

(* else if x > 0 { 1 } else { 0 } *)
and parse_elseifs st acc =
  if not (at st ELSE) then (List.rev acc, None)
  else begin
    advance st;
    match cur_token st with
    | IF ->
        advance st;
        let cond = parse_header_expr st in
        let body = parse_block st in
        parse_elseifs st ((cond, body) :: acc)
    | _ -> (List.rev acc, Some (parse_block st))
  end

(* match c { Color.Red => 0, _ => 1 } *)
and parse_match st =
  let lo = cur_pos st in
  expect st MATCH;
  let scrutinee = parse_header_expr st in
  parse_arms st lo scrutinee

(* { Color.Red => 0, _ => { 1 } } *)
and parse_arms st lo scrutinee =
  expect st LBRACE;
  let arms = ref [] in
  while has_arm st do
    let arm = parse_arm st in
    arms := arm :: !arms;
    if at st COMMA then advance st
    else if
      not
        (at st RBRACE || at st EOF
        || List.for_all ends_in_block arm.arm_body.value)
    then
      Diagnostic.error (cur_span st) "expected `,` between arms"
      |> found st |> fail
  done;
  expect st RBRACE;
  mk lo st (Match (scrutinee, List.rev !arms))

(* Color.Red => 0 *)
and parse_arm st =
  let lo = cur_pos st in
  let pat = parse_pattern st in
  expect st FATARROW;
  let body_lo = cur_pos st in
  let body = spanned [ parse_stmt st ] (span_from body_lo st) in
  { pat; arm_body = body; arm_span = span_from lo st }

(* _, n, Color.Red, .Red, .Circle(r), Shape.Rectangle(a, _) *)
and parse_pattern st =
  let lo = cur_pos st in
  let pattern pdesc = { pdesc; pspan = span_from lo st } in
  match cur_token st with
  | UNDERSCORE ->
      advance st;
      pattern PatWild
  | IDENT name when not (List.mem (peek_token st) [ DOT; LPAREN ]) ->
      advance st;
      pattern (PatBind (Interner.intern name))
  | _ ->
      let e = parse_header_expr st in
      { pdesc = PatValue e; pspan = e.espan }

(* while i < len { }, while @outer c { } *)
and parse_while st =
  let lo = cur_pos st in
  expect st WHILE;
  let label = parse_loop_target st in
  let cond = parse_header_expr st in
  let body = parse_block st in
  let stop = { edesc = Break (None, None); espan = cond.espan } in
  let check = If ([ (cond, body) ], Some (spanned [ stop ] cond.espan)) in
  mk lo st (Loop (label, WhileLoop, [ { edesc = check; espan = cond.espan } ]))

(* break @outer, loop @outer { } *)
and parse_loop_target st =
  if at st AT then begin
    advance st;
    Some (expect_ident_span st)
  end
  else None

(* for i := 0..len { }, for @outer i := xs { } *)
and parse_for st =
  let lo = cur_pos st in
  expect st FOR;
  let label = parse_loop_target st in
  let name = expect_ident_span st in
  expect st COLON;
  expect st ASSIGN;
  let iter = parse_header_expr st in
  let body = (parse_block st).value in
  mk lo st (For (label, name, iter, body))

(* loop { }, loop @outer { } *)
and parse_loop st =
  let lo = cur_pos st in
  expect st LOOP;
  let label = parse_loop_target st in
  let body = (parse_block st).value in
  mk lo st (Loop (label, PlainLoop, body))

(* main :: fn { }, puts : (cstr) -> i32 : extern "C" *)
let parse_decl st =
  match cur_token st with
  | IDENT _ -> Bind (parse_binding st GlobalBinding)
  | _ ->
      Diagnostic.error (cur_span st) "expected declaration" |> found st |> fail

(* n := 0; main :: fn { }; *)
let parse_file st =
  skip_semi st;
  let decls = ref [] in
  while not (at st EOF) do
    decls := parse_decl st :: !decls;
    if not (at st SEMI) then
      Diagnostic.error (cur_span st) "expected `;`" |> found st |> fail;
    skip_semi st
  done;
  List.rev !decls

(* The lexer already reported a bad token so we just stop *)
let parse lex lexbuf =
  let read () =
    match lex lexbuf with
    | ERROR _, _ -> raise Failed
    | token, span -> { token; span }
  in
  let current = read () in
  try
    parse_file
      { read; current; ahead = []; prev_end = Span.hi Span.dummy; opens = [] }
  with ParserError d ->
    Diagnostic.emit d;
    raise Failed
