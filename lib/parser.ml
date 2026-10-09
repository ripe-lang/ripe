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
type infix = { prec : int; assoc : assoc; build : expr -> expr -> expr_desc }
type field_form = NamedField | PositionalField
type binding_context = LocalBinding | GlobalBinding
type expression_context = NormalExpression | HeaderExpression

let cur_token st = st.current.token
let cur_span st = st.current.span
let cur_pos st = Span.lo (cur_span st)
let span_from lo st = Span.make lo st.prev_end
let mk lo st desc = { desc; span = span_from lo st }
let mkt lo st tdesc = { tdesc; tspan = span_from lo st }
let closer_of = function LPAREN -> RPAREN | LBRACKET -> RBRACKET | _ -> RBRACE

let is_expr_start = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | MINUS | STAR | AMP | TILDE
  | BANG | TRUE | FALSE | NULL | SIZEOF | CAST | LPAREN | LBRACKET | IF | LBRACE
  | LOOP | WHILE | FOR | MATCH ->
      true
  | _ -> false

let is_type_start = function
  | IDENT _ | STAR | LBRACKET | FUNC | EXTERN | LPAREN -> true
  | _ -> false

let is_member_start = function IDENT _ -> true | _ -> false

let ends_in_block = function
  | Expr { desc = If _ | Match _ | While _ | For _ | Loop _ | Block _; _ }
  | Decl (LocalFunc _ | LocalStruct _ | LocalEnum _) ->
      true
  | Expr _ | Decl _ -> false

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

let opens_struct_field st =
  match (peek_token st, (peek_nth st 1).token) with
  | tok, COMMA -> is_expr_start tok
  | _ -> false

let has_abi st = match peek_token st with STRING _ -> true | _ -> false
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

let expect_decl_sep st ~what =
  match cur_token st with
  | COMMA -> advance st
  | RBRACE | EOF -> ()
  | _ ->
      Diagnostic.error (cur_span st) "expected %s separator" what
      |> Diagnostic.help ("separate " ^ what ^ "s with `,`")
      |> fail

let expect_literal_field_sep st =
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

let abi_wants_func st = function
  | NamedAbi _ ->
      Diagnostic.error (cur_span st) "expected `fn`" |> found st |> fail
  | NoAbi -> ()

let left prec build = Some { prec; assoc = Left; build }
let right prec build = Some { prec; assoc = Right; build }
let chain prec c build = Some { prec; assoc = Chain c; build }
let bin op l r = BinOp (op, l, r)
let assign op l r = Assign (op, l, r)

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
  | DOTDOT -> chain 2 Range (fun l r -> Range (l, r))
  | DOTDOTEQ -> chain 2 Range (fun l r -> RangeInclusive (l, r))
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

and binding_name st context =
  match cur_token st with
  | UNDERSCORE when context = LocalBinding ->
      let span = cur_span st in
      advance st;
      spanned (Interner.intern "_") span
  | _ -> expect_ident_span st

and binding_initializer st =
  expect st ASSIGN;
  parse_expr st

(* i32, *i32, fn (i32, i32) i32 *)
and parse_typ st =
  let lo = cur_pos st in
  match cur_token st with
  | EXTERN when not (has_abi st) ->
      Diagnostic.error (cur_span st) "expected type" |> found st |> fail
  | EXTERN ->
      advance st;
      parse_func_ptr st lo (parse_abi st)
  | STAR ->
      advance st;
      mkt lo st (Pointer (parse_typ st))
  | IDENT _ when starts_member st ->
      Diagnostic.error (cur_span st) "expected type" |> found st |> fail
  | IDENT name ->
      advance st;
      mkt lo st (Named (Interner.intern name))
  (* [N]T fixed size array, []T slice *)
  | LBRACKET ->
      advance st;
      if at st RBRACKET then begin
        advance st;
        mkt lo st (Slice (parse_typ st))
      end
      else
        let n = parse_expr st in
        expect st RBRACKET;
        mkt lo st (Array (n, parse_typ st))
  | FUNC when is_member_start (peek_token st) ->
      Diagnostic.error (cur_span st) "expected type" |> found st |> fail
  | FUNC -> parse_func_ptr st lo NoAbi
  | LPAREN ->
      advance st;
      if at st RPAREN then begin
        advance st;
        mkt lo st UnitType
      end
      else
        let inner = parse_typ st in
        expect st RPAREN;
        { inner with tspan = span_from lo st }
  | _ -> Diagnostic.error (cur_span st) "expected type" |> found st |> fail

(* x : i32, x : i32 = 42, x := 42 *)
and parse_binding st context =
  let name = binding_name st context in
  let typ =
    if peek_token st == ASSIGN then begin
      expect st COLON;
      None
    end
    else Some (typ_after st COLON)
  in
  let init = if at st ASSIGN then Some (binding_initializer st) else None in
  (name, typ, init)

(* fn (i32, i32) i32, extern "C" fn (i32) i32 *)
and parse_func_ptr st lo abi =
  expect st FUNC;
  expect st LPAREN;
  let params = comma_sep st RPAREN parse_typ in
  expect st RPAREN;
  let ret =
    if is_type_start (cur_token st) then Some (parse_typ st) else None
  in
  mkt lo st (FuncPtr (abi, params, ret))

(* The "C" of extern "C" fn exit(code: i32) never *)
and parse_abi st =
  match cur_token st with
  | STRING name ->
      let span = cur_span st in
      advance st;
      NamedAbi (spanned name span)
  | _ -> Diagnostic.error (cur_span st) "expected ABI name" |> found st |> fail

(* extern "C" *)
and parse_decl_abi st =
  if at st EXTERN then begin
    advance st;
    parse_abi st
  end
  else NoAbi

(* x: i32, y: i32 *)
and parse_fields st =
  let fields = ref [] in
  while not (at st RBRACE || at st EOF) do
    let name = expect_ident_span st in
    let typ = typ_after st COLON in
    fields := { field_name = name; field_typ = typ } :: !fields;
    expect_decl_sep st ~what:"field"
  done;
  List.rev !fields

(* struct point { x: i32, y: i32 } *)
and parse_struct_def st =
  let lo = cur_pos st in
  expect st STRUCT;
  let name = expect_ident_span st in
  expect st LBRACE;
  let fields = parse_fields st in
  expect st RBRACE;
  { struct_name = name; fields; struct_span = span_from lo st }

(* Red, Green, Blue *)
and parse_variants st =
  let variants = ref [] in
  while not (at st RBRACE || at st EOF) do
    variants := expect_ident_span st :: !variants;
    expect_decl_sep st ~what:"variant"
  done;
  List.rev !variants

(* enum Color { Red, Green, Blue } *)
and parse_enum_def st =
  let lo = cur_pos st in
  expect st ENUM;
  let name = expect_ident_span st in
  expect st LBRACE;
  let variants = parse_variants st in
  expect st RBRACE;
  { enum_name = name; variants; enum_span = span_from lo st }

(* type binop = (i32, i32) i32 *)
and parse_alias_def st =
  let lo = cur_pos st in
  expect st TYPE;
  let name = expect_ident_span st in
  let typ = typ_after st ASSIGN in
  { alias_name = name; alias_typ = typ; alias_span = span_from lo st }

(* x: i32 *)
and parse_param st =
  let lo = cur_pos st in
  let name = expect_ident_span st in
  let typ = typ_after st COLON in
  { param_name = name; param_typ = typ; param_span = span_from lo st }

(* (a: i32, b: i32), (fmt: cstr, ...) *)
and parse_params st =
  expect st LPAREN;
  let params = ref [] in
  let variadic = ref None in
  while not (at st RPAREN || at st EOF) do
    match cur_token st with
    | ELLIPSIS ->
        variadic := Some (cur_span st);
        advance st;
        if not (at st RPAREN) then
          Diagnostic.error (cur_span st) "`...` must be the last parameter"
          |> fail
    | _ -> (
        params := parse_param st :: !params;
        match cur_token st with
        | COMMA -> advance st
        | RPAREN | EOF -> ()
        | _ ->
            Diagnostic.error (cur_span st) "expected parameter separator"
            |> Diagnostic.help "separate parameters with `,`"
            |> fail)
  done;
  expect st RPAREN;
  (List.rev !params, !variadic)

(* i32 *)
and parse_ret_type st =
  match cur_token st with
  | FUNC when is_member_start (peek_token st) -> None
  | tok when not (is_type_start tok) -> None
  | _ -> Some (parse_typ st)

(* fn add(a: i32, b: i32) i32 { ... }, extern "C" fn puts(s: cstr) i32 *)
and parse_func_def st abi =
  let lo = cur_pos st in
  expect st FUNC;
  let name = expect_ident_span st in
  let params, variadic = parse_params st in
  let ret = parse_ret_type st in
  let imported = abi <> NoAbi && not (at st LBRACE) in
  let body = if imported then [] else (parse_block st).value in
  if not imported then
    begin match variadic with
    | Some span ->
        Diagnostic.error span "a function with a body cannot be variadic"
        |> Diagnostic.help "`...` only works on a declaration with no body"
        |> fail
    | None -> ()
    end;
  ( {
      func_name = name;
      params;
      ret;
      body;
      variadic = Option.is_some variadic;
      extern_abi = abi;
      func_span = span_from lo st;
    },
    imported )

(* a + b * c *)
and parse_expr ?(context = NormalExpression) ?(min_prec = 1) st =
  let lo = cur_pos st in
  (* Precedence climbing for infix ops *)
  let rec infix lhs =
    match infix_of (cur_token st) with
    | Some op when op.prec >= min_prec -> (
        let op_span = cur_span st in
        advance st;
        require_expr_start st op_span;
        let next_min_prec =
          match op.assoc with Right -> op.prec | Left | Chain _ -> op.prec + 1
        in
        let rhs = parse_expr ~context ~min_prec:next_min_prec st in
        let lhs = mk lo st (op.build lhs rhs) in
        match (op.assoc, infix_of (cur_token st)) with
        | Chain chain, Some { assoc = Chain next; _ } when chain = next ->
            let name, help =
              match chain with
              | Comparison ->
                  ( "comparison",
                    "split the chain into separate comparisons joined with `&&`"
                  )
              | Range -> ("range", "parenthesize a range if nesting is intended")
            in
            Diagnostic.error (cur_span st) "%s operators cannot be chained" name
            |> Diagnostic.label "second %s operator" name
            |> Diagnostic.secondary op_span
                 (Printf.sprintf "first %s operator" name)
            |> Diagnostic.help help |> fail
        | _ -> infix lhs)
    | _ -> lhs
  in
  infix (parse_prefix st context)

(* point { x: 1 } *)
and parse_struct_lit st lo name =
  expect st LBRACE;
  let fields = parse_struct_lit_fields st in
  expect st RBRACE;
  mk lo st (StructLit (name, fields))

(* -x *)
and parse_prefix st context =
  let lo = cur_pos st in
  let unary desc =
    let span = cur_span st in
    advance st;
    require_expr_start st span;
    let operand = parse_prefix st context in
    mk lo st (UnOp (desc, operand))
  in
  match cur_token st with
  | BANG -> unary Not
  | MINUS -> unary Neg
  | TILDE -> unary BitNot
  | AMP -> unary AddressOf
  | STAR -> unary Deref
  | _ ->
      let lhs = parse_primary st context in
      let lhs =
        match lhs.desc with
        | Ident head -> parse_path st context lhs head
        | _ -> lhs
      in
      parse_postfix st lhs

(* point, Color.Red, point { x: 1 } *)
and parse_path st context lhs head =
  let lo = Span.lo lhs.span in
  if at st DOT then begin
    advance st;
    let rec segments owners member =
      if at st DOT then begin
        advance st;
        segments (member :: owners) (expect_ident_span st)
      end
      else (List.rev owners, member)
    in
    (* The last name is the member *)
    let owners, member = segments [] (expect_ident_span st) in
    path_expr { owner = Nonempty.make (spanned head lhs.span) owners; member }
  end
  else if not (at st LBRACE) then lhs
  else
    match context with
    | NormalExpression -> parse_struct_lit st lo (spanned head lhs.span)
    | HeaderExpression when opens_struct_field st ->
        Diagnostic.error (cur_span st) "a struct literal can't go in a header"
        |> Diagnostic.label "this `{` starts the body"
        |> Diagnostic.help "wrap the literal in parentheses"
        |> fail
    | HeaderExpression -> lhs

(* x.field, arr[i], f(args) *)
and parse_postfix st (lhs : expr) =
  let lo = Span.lo lhs.span in
  match cur_token st with
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

(* (), (x) *)
and parse_paren st =
  let lo = cur_pos st in
  expect st LPAREN;
  if at st RPAREN then begin
    advance st;
    mk lo st Unit
  end
  else
    let e = parse_expr st in
    expect st RPAREN;
    e

(* 1, x, "str", foo(a, b) *)
and parse_primary st context =
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
      mk lo st (ArrayLit elems)
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
      { desc = Ident name; span = nspan }
  | STRING s ->
      advance st;
      mk lo st (String s)
  | IF -> parse_if st
  | MATCH -> parse_match st
  | LBRACE when context = HeaderExpression ->
      Diagnostic.error (cur_span st) "expected expression" |> fail
  | LBRACE ->
      let body = (parse_block st).value in
      mk lo st (Block body)
  | LOOP -> parse_loop st
  | WHILE -> parse_while st
  | FOR -> parse_for st
  | ELSE ->
      Diagnostic.error (cur_span st) "`else` without a matching `if`"
      |> found st
      |> Diagnostic.help "an `if` used as a value closes at its `}`"
      |> fail
  | _ ->
      Diagnostic.error (cur_span st) "expected expression" |> found st |> fail

(* i, 1..3, 1.., ..3, .. *)
and parse_index_arg st =
  let lo = cur_pos st in
  (* Endpoints parse above `..` so a missing one leaves the loop alone *)
  let endpoint () = parse_expr ~min_prec:3 st in
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

(* x: 3, y: 4 or 3, 4 *)
and parse_struct_lit_fields st =
  skip_semi st;
  let field_form () =
    match (cur_token st, peek_token st) with
    | IDENT _, COLON -> NamedField
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
        expect st COLON;
        (spanned (Some name.value) name.span, parse_expr st)
    | PositionalField ->
        let e = parse_expr st in
        (spanned None e.span, e)
  in
  let[@tail_mod_cons] rec go () =
    if at st RBRACE then []
    else
      let field = parse_field () in
      expect_literal_field_sep st;
      field :: go ()
  in
  go ()

(* The x < len of if x < len { } *)
and parse_header_expr st = parse_expr ~context:HeaderExpression st

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
  | IF -> Expr (parse_if st)
  | MATCH -> Expr (parse_match st)
  | WHILE -> Expr (parse_while st)
  | FOR -> Expr (parse_for st)
  | LOOP -> Expr (parse_loop st)
  | LBRACE ->
      let body = (parse_block st).value in
      Expr (mk lo st (Block body))
  | (IDENT _ | UNDERSCORE) when peek_token st == COLON ->
      let name, ann, e = parse_binding st LocalBinding in
      Expr (mk lo st (Binding (name, ann, e)))
  | BREAK ->
      advance st;
      let target = parse_loop_target st in
      if at_value_end st then Expr (mk lo st (Break (target, None)))
      else Expr (mk lo st (Break (target, Some (parse_expr st))))
  | CONTINUE ->
      advance st;
      Expr (mk lo st (Continue (parse_loop_target st)))
  | RETURN ->
      advance st;
      if at_value_end st then Expr (mk lo st (Return None))
      else Expr (mk lo st (Return (Some (parse_expr st))))
  | FUNC | STRUCT | TYPE | ENUM | EXTERN -> parse_local_decl st
  | _ -> Expr (parse_expr st)

(* type small = i32, fn helper() { } *)
and parse_local_decl st =
  let decl =
    match cur_token st with
    | STRUCT -> LocalStruct (parse_struct_def st)
    | TYPE -> LocalTypeAlias (parse_alias_def st)
    | FUNC -> LocalFunc (fst (parse_func_def st NoAbi))
    | ENUM -> LocalEnum (parse_enum_def st)
    | EXTERN ->
        Diagnostic.error (cur_span st) "`extern` must be at the top level"
        |> fail
    | _ ->
        Diagnostic.error (cur_span st) "expected local declaration"
        |> found st |> fail
  in
  Decl decl

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
      Diagnostic.error (cur_span st) "expected arm separator"
      |> Diagnostic.help "separate arms with `,`"
      |> fail
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

(* _, n, Color.Red *)
and parse_pattern st =
  let lo = cur_pos st in
  let pattern pdesc = { pdesc; pspan = span_from lo st } in
  match cur_token st with
  | UNDERSCORE ->
      advance st;
      pattern PatWild
  | IDENT name when peek_token st != DOT ->
      advance st;
      pattern (PatBind (Interner.intern name))
  | _ ->
      let e = parse_expr ~context:HeaderExpression st in
      { pdesc = PatValue e; pspan = e.span }

(* while i < len { }, while :outer c { } *)
and parse_while st =
  let lo = cur_pos st in
  expect st WHILE;
  let label = parse_loop_target st in
  let cond = parse_header_expr st in
  let body = (parse_block st).value in
  mk lo st (While (label, cond, body))

(* break :outer, loop :outer { } *)
and parse_loop_target st =
  if at st COLON then begin
    advance st;
    Some (expect_ident_span st)
  end
  else None

(* for i in 0..len { }, for :outer i in xs { } *)
and parse_for st =
  let lo = cur_pos st in
  expect st FOR;
  let label = parse_loop_target st in
  let name = expect_ident_span st in
  expect st IN;
  let iter = parse_header_expr st in
  let body = (parse_block st).value in
  mk lo st (For (label, name, iter, body))

(* loop { }, loop :outer { } *)
and parse_loop st =
  let lo = cur_pos st in
  expect st LOOP;
  let label = parse_loop_target st in
  let body = (parse_block st).value in
  mk lo st (Loop (label, body))

(* n : i32 = 0, flag : bool *)
let parse_global st =
  let lo = cur_pos st in
  let name, typ, init = parse_binding st GlobalBinding in
  Global { name; typ; init; span = span_from lo st }

(* fn f() i32 { }, extern "C" fn puts(s: cstr) i32 *)
let parse_decl st =
  let abi = parse_decl_abi st in
  begin match cur_token st with
  | STRUCT | IDENT _ | TYPE | ENUM -> abi_wants_func st abi
  | _ -> ()
  end;
  match cur_token st with
  | FUNC ->
      let fd, imported = parse_func_def st abi in
      if imported then Extern fd else Func fd
  | STRUCT -> Struct (parse_struct_def st)
  | IDENT _ -> parse_global st
  | TYPE -> TypeAlias (parse_alias_def st)
  | ENUM -> Enum (parse_enum_def st)
  | _ ->
      Diagnostic.error (cur_span st) "expected declaration" |> found st |> fail

(* n := 0; fn f() { } *)
let parse_file st =
  skip_semi st;
  let decls = ref [] in
  while not (at st EOF) do
    let decl = parse_decl st in
    decls := decl :: !decls;
    let ends_in_brace =
      match decl with
      | Func _ | Struct _ | Enum _ -> true
      | Extern _ | Global _ | TypeAlias _ -> false
    in
    if not (ends_in_brace || at st SEMI) then
      Diagnostic.error (cur_span st) "expected `;`" |> found st |> fail;
    skip_semi st
  done;
  List.rev !decls

(* The lexer already reported a bad token so we just stop *)
let parse (lex : Lexing.lexbuf -> Tokens.token * Ast.span) lexbuf =
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
