(* SPDX-License-Identifier: Apache-2.0 *)

open Tokens
open Ast

exception ParserError of Diagnostic.t

type token_info = { token : token; span : span; line : int }

type state = {
  read : unit -> token_info;
  mutable current : token_info;
  mutable ahead : token_info list;
  mutable prev_end : int;
  diags : Diagnostic.sink;
}

type chain = Comparison | Range
type assoc = Left | Right | Chain of chain
type infix = { prec : int; assoc : assoc; build : expr -> expr -> expr_desc }
type field_form = NamedField | PositionalField
type binding_context = LocalBinding | GlobalBinding
type expression_context = NormalExpression | HeaderExpression
type statement_context = BlockStatement | MatchArmBody

type item =
  | HeaderItem of module_header
  | ImportItem of import
  | DeclItem of decl

let cur_token st = st.current.token
let cur_line st = st.current.line
let cur_span st = st.current.span
let cur_pos st = Span.lo (cur_span st)
let span_from lo st = Span.make lo st.prev_end
let mk lo st desc = { desc; span = span_from lo st }
let mkt lo st tdesc = { tdesc; tspan = span_from lo st }

let recovery_span st d =
  Option.value (Diagnostic.primary d) ~default:(cur_span st)

let is_error_expr e = match e.desc with ErrorExpr -> true | _ -> false
let is_closer = function RPAREN | RBRACKET | RBRACE -> true | _ -> false

let is_expr_start = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | TRUE | FALSE | NULL | SIZEOF | LPAREN | LBRACKET | UNDEFINED
  | IF | LBRACE | LOOP | MATCH | ERROR _ ->
      true
  | _ -> false

let is_type_start = function
  | IDENT _ | STAR | LBRACKET | FUNC | EXTERN | LPAREN | ERROR _ -> true
  | _ -> false

let is_semi = function AUTOSEMI | SEMI -> true | _ -> false

let is_ambiguous_continuation = function
  | PLUS | MINUS | STAR | AMP -> true
  | _ -> false

let is_dereference_assignment token e =
  match e.desc with Assign _ -> token == STAR | _ -> false

let is_stmt_keyword = function
  | CONST | VAR | RETURN | IF | WHILE | FOR | BREAK | CONTINUE | LOOP | MATCH ->
      true
  | _ -> false

let is_stmt_start tok = is_expr_start tok || is_stmt_keyword tok

let is_item_start = function
  | FUNC | EXTERN | STRUCT | PUBLIC | TYPE | IMPORT | CONST | VAR | ENUM -> true
  | _ -> false

let is_member_start = function IDENT _ -> true | _ -> false

let is_next_line_start st line is_start =
  cur_line st > line && is_start (cur_token st)

let is_member_stop st =
  is_closer (cur_token st) || is_stmt_keyword (cur_token st)

let next_token st last = match last.token with EOF -> last | _ -> st.read ()

let advance st =
  st.prev_end <- Span.hi st.current.span;
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

let loop_lo st label =
  match label with
  | Some (l : loop_label) -> Span.lo l.span
  | None -> cur_pos st

let found st d = Diagnostic.found (show_found_token (cur_token st)) d
let fail d = raise (ParserError d)

let require_expr_start st span =
  if not (is_expr_start (cur_token st)) then
    Diagnostic.error span "expected expression" |> fail

let at_value_end st =
  is_semi (cur_token st) || at st RBRACE || at st EOF || at st COMMA

let skip_semi st =
  while is_semi (cur_token st) do
    advance st
  done

(* The next field must keep its name *)
let starts_member st =
  match (cur_token st, peek_token st) with IDENT _, COLON -> true | _ -> false

(* A broken field should not turn a literal into a block *)
let opens_struct_lit st =
  let tok = peek_token st in
  not (is_stmt_start tok && not (is_expr_start tok))

let opens_struct_field st =
  match (peek_token st, (peek_nth st 1).token) with
  | IDENT _, COLON -> (
      match (peek_nth st 2).token with WHILE | FOR | LOOP -> false | _ -> true)
  | tok, COMMA -> is_expr_start tok
  | _ -> false

(* A cast names a type and []i32 or *[2]i32 can't be read as an expression *)
let rec array_type st depth index =
  let info = peek_nth st index in
  match info.token with
  | EOF -> false
  | LBRACKET -> array_type st (depth + 1) (index + 1)
  | RBRACKET when depth = 1 -> (
      let next = peek_nth st (index + 1) in
      match next.token with
      | LBRACKET -> array_type st 1 (index + 2)
      | IDENT _ | STAR | FUNC | EXTERN -> true
      | _ -> false)
  | RBRACKET -> array_type st (depth - 1) (index + 1)
  | _ -> array_type st depth (index + 1)

let rec after_stars st index =
  match (peek_nth st index).token with
  | STAR -> after_stars st (index + 1)
  | LBRACKET | FUNC | EXTERN -> true
  | _ -> false

let opens_type st =
  match cur_token st with
  | FUNC | EXTERN -> true
  | STAR -> after_stars st 0
  | LBRACKET -> array_type st 1 0
  | _ -> false

let has_abi st = match peek_token st with STRING _ -> true | _ -> false

(* A missing separator should not hide a declaration *)
let names_decl tok name =
  match (tok, name) with
  | (FUNC | STRUCT | ENUM | TYPE | CONST | VAR | IMPORT), IDENT _
  | EXTERN, STRING _
  | PUBLIC, (FUNC | STRUCT | ENUM | TYPE | CONST | VAR | IMPORT | EXTERN) ->
      true
  | _ -> false

let starts_item st = names_decl (cur_token st) (peek_token st)

let recover_statement st line =
  let at_boundary () =
    at st EOF || at st RBRACE
    || is_semi (cur_token st)
    || (cur_line st > line && (is_stmt_start (cur_token st) || starts_item st))
  in
  while not (at_boundary ()) do
    advance st
  done

let skip_line st line stops =
  while
    not
      (at st EOF || at st RBRACE
      || is_semi (cur_token st)
      || cur_line st > line
      || List.exists (at st) stops)
  do
    advance st
  done

let skip_to_close st =
  while not (at st EOF || at st RPAREN || at st LBRACE) do
    advance st
  done

let has_arm st = not (at st RBRACE || at st EOF)

let report st d =
  let duplicate =
    match cur_token st with
    | ERROR _ -> Diagnostic.primary d = Some (cur_span st)
    | _ -> false
  in
  if not duplicate then begin
    Diagnostic.emit st.diags d
  end

let recover_expr st d =
  report st d;
  { desc = ErrorExpr; span = recovery_span st d }

let expect st t =
  if at st t then advance st
  else
    Diagnostic.error (cur_span st) "expected `%s`" (show_token t)
    |> found st |> fail

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

let expect_decl_name st =
  let span = cur_span st in
  ident (expect_ident st) span

(* The bad separator should only be reported once *)
let expect_decl_sep st ~what =
  match cur_token st with
  | AUTOSEMI | SEMI -> skip_semi st
  | RBRACE | EOF -> ()
  | tok ->
      if not (is_member_stop st) then begin
        Diagnostic.error (cur_span st) "expected %s separator" what
        |> Diagnostic.help ("separate " ^ what ^ "s with a newline or `;`")
        |> report st;
        if not (is_member_start tok) then advance st
      end

let expect_literal_field_sep st =
  match cur_token st with
  | COMMA -> advance st
  | AUTOSEMI | SEMI ->
      Diagnostic.error (cur_span st) "missing `,` before newline" |> fail
  | RBRACE -> ()
  | _ ->
      Diagnostic.error (cur_span st) "expected `,` between fields"
      |> found st |> fail

(* A closer that belongs to somebody else ends the list where it stands *)
let ends_list st stop = at st stop || is_closer (cur_token st)

(* item (, item)* with an optional trailing comma before stop *)
let comma_sep st stop parse_one =
  let[@tail_mod_cons] rec rest () =
    if at st COMMA then begin
      advance st;
      if ends_list st stop then rest ()
      else
        let item = parse_one st in
        item :: rest ()
    end
    else begin
      if is_semi (cur_token st) then
        Diagnostic.error (cur_span st) "missing `,` before newline" |> fail;
      []
    end
  in
  if ends_list st stop then []
  else
    let first = parse_one st in
    first :: rest ()

let recover_declaration st d =
  begin match Diagnostic.primary d with
  | Some span when cur_pos st = Span.lo span && not (is_closer (cur_token st))
    ->
      advance st
  | _ -> ()
  end;
  let nesting = ref 0 in
  let at_boundary () =
    at st EOF
    || (!nesting = 0 && starts_item st && not (is_stmt_start (cur_token st)))
  in
  while not (at_boundary ()) do
    begin match cur_token st with
    | LPAREN | LBRACKET | LBRACE -> incr nesting
    | (RPAREN | RBRACKET | RBRACE) when !nesting > 0 -> decr nesting
    | _ -> ()
    end;
    advance st
  done

let abi_wants_func st = function
  | NamedAbi _ ->
      Diagnostic.error (cur_span st) "expected `func`" |> found st |> report st
  | NoAbi | AbiError -> ()

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
  let line = cur_line st in
  expect st tok;
  if cur_line st > line || starts_member st then
    Diagnostic.error span "expected type" |> fail;
  parse_typ st

and binding_name st context =
  match cur_token st with
  | UNDERSCORE when context = LocalBinding ->
      let span = cur_span st in
      advance st;
      spanned (Interner.intern "_") span
  | _ -> expect_ident_span st

and binding_annotation st =
  match (cur_token st, peek_token st) with
  | COLON, _ ->
      let line = cur_line st in
      Some
        (try typ_after st COLON
         with ParserError d ->
           report st d;
           skip_line st line [ ASSIGN ];
           error_typ (recovery_span st d))
  | IDENT _, (ASSIGN | SEMI | AUTOSEMI | RBRACE | EOF | DOT) ->
      Diagnostic.error (cur_span st) "expected `:`" |> found st |> report st;
      Some (parse_typ st)
  | _ -> None

and binding_initializer st =
  let span = cur_span st in
  let line = cur_line st in
  try
    expect st ASSIGN;
    if cur_line st > line then require_expr_start st span;
    parse_expr st
  with ParserError d -> recover_expr st d

and expression_statement st context lo =
  let first = parse_expr st in
  match context with
  | BlockStatement when at st COMMA -> Expr (parse_pair_assign st lo first)
  | BlockStatement | MatchArmBody -> Expr first

(* i32, *i32, func (i32, i32) i32 *)
and parse_typ st =
  let lo = cur_pos st in
  match cur_token st with
  | ERROR _ ->
      advance st;
      mkt lo st ErrorType
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
      let rec path modules base =
        if at st DOT then begin
          advance st;
          path (base :: modules) (expect_ident st)
        end
        else mkt lo st (Named (List.rev modules, base))
      in
      path [] (Interner.intern name)
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

(* const N: i32 = 4, var x: i32, var x = 42, var x *)
and parse_binding st context =
  let kind =
    match cur_token st with
    | CONST ->
        advance st;
        Const
    | VAR ->
        advance st;
        Var
    | _ ->
        Diagnostic.error (cur_span st) "expected `const` or `var`"
        |> found st |> fail
  in
  let name = binding_name st context in
  let typ = binding_annotation st in
  (* Only var may omit the value *)
  let init =
    if at st ASSIGN || (context = LocalBinding && kind = Const) then
      Some (binding_initializer st)
    else None
  in
  (kind, name, typ, init)

(* func (i32, i32) i32, extern "C" func (i32) i32 *)
and parse_func_ptr st lo abi =
  try
    expect st FUNC;
    expect st LPAREN;
    let params = comma_sep st RPAREN parse_typ in
    expect st RPAREN;
    let ret =
      if is_type_start (cur_token st) then Some (parse_typ st) else None
    in
    mkt lo st (FuncPtr (abi, params, ret))
  with ParserError d ->
    report st d;
    error_typ (recovery_span st d)

(* The "C" of extern "C" func exit(code: i32) never *)
and parse_abi st =
  match cur_token st with
  | STRING name ->
      let span = cur_span st in
      advance st;
      NamedAbi (spanned name span)
  | _ ->
      Diagnostic.error (cur_span st) "expected ABI name"
      |> found st |> report st;
      if not (at st EOF || starts_item st) then advance st;
      AbiError

(* pub *)
and parse_modifiers st =
  let[@tail_mod_cons] rec go () =
    match cur_token st with
    | PUBLIC ->
        advance st;
        Pub :: go ()
    | _ -> []
  in
  go ()

(* pub, extern "C", pub extern "C" *)
and parse_decl_modifiers st =
  let mods = parse_modifiers st in
  if at st EXTERN then begin
    advance st;
    let abi = parse_abi st in
    (mods @ parse_modifiers st, abi)
  end
  else (mods, NoAbi)

(* x: i32; y: i32 *)
and parse_fields st =
  let fields = ref [] in
  skip_semi st;
  while
    not (at st RBRACE || at st EOF || starts_item st || is_member_stop st)
  do
    try
      let name = expect_decl_name st in
      let typ = typ_after st COLON in
      fields := { field_name = name; field_typ = typ } :: !fields;
      expect_decl_sep st ~what:"field"
    with ParserError d ->
      report st d;
      let span = recovery_span st d in
      fields :=
        { field_name = missing_ident span; field_typ = error_typ span }
        :: !fields;
      while not (at st EOF || at st RBRACE || is_semi (cur_token st)) do
        advance st
      done;
      skip_semi st
  done;
  List.rev !fields

(* { x: i32; y: i32 } *)
and parse_struct_body st =
  if not (at st LBRACE) then begin
    Diagnostic.error (cur_span st) "expected `{`" |> found st |> report st;
    None
  end
  else begin
    advance st;
    let fields = parse_fields st in
    if at st RBRACE then begin
      advance st;
      Some fields
    end
    else begin
      Diagnostic.error (cur_span st) "expected `}`" |> found st |> report st;
      None
    end
  end

(* struct point { x: i32; y: i32 } *)
and parse_struct_def st mods =
  let lo = cur_pos st in
  expect st STRUCT;
  let name = expect_decl_name st in
  let fields = parse_struct_body st in
  {
    struct_name = name;
    fields;
    struct_modifiers = mods;
    struct_span = span_from lo st;
  }

(* Red; Green; Blue *)
and parse_variants st =
  let variants = ref [] in
  skip_semi st;
  while
    not (at st RBRACE || at st EOF || starts_item st || is_member_stop st)
  do
    try
      variants := expect_decl_name st :: !variants;
      expect_decl_sep st ~what:"variant"
    with ParserError d ->
      report st d;
      variants := missing_ident (recovery_span st d) :: !variants;
      while not (at st EOF || at st RBRACE || is_semi (cur_token st)) do
        advance st
      done;
      skip_semi st
  done;
  List.rev !variants

(* { Red; Green; Blue } *)
and parse_enum_body st =
  if not (at st LBRACE) then begin
    Diagnostic.error (cur_span st) "expected `{`" |> found st |> report st;
    None
  end
  else begin
    advance st;
    let variants = parse_variants st in
    if at st RBRACE then begin
      advance st;
      Some variants
    end
    else begin
      Diagnostic.error (cur_span st) "expected `}`" |> found st |> report st;
      None
    end
  end

(* enum Color { Red; Green; Blue } *)
and parse_enum_def st mods =
  let lo = cur_pos st in
  expect st ENUM;
  let name = expect_decl_name st in
  let variants = parse_enum_body st in
  {
    enum_name = name;
    variants;
    enum_modifiers = mods;
    enum_span = span_from lo st;
  }

(* type binop = (i32, i32) i32 *)
and parse_alias_def st mods =
  let lo = cur_pos st in
  expect st TYPE;
  let name = expect_decl_name st in
  let line = cur_line st in
  let typ =
    match cur_token st with
    | ASSIGN -> (
        try typ_after st ASSIGN
        with ParserError d ->
          report st d;
          skip_line st line [];
          error_typ (recovery_span st d))
    | _ ->
        let span = cur_span st in
        Diagnostic.error span "expected `=`" |> found st |> report st;
        skip_line st line [];
        error_typ span
  in
  {
    alias_name = name;
    alias_typ = typ;
    alias_modifiers = mods;
    alias_span = span_from lo st;
  }

(* x: i32 *)
and parse_param st =
  let lo = cur_pos st in
  let name = expect_decl_name st in
  let typ = typ_after st COLON in
  { param_name = name; param_typ = typ; param_span = span_from lo st }

(* (a: i32, b: i32), (fmt: cstr, ...) *)
and parse_params st =
  expect st LPAREN;
  let rec go params =
    match cur_token st with
    | RPAREN -> (List.rev params, None)
    | ELLIPSIS ->
        let span = cur_span st in
        advance st;
        if not (at st RPAREN) then begin
          Diagnostic.error (cur_span st) "`...` must be the last parameter"
          |> report st;
          skip_to_close st
        end;
        (List.rev params, Some span)
    | _ -> (
        let param =
          try parse_param st
          with ParserError d ->
            report st d;
            let span = recovery_span st d in
            while
              not (at st EOF || at st RPAREN || at st LBRACE || at st COMMA)
            do
              advance st
            done;
            {
              param_name = missing_ident span;
              param_typ = error_typ span;
              param_span = span;
            }
        in
        let params = param :: params in
        match cur_token st with
        | COMMA ->
            advance st;
            go params
        | RPAREN -> (List.rev params, None)
        | tok ->
            Diagnostic.error (cur_span st) "expected parameter separator"
            |> Diagnostic.help "separate parameters with `,`"
            |> report st;
            if is_semi tok || tok == ELLIPSIS || starts_member st then begin
              skip_semi st;
              go params
            end
            else begin
              skip_to_close st;
              (List.rev params, None)
            end)
  in
  let params, variadic = go [] in
  if at st RPAREN then advance st
  else Diagnostic.error (cur_span st) "expected `)`" |> found st |> report st;
  (params, variadic)

(* i32 *)
and parse_ret_type st =
  match cur_token st with
  | LBRACE | AUTOSEMI | SEMI | EOF | ASSIGN -> None
  | FUNC when is_member_start (peek_token st) -> None
  | tok when not (is_type_start tok) -> None
  | _ -> Some (parse_typ st)

(* The add of func add(a: i32) i32 *)
and parse_func_name st =
  try expect_decl_name st
  with ParserError d ->
    report st d;
    if is_member_start (peek_token st) then begin
      advance st;
      expect_decl_name st
    end
    else begin
      skip_line st (cur_line st) [ LPAREN; LBRACKET; LBRACE; RPAREN; RBRACKET ];
      missing_ident (recovery_span st d)
    end

(* { return 1 } *)
and parse_func_body st =
  if at st LBRACE then (parse_block st).value
  else begin
    let d = Diagnostic.error (cur_span st) "expected `{`" |> found st in
    [ Expr (recover_expr st d) ]
  end

(* func add(a: i32, b: i32) i32 { ... }, extern "C" func puts(s: cstr) i32 *)
and parse_func_def st mods abi =
  let lo = cur_pos st in
  expect st FUNC;
  let name = parse_func_name st in
  let params, variadic = parse_params st in
  let ret = parse_ret_type st in
  let imported = abi <> NoAbi && not (at st LBRACE) in
  let body = if imported then [] else parse_func_body st in
  if not imported then
    begin match variadic with
    | Some span ->
        Diagnostic.error span "a function with a body cannot be variadic"
        |> Diagnostic.help "`...` only works on a declaration with no body"
        |> report st
    | None -> ()
    end;
  ( {
      func_name = name;
      params;
      ret;
      body;
      func_modifiers = mods;
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

(* point { x: 1 }, geometry.point { x: 1 } *)
and parse_struct_lit st lo path name =
  expect st LBRACE;
  let fields = parse_struct_lit_fields st in
  expect st RBRACE;
  mk lo st (StructLit (path, name, fields))

(* -x *)
and parse_prefix st context =
  let lo = cur_pos st in
  let unary desc =
    let span = cur_span st in
    advance st;
    require_expr_start st span;
    let operand = parse_prefix st context in
    if is_error_expr operand then mk lo st ErrorExpr
    else mk lo st (UnOp (desc, operand))
  in
  match cur_token st with
  | BANG -> unary Not
  | PLUS -> unary Pos
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

(* point, geometry.point, point { x: 1 } *)
and parse_path st context lhs head =
  let lo = Span.lo lhs.span in
  let modules, name, plain =
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
      let path =
        { owner = Nonempty.make (spanned head lhs.span) owners; member }
      in
      let plain = path_expr path in
      (path_names path, spanned member.value plain.span, plain)
    end
    else ([], spanned head lhs.span, lhs)
  in
  if not (at st LBRACE) then plain
  else
    match context with
    | NormalExpression when opens_struct_lit st ->
        parse_struct_lit st lo modules name
    | HeaderExpression when opens_struct_field st ->
        Diagnostic.error (cur_span st) "a struct literal can't go in a header"
        |> Diagnostic.label "this `{` starts the body"
        |> Diagnostic.help "wrap the literal in parentheses"
        |> report st;
        parse_struct_lit st lo modules name
    | NormalExpression | HeaderExpression -> plain

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

(* (), (x), (i32)(x) *)
and parse_paren st =
  let lo = cur_pos st in
  expect st LPAREN;
  if at st RPAREN then begin
    advance st;
    mk lo st Unit
  end
  else if opens_type st then begin
    let t = parse_typ st in
    expect st RPAREN;
    expect st LPAREN;
    let args = parse_comma_list st RPAREN in
    expect st RPAREN;
    match args with
    | [ arg ] -> mk lo st (Cast (t, arg))
    | _ ->
        let d =
          Diagnostic.error (span_from lo st) "wrong number of arguments"
          |> Diagnostic.label "expected 1 argument, found %d" (List.length args)
        in
        recover_expr st d
  end
  else
    let e = parse_expr st in
    if at st RPAREN || not (is_error_expr e) then expect st RPAREN;
    e

(* 1, x, "str", foo(a, b) *)
and parse_primary st context =
  let lo = cur_pos st in
  match cur_token st with
  | ERROR _ ->
      advance st;
      mk lo st ErrorExpr
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
  | IDENT _ when peek_token st == COLON -> parse_labeled_loop st
  | IDENT name ->
      let name = Interner.intern name in
      let nspan = cur_span st in
      advance st;
      { desc = Ident name; span = nspan }
  | STRING s ->
      advance st;
      mk lo st (String s)
  | UNDEFINED ->
      advance st;
      mk lo st Undefined
  | IF -> parse_if st
  | MATCH -> parse_match st context
  | LBRACE when context = HeaderExpression ->
      Diagnostic.error (cur_span st) "expected expression" |> fail
  | LBRACE ->
      let body = (parse_block st).value in
      mk lo st (Block body)
  | LOOP -> parse_loop st
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
and parse_header_expr st =
  match parse_expr ~context:HeaderExpression st with
  | e -> e
  | exception ParserError d when at st LBRACE -> recover_expr st d

(* a, b = b, a *)
and parse_pair_assign st lo t1 =
  expect st COMMA;
  let t2 = parse_expr ~min_prec:2 st in
  if at st COMMA then
    Diagnostic.error (cur_span st)
      "pair assignment requires exactly two targets"
    |> fail;
  expect st ASSIGN;
  let v1 = parse_expr st in
  expect st COMMA;
  let v2 = parse_expr st in
  if at st COMMA then
    Diagnostic.error (cur_span st) "pair assignment requires exactly two values"
    |> fail;
  mk lo st (PairAssign (t1, t2, v1, v2))

(* { return a + b }  *)
and parse_block st =
  let lo = cur_pos st in
  expect st LBRACE;
  let body = parse_stmts st in
  expect st RBRACE;
  spanned body (span_from lo st)

(* stmt; stmt *)
and parse_stmts st =
  let[@tail_mod_cons] rec go follows_auto_semi =
    if at st EOF || at st RBRACE then []
    else
      let line = cur_line st in
      let start_token = cur_token st in
      let start_span = cur_span st in
      match parse_stmt st with
      | s ->
          if follows_auto_semi then
            begin match s with
            | Expr e
              when is_ambiguous_continuation start_token
                   && not (is_dereference_assignment start_token e) ->
                Diagnostic.error start_span
                  "operator starts a new statement after a newline"
                |> Diagnostic.help "move the operator to the previous line"
                |> report st
            | Expr _ -> ()
            | Decl _ -> ()
            end;

          let after_auto_semi = at st AUTOSEMI in
          if is_semi (cur_token st) then skip_semi st
          else if
            not
              (at st RBRACE || at st EOF
              || is_next_line_start st line is_stmt_start)
          then begin
            Diagnostic.error (cur_span st) "expected `;`"
            |> found st |> report st;
            recover_statement st line;
            skip_semi st
          end;
          s :: go after_auto_semi
      | exception ParserError d ->
          let error =
            if at st EOF then { desc = ErrorExpr; span = recovery_span st d }
            else recover_expr st d
          in
          recover_statement st line;
          skip_semi st;
          Expr error :: go false
  in
  skip_semi st;
  go false

(* var n = 1, if c { }, return x *)
and parse_stmt ?(context = BlockStatement) st =
  let lo = cur_pos st in
  match cur_token st with
  | IF -> Expr (parse_if st)
  | MATCH -> Expr (parse_match st NormalExpression)
  | WHILE -> Expr (parse_while st)
  | FOR -> Expr (parse_for st)
  | LOOP -> Expr (parse_loop st)
  | IDENT _ when peek_token st == COLON -> Expr (parse_labeled_loop st)
  | LBRACE ->
      let body = (parse_block st).value in
      Expr (mk lo st (Block body))
  | CONST | VAR ->
      let kind, name, ann, e = parse_binding st LocalBinding in
      Expr (mk lo st (Binding (kind, name, ann, e)))
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
  | FUNC when peek_token st == LPAREN -> expression_statement st context lo
  | PUBLIC | FUNC | STRUCT | TYPE | ENUM | EXTERN -> parse_local_decl st
  | _ -> expression_statement st context lo

(* pub type small = i32 inside a body *)
and parse_local_decl st =
  let modifiers = parse_modifiers st in
  let decl =
    match cur_token st with
    | STRUCT -> LocalStruct (parse_struct_def st modifiers)
    | TYPE -> LocalTypeAlias (parse_alias_def st modifiers)
    | FUNC -> LocalFunc (fst (parse_func_def st modifiers NoAbi))
    | ENUM -> LocalEnum (parse_enum_def st modifiers)
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

(* match c { Color.Red => 0; _ => 1 } *)
and parse_match st context =
  let lo = cur_pos st in
  expect st MATCH;
  let scrutinee = parse_header_expr st in
  (* The outer header needs this brace *)
  if context = HeaderExpression && is_error_expr scrutinee then scrutinee
  else parse_arms st lo scrutinee

(* { Color.Red => 0; _ => 1 } *)
and parse_arms st lo scrutinee =
  expect st LBRACE;
  let arms = ref [] in
  while has_arm st do
    try
      arms := parse_arm st :: !arms;
      expect_decl_sep st ~what:"arm"
    with ParserError d ->
      report st d;
      while not (at st RBRACE || at st EOF || is_semi (cur_token st)) do
        advance st
      done;
      skip_semi st
  done;
  expect st RBRACE;
  mk lo st (Match (scrutinee, List.rev !arms))

(* Color.Red => 0 *)
and parse_arm st =
  let lo = cur_pos st in
  let pat = parse_pattern st in
  let arrow = cur_span st in
  let line = cur_line st in
  expect st FATARROW;
  if cur_line st > line then
    Diagnostic.error arrow "expected expression" |> fail;
  let body_lo = cur_pos st in
  let body =
    spanned [ parse_stmt ~context:MatchArmBody st ] (span_from body_lo st)
  in
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

(* while i < len { } *)
and parse_while ?label st =
  let lo = loop_lo st label in
  expect st WHILE;
  let cond = parse_header_expr st in
  let body = (parse_block st).value in
  mk lo st (While (label, cond, body))

(* break :outer *)
and parse_loop_target st =
  if at st COLON then begin
    advance st;
    Some (expect_ident_span st)
  end
  else None

(* for i in 0..len { } *)
and parse_for ?label st =
  let lo = loop_lo st label in
  expect st FOR;
  let name = expect_ident_span st in
  expect st IN;
  let iter = parse_header_expr st in
  let body = (parse_block st).value in
  mk lo st (For (label, name, iter, body))

(* loop { } *)
and parse_loop ?label st =
  let lo = loop_lo st label in
  expect st LOOP;
  let body = (parse_block st).value in
  mk lo st (Loop (label, body))

(* outer: for row in grid { } *)
and parse_labeled_loop st =
  let label = expect_ident_span st in
  expect st COLON;
  match cur_token st with
  | WHILE -> parse_while ~label st
  | FOR -> parse_for ~label st
  | LOOP -> parse_loop ~label st
  | _ ->
      Diagnostic.error (cur_span st) "expected a loop after a label"
      |> found st |> fail

(* const PAGE_SIZE: i32 = 4096, var n: i32 = 0, var flag: bool *)
let parse_global st mods =
  let lo = cur_pos st in
  let kind, name, typ, init = parse_binding st GlobalBinding in
  let name = ident name.value name.span in
  Global { name; typ; init; kind; modifiers = mods; span = span_from lo st }

(* pub func f() i32 { } *)
let parse_decl st =
  let mods, abi = parse_decl_modifiers st in
  begin match cur_token st with
  | STRUCT | CONST | VAR | TYPE | ENUM -> abi_wants_func st abi
  | _ -> ()
  end;
  match cur_token st with
  | FUNC ->
      let fd, imported = parse_func_def st mods abi in
      if imported then Extern fd else Func fd
  | STRUCT -> Struct (parse_struct_def st mods)
  | CONST | VAR -> parse_global st mods
  | TYPE -> TypeAlias (parse_alias_def st mods)
  | ENUM -> Enum (parse_enum_def st mods)
  | RPAREN | RBRACKET | RBRACE ->
      Diagnostic.error (cur_span st) "unexpected closing delimiter" |> fail
  | _ ->
      Diagnostic.error (cur_span st) "expected declaration" |> found st |> fail

(* import math.vector *)
let parse_import st =
  let lo = cur_pos st in
  expect st IMPORT;
  let[@tail_mod_cons] rec rest () =
    if at st DOT then begin
      advance st;
      let name = expect_ident st in
      name :: rest ()
    end
    else []
  in
  let first = expect_ident st in
  let path = first :: rest () in
  { path; span = span_from lo st }

(* module math *)
let parse_module_header st =
  let lo = cur_pos st in
  expect st MODULE;
  let name = expect_ident st in
  { name; span = span_from lo st }

(* module m, import a.b, func f() { } *)
let parse_item st ~first =
  match cur_token st with
  | MODULE when first -> HeaderItem (parse_module_header st)
  | MODULE ->
      Diagnostic.error (cur_span st) "`module` must be the first item" |> fail
  | IMPORT -> ImportItem (parse_import st)
  | _ -> DeclItem (parse_decl st)

(* module m; import a.b; func f() { } *)
let parse_module st =
  let rec go ~first header imports decls =
    if at st EOF then
      { header; imports = List.rev imports; decls = List.rev decls }
    else
      let line = cur_line st in
      match parse_item st ~first with
      | item -> (
          if is_semi (cur_token st) then skip_semi st
          else if not (at st EOF || is_next_line_start st line is_item_start)
          then begin
            let d = Diagnostic.error (cur_span st) "expected `;`" |> found st in
            report st d;
            recover_declaration st d
          end;
          match item with
          | HeaderItem h -> go ~first:false (Some h) imports decls
          | ImportItem i -> go ~first:false header (i :: imports) decls
          | DeclItem decl -> go ~first:false header imports (decl :: decls))
      | exception ParserError d ->
          report st d;
          recover_declaration st d;
          go ~first:false header imports decls
  in
  skip_semi st;
  go ~first:true None [] []

let read_token diags lex lexbuf () =
  match lex lexbuf with
  | ERROR msg, span, line ->
      Diagnostic.emit diags (Diagnostic.error span "%s" msg);
      { token = ERROR msg; span; line }
  | token, span, line -> { token; span; line }

let parse ~diags (lex : Lexing.lexbuf -> Tokens.token * Ast.span * int) lexbuf =
  let read = read_token diags lex lexbuf in
  let current = read () in
  let st =
    { read; current; ahead = []; prev_end = Span.hi Span.dummy; diags }
  in
  parse_module st
