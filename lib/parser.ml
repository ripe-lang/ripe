(* SPDX-License-Identifier: Apache-2.0 *)

open Tokens
open Ast

exception ParseError of Diagnostic.t

type token_info = { token : token; span : span; line : int }

type state = {
  mutable tok : token;
  mutable tok_span : span;
  mutable tok_line : int;
  mutable tok_depth : int;
  mutable open_delims : (token * token * span) list;
  read : unit -> token_info;
  diags : Diagnostic.sink;
  mutable prev_end : int;
  mutable ahead : token_info list;
}

type chain = Comparison | Range
type assoc = Left | Right
type infix = { prec : int; assoc : assoc; chain : chain option }
type field_form = NamedField | PositionalField

(* Span of the current lookahead token so the caret lands under it *)
let cur_span st = st.tok_span

let rec peek_nth st n =
  match List.nth_opt st.ahead n with
  | Some info -> info
  | None ->
      st.ahead <- st.ahead @ [ st.read () ];
      peek_nth st n

let peek st = peek_nth st 0

let closer_for = function
  | LPAREN -> Some RPAREN
  | LBRACKET -> Some RBRACKET
  | LBRACE -> Some RBRACE
  | _ -> None

let is_closer = function RPAREN | RBRACKET | RBRACE -> true | _ -> false
let is_delim tok = is_closer tok || closer_for tok <> None

(* The openers a crossed closer reaches over were never going to close *)
let rec past_opener want = function
  | [] -> None
  | (w, _, _) :: rest -> if w = want then Some rest else past_opener want rest

(* An opener counts from the outside and its closer from the inside *)
let enter_delim st token span =
  match closer_for token with
  | Some want ->
      let outer = List.length st.open_delims in
      st.open_delims <- (want, token, span) :: st.open_delims;
      outer
  | None when is_closer token -> (
      match past_opener token st.open_delims with
      | Some rest ->
          st.open_delims <- rest;
          List.length rest + 1
      | None -> List.length st.open_delims)
  | None -> List.length st.open_delims

let drop_opener st =
  match st.open_delims with _ :: rest -> st.open_delims <- rest | [] -> ()

let unclosed st closer =
  List.find_opt (fun (want, _, _) -> want = closer) st.open_delims

let advance st =
  st.prev_end <- Span.hi st.tok_span;
  let info =
    match st.ahead with
    | info :: rest ->
        st.ahead <- rest;
        info
    | [] -> st.read ()
  in
  st.tok <- info.token;
  st.tok_span <- info.span;
  st.tok_line <- info.line;
  st.tok_depth <- enter_delim st info.token info.span

let at st t = st.tok = t
let at_one_of stops st = List.mem st.tok stops
let no_stop _ = false

(* Start of the current lookahead token *)
let cur_pos st = Span.lo st.tok_span

let loop_lo st label =
  match label with
  | Some (l : Ast.loop_label) -> Span.lo l.span
  | None -> cur_pos st

let fail st headline =
  raise (ParseError (Diagnostic.error headline |> Diagnostic.at (cur_span st)))

let found_error st headline =
  Diagnostic.with_found (cur_span st) headline (show_found_token st.tok)

let fail_found st headline = raise (ParseError (found_error st headline))

let is_expr_start = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | TRUE | FALSE | NULL | SIZEOF | BITCAST | LPAREN | LBRACKET
  | UNDEFINED | IF | LBRACE | LOOP | MATCH | ERROR _ ->
      true
  | _ -> false

let require_expr_start st span =
  if not (is_expr_start st.tok) then
    raise (ParseError (Diagnostic.expected_expression span))

let is_type_start = function
  | IDENT _ | STAR | LBRACKET | FUNC | EXTERN | LPAREN | ERROR _ -> true
  | _ -> false

let is_semi = function AUTOSEMI | SEMI -> true | _ -> false

let skip_semi st =
  while is_semi st.tok do
    advance st
  done

let is_ambiguous_continuation = function
  | PLUS | MINUS | STAR | AMP -> true
  | _ -> false

let is_dereference_assignment token e =
  match e.desc with Assign _ -> token = STAR | _ -> false

let diagnose_dropped_continuation st token span expr =
  if
    is_ambiguous_continuation token
    && not (is_dereference_assignment token expr)
  then
    Diagnostic.emit st.diags
      (Diagnostic.error "operator starts a new statement after a newline"
      |> Diagnostic.at span
      |> Diagnostic.help "move the operator to the previous line")

let is_stmt_start = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | CONST | VAR | RETURN | IF | WHILE | FOR | BREAK | CONTINUE
  | TRUE | FALSE | NULL | SIZEOF | BITCAST | LPAREN | LBRACE | LBRACKET
  | UNDEFINED | LOOP | MATCH | ERROR _ ->
      true
  | _ -> false

let is_item_start = function
  | FUNC | EXTERN | STRUCT | PUBLIC | TYPE | IMPORT | CONST | VAR | ENUM -> true
  | _ -> false

let is_stmt_keyword = function
  | CONST | VAR | RETURN | IF | WHILE | FOR | BREAK | CONTINUE | LOOP | MATCH ->
      true
  | _ -> false

let is_member_start = function IDENT _ -> true | _ -> false
let is_params_end = function LBRACE | EOF -> true | _ -> false

(* A name with a `:` behind it opens a member so no type can reach across it *)
let starts_member st =
  match st.tok with IDENT _ -> (peek st).token = COLON | _ -> false

(* Junk still belongs to a literal so only a real statement makes it a block *)
let opens_struct_lit st =
  let tok = (peek st).token in
  not (is_stmt_start tok && not (is_expr_start tok))

let is_param_start st =
  match st.tok with
  | ELLIPSIS -> true
  | IDENT _ -> (peek st).token = COLON
  | tok -> is_semi tok

let is_next_line_start st line is_start = st.tok_line > line && is_start st.tok
let has_abi st = match (peek st).token with STRING _ -> true | _ -> false

(* An `extern` opens a declaration unless another one or the line end follows *)
let opens_extern st =
  match (peek st).token with
  | STRING _ | FUNC -> true
  | _ when (peek_nth st 1).token = FUNC -> true
  | tok -> not (is_semi tok || is_closer tok || tok = EOF || is_item_start tok)

let opens_decl tok name after =
  match (tok, name, after) with
  | FUNC, IDENT _, LPAREN
  | (STRUCT | ENUM), IDENT _, LBRACE
  | TYPE, IDENT _, ASSIGN
  | (CONST | VAR), IDENT _, (COLON | ASSIGN)
  | IMPORT, IDENT _, _
  | EXTERN, (STRING _ | FUNC), _
  | PUBLIC, _, _ ->
      true
  | _ -> false

(* A broken declaration is still one so only its name is asked for here *)
let names_decl tok name =
  match (tok, name) with
  | (FUNC | STRUCT | ENUM | TYPE | CONST | VAR | IMPORT), IDENT _
  | EXTERN, (STRING _ | FUNC)
  | PUBLIC, _ ->
      true
  | _ -> false

(* A keyword with nothing usable behind it opens a type or nothing at all *)
let starts_item st =
  match st.tok with
  | EXTERN -> opens_extern st
  | tok -> names_decl tok (peek st).token

let resumes_item st = starts_item st && not (is_stmt_start st.tok)

(* A whole declaration behind a keyword means that keyword was a slip *)
let is_stray_keyword st =
  is_item_start st.tok && st.tok <> PUBLIC
  (* An `extern func` is only missing its ABI so it keeps both words *)
  && (not (st.tok = EXTERN && (peek st).token = FUNC))
  && opens_decl (peek st).token (peek_nth st 1).token (peek_nth st 2).token

(* The enclosing block needs its own closer back *)
let rec sync_to_item st depth =
  match st.tok with
  | EOF -> ()
  | RBRACE when depth > 0 && st.tok_depth <= depth -> ()
  | _ when resumes_item st -> ()
  | _ ->
      advance st;
      sync_to_item st depth

let rec sync_to_stmt st depth line after_semi =
  match st.tok with
  | EOF -> ()
  | RBRACE when st.tok_depth = depth -> ()
  | _
    when st.tok_depth = depth
         && (is_stmt_start st.tok || resumes_item st)
         && (after_semi || st.tok_line > line) ->
      ()
  | (AUTOSEMI | SEMI) when st.tok_depth = depth ->
      advance st;
      sync_to_stmt st depth line true
  | _ ->
      advance st;
      sync_to_stmt st depth line after_semi

let expect_ident st =
  match st.tok with
  | IDENT s ->
      advance st;
      Interner.intern s
  | _ -> fail_found st "expected identifier"

let expect_ident_span st =
  let span = st.tok_span in
  let name = expect_ident st in
  (name, span)

let expect_decl_name st =
  let span = st.tok_span in
  Ast.ident (expect_ident st) span

let expect_binding_name st =
  match st.tok with
  | UNDERSCORE ->
      let span = st.tok_span in
      advance st;
      (Interner.intern "_", span)
  | _ -> expect_ident_span st

let to_match (_, opener, span) d =
  Diagnostic.secondary span
    (Printf.sprintf "to match this `%s`" (show_token opener))
    d

let expected_error st t =
  let headline = Printf.sprintf "expected `%s`" (show_token t) in
  let d =
    Diagnostic.with_found (cur_span st) headline (show_found_token st.tok)
  in
  (* Nothing closes at eof so every delimiter still open is one of the news *)
  if st.tok = EOF then List.fold_left (fun d e -> to_match e d) d st.open_delims
  else match unclosed st t with Some e -> to_match e d | None -> d

let expect st t =
  if st.tok = t then advance st else raise (ParseError (expected_error st t))

(* A closer that belongs to somebody else ends the list where it stands *)
let ends_list st stop = st.tok = stop || is_closer st.tok

(* item (, item)* with an optional trailing comma before stop *)
let comma_sep st stop parse_one =
  let items = ref [] in
  if not (ends_list st stop) then begin
    items := [ parse_one () ];
    while st.tok = COMMA do
      advance st;
      if not (ends_list st stop) then items := parse_one () :: !items
    done;
    if is_semi st.tok then fail st "missing `,` before newline"
  end;
  List.rev !items

let make_span _st lo hi = Span.make lo hi
let mk lo st desc = { desc; span = make_span st lo st.prev_end }
let mkt lo st tdesc = { tdesc; tspan = make_span st lo st.prev_end }

let recovery_span st d =
  Option.value (Diagnostic.primary d) ~default:(cur_span st)

(* The blamed token is junk unless it owns itself like a closer does *)
let sync_past st depth d =
  (match Diagnostic.primary d with
  | Some span when cur_pos st = Span.lo span && not (is_closer st.tok) ->
      advance st
  | _ -> ());
  sync_to_item st depth

let emit_parse_error st d =
  let duplicate =
    match st.tok with
    | ERROR _ -> Diagnostic.primary d = Some (cur_span st)
    | _ -> false
  in
  if not duplicate then Diagnostic.emit st.diags d

let drop_stray_keyword st =
  let headline =
    if st.tok = EXTERN then "expected ABI name" else "expected identifier"
  in
  advance st;
  emit_parse_error st (found_error st headline)

let error_expr st d = { desc = ErrorExpr; span = recovery_span st d }
let is_error_expr e = match e.desc with ErrorExpr -> true | _ -> false
let error_typ st d = { tdesc = ErrorType; tspan = recovery_span st d }

(* A nameless parameter is where a whole run of them got dropped *)
let has_hole params =
  List.exists (fun (p : param) -> Option.is_none p.param_name.value) params

(* The junk standing in for a name is eaten so it can't start a declaration *)
let recover_decl_name st opener =
  let owns_itself st = st.tok = opener || st.tok = EOF || is_delim st.tok in
  try expect_decl_name st
  with ParseError d ->
    emit_parse_error st d;
    let span = recovery_span st d in
    if (not (owns_itself st)) && cur_pos st = Span.lo span then advance st;
    let line = st.tok_line in
    while
      (not (owns_itself st))
      && st.tok_line = line
      && (not (starts_item st))
      && not (is_stmt_keyword st.tok)
    do
      advance st
    done;
    Ast.missing_ident span

let missing_field span =
  {
    field_name = Ast.missing_ident span;
    field_typ = { tdesc = ErrorType; tspan = span };
  }

let rec sync_to_depth_token st depth line stop =
  if
    st.tok = EOF
    || (st.tok_depth = depth && (stop st || is_semi st.tok || st.tok = RBRACE))
    || (depth = 0 && starts_item st && st.tok_line > line)
    || st.tok_depth = depth && st.tok_line > line
       && if depth = 0 then is_item_start st.tok else is_stmt_start st.tok
  then ()
  else (
    advance st;
    sync_to_depth_token st depth line stop)

let recover st ~depth ~stop ~parse ~fallback =
  let line = st.tok_line in
  try parse ()
  with ParseError d ->
    emit_parse_error st d;
    sync_to_depth_token st depth line stop;
    fallback st d

(* { item; item } *)
let parse_braced st parse =
  let depth = st.tok_depth in
  (* Nothing opened so what stands here is junk unless it is a stray closer *)
  match expect st LBRACE with
  | exception ParseError d ->
      emit_parse_error st d;
      sync_past st depth d;
      None
  | () -> (
      try
        let items = parse st in
        expect st RBRACE;
        Some items
      with ParseError d ->
        emit_parse_error st d;
        sync_to_item st depth;
        None)

let is_stray_closer st = is_closer st.tok && unclosed st st.tok = None
let is_member_stop st = is_stray_closer st || is_stmt_keyword st.tok

let decl_sep_error st what =
  Diagnostic.error ("expected " ^ what ^ " separator")
  |> Diagnostic.at (cur_span st)
  |> Diagnostic.help ("separate " ^ what ^ "s with a newline or `;`")

let param_sep_error st =
  Diagnostic.error "expected parameter separator"
  |> Diagnostic.at (cur_span st)
  |> Diagnostic.help "separate parameters with `,`"

(* Junk that can't start the next item is eaten so it isn't reported twice *)
let expect_decl_sep st ~what =
  match st.tok with
  | AUTOSEMI | SEMI -> skip_semi st
  | RBRACE | EOF -> ()
  | _ when is_member_stop st -> ()
  | tok ->
      emit_parse_error st (decl_sep_error st what);
      if not (is_member_start tok) then advance st

(* An unclosed body stops at a statement or a declaration that starts a line *)
let ends_member_list st prev_line =
  is_member_stop st || (resumes_item st && st.tok_line > prev_line)

(* item; item *)
let parse_decl_list st ~what ~missing parse_one =
  let depth = st.tok_depth in
  let items = ref [] in
  let prev_line = ref st.tok_line in
  while
    st.tok <> RBRACE && st.tok <> EOF && not (ends_member_list st !prev_line)
  do
    let line = st.tok_line in
    let stuck = cur_pos st in
    prev_line := line;
    (try
       items := parse_one depth :: !items;
       expect_decl_sep st ~what
     with ParseError d ->
       emit_parse_error st d;
       sync_to_depth_token st depth line starts_member;
       items := missing (recovery_span st d) :: !items;
       skip_semi st);
    if cur_pos st = stuck then advance st
  done;
  List.rev !items

let expect_literal_field_sep st =
  match st.tok with
  | COMMA -> advance st
  | AUTOSEMI | SEMI -> fail st "missing `,` before newline"
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
  | _ -> failwith "not a binary operator"

let assign_of = function
  | ASSIGN -> Some None
  | PLUS_ASSIGN -> Some (Some Add)
  | MINUS_ASSIGN -> Some (Some Sub)
  | STAR_ASSIGN -> Some (Some Mul)
  | SLASH_ASSIGN -> Some (Some Div)
  | PERCENT_ASSIGN -> Some (Some Mod)
  | AMP_ASSIGN -> Some (Some BitAnd)
  | PIPE_ASSIGN -> Some (Some BitOr)
  | CARET_ASSIGN -> Some (Some BitXor)
  | LSHIFT_ASSIGN -> Some (Some Lshift)
  | RSHIFT_ASSIGN -> Some (Some Rshift)
  | _ -> None

(* i32, *i32, func (i32, i32) i32 *)
let rec parse_typ st =
  let lo = cur_pos st in
  match st.tok with
  | ERROR _ ->
      advance st;
      mkt lo st ErrorType
  | EXTERN when not (has_abi st) -> fail_found st "expected type"
  | EXTERN ->
      advance st;
      parse_func_ptr st lo (parse_abi st)
  | STAR ->
      advance st;
      mkt lo st (Pointer (parse_typ st))
  | IDENT _ when starts_member st -> fail_found st "expected type"
  | LBRACE when is_type_start (peek st).token ->
      emit_parse_error st (found_error st "expected type");
      drop_opener st;
      advance st;
      parse_typ st
  | (STRUCT | ENUM | TYPE | CONST | VAR | PUBLIC | IMPORT)
    when is_type_start (peek st).token && (peek st).line = st.tok_line ->
      emit_parse_error st (found_error st "expected type");
      advance st;
      parse_typ st
  | IDENT name ->
      advance st;
      let path = ref [ Interner.intern name ] in
      while st.tok = DOT do
        advance st;
        path := expect_ident st :: !path
      done;
      let modules, base =
        match !path with
        | base :: rest -> (List.rev rest, base)
        | [] -> ([], Interner.intern "")
      in
      mkt lo st (Named (modules, base))
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
  | FUNC -> parse_func_ptr st lo Ast.NoAbi
  | LPAREN ->
      advance st;
      if at st RPAREN then begin
        advance st;
        mkt lo st UnitType
      end
      else
        let inner = parse_typ st in
        expect st RPAREN;
        { inner with tspan = Span.make lo st.prev_end }
  | _ -> fail_found st "expected type"

(* I have to wrap it in a fun so it doesn't run before the catch is ready *)
and recover_typ st depth stop =
  recover st ~depth ~stop ~parse:(fun () -> parse_typ st) ~fallback:error_typ

(* A newline or the next member's name ends it so the caret stays on the `:` *)
and typ_after st tok =
  let span = cur_span st in
  let line = st.tok_line in
  expect st tok;
  if st.tok_line > line || starts_member st then
    raise (ParseError (Diagnostic.expected_type span));
  parse_typ st

and recover_typ_after st depth stop tok =
  recover st ~depth ~stop
    ~parse:(fun () -> typ_after st tok)
    ~fallback:error_typ

(* Nothing after the `=` puts the caret on it, not on the next line *)
and recover_init st depth =
  let span = cur_span st in
  let line = st.tok_line in
  recover st ~depth ~stop:no_stop
    ~parse:(fun () ->
      expect st ASSIGN;
      if st.tok_line > line then require_expr_start st span;
      parse_expr st 1)
    ~fallback:error_expr

and optional_annotation st depth =
  if at st COLON then
    Some (recover_typ_after st depth (at_one_of [ ASSIGN ]) COLON)
  else None

(* func (i32, i32) i32, extern "C" func (i32) i32 *)
and parse_func_ptr st lo abi =
  expect st FUNC;
  expect st LPAREN;
  let params = comma_sep st RPAREN (fun () -> parse_typ st) in
  expect st RPAREN;
  let ret = if is_type_start st.tok then Some (parse_typ st) else None in
  mkt lo st (FuncPtr (abi, params, ret))

(* The "C" of extern "C" func exit(code: i32) never *)
and parse_abi st =
  match st.tok with
  | STRING name ->
      let span = cur_span st in
      advance st;
      Ast.NamedAbi (name, span)
  | _ ->
      Diagnostic.emit st.diags
        (Diagnostic.with_found (cur_span st) "expected ABI name"
           (show_found_token st.tok));
      (* The junk standing in for the ABI would derail what follows it *)
      sync_to_depth_token st st.tok_depth st.tok_line (at_one_of [ FUNC ]);
      Ast.AbiError

(* pub *)
and parse_modifiers st =
  let rec go acc =
    match st.tok with
    | PUBLIC ->
        advance st;
        go (Ast.Pub :: acc)
    | _ -> List.rev acc
  in
  go []

(* x: i32 *)
and parse_fields st =
  parse_decl_list st ~what:"field" ~missing:missing_field (fun _ ->
      let name = expect_decl_name st in
      let t = typ_after st COLON in
      ({ field_name = name; field_typ = t } : field))

(* Red; Green; Blue *)
and parse_variants st =
  parse_decl_list st ~what:"variant" ~missing:Ast.missing_ident (fun _ ->
      expect_decl_name st)

(* struct point { x: i32; y: i32 } *)
and parse_struct_def st mods =
  let lo = cur_pos st in
  advance st;
  (* STRUCT *)
  let name = recover_decl_name st LBRACE in
  let fields = parse_braced st parse_fields in
  let hi = st.prev_end in
  {
    struct_name = name;
    fields;
    struct_modifiers = mods;
    struct_span = make_span st lo hi;
  }

(* enum Color { Red; Green; Blue } *)
and parse_enum_def st mods =
  let lo = cur_pos st in
  advance st;
  (* ENUM *)
  let name = recover_decl_name st LBRACE in
  let variants = parse_braced st parse_variants in
  let hi = st.prev_end in
  {
    enum_name = name;
    variants;
    enum_modifiers = mods;
    enum_span = make_span st lo hi;
  }

(* type binop = (i32, i32) i32 *)
and parse_alias_def st mods =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  advance st;
  let name = recover_decl_name st ASSIGN in
  let typ = recover_typ_after st depth no_stop ASSIGN in
  let hi = st.prev_end in
  ({
     alias_name = name;
     alias_typ = typ;
     alias_modifiers = mods;
     alias_span = make_span st lo hi;
   }
    : type_alias_def)

(* (a: i32, b: i32) or (fmt: cstr, ...) returns (params, variadic) *)
and parse_params st =
  expect st LPAREN;
  let depth = st.tok_depth in
  let params = ref [] in
  let variadic = ref false in
  let stop st = starts_member st || at_one_of [ COMMA; RPAREN; LBRACE ] st in
  let add lo name t =
    params :=
      ({
         param_name = name;
         param_typ = t;
         param_span = make_span st lo st.prev_end;
       }
        : param)
      :: !params
  in
  (* A missing name keeps the arity so the call sites still line up *)
  let parse_one () =
    let lo = cur_pos st in
    let name =
      recover st ~depth
        ~stop:(at_one_of [ COLON; COMMA; RPAREN ])
        ~parse:(fun () -> expect_decl_name st)
        ~fallback:(fun st _ -> Ast.missing_ident (cur_span st))
    in
    (* A name that never parsed leaves nothing to hang an annotation on *)
    if Option.is_none name.value && not (at st COLON) then begin
      add lo name { tdesc = ErrorType; tspan = cur_span st };
      false
    end
    else
      let line = st.tok_line in
      try
        add lo name (typ_after st COLON);
        true
      with ParseError d ->
        emit_parse_error st d;
        sync_to_depth_token st depth line stop;
        add lo name (error_typ st d);
        false
  in
  (* A list can only open with a name so an opener here is a mistyped `(` *)
  if closer_for st.tok <> None then begin
    emit_parse_error st (found_error st "expected identifier");
    drop_opener st;
    advance st
  end;
  if st.tok <> RPAREN then begin
    (* The sync after a bad parameter ate whatever separator it had *)
    let parsed = ref (parse_one ()) in
    while
      st.tok <> RPAREN
      && (not (is_params_end st.tok))
      && (not !variadic)
      && (st.tok = COMMA || is_param_start st)
    do
      let stuck = cur_pos st in
      if st.tok = COMMA then advance st
      else begin
        if !parsed then emit_parse_error st (param_sep_error st);
        skip_semi st
      end;
      if st.tok = ELLIPSIS then (
        advance st;
        variadic := true)
      else if st.tok <> RPAREN && not (is_params_end st.tok) then
        parsed := parse_one ();
      if cur_pos st = stuck then advance st
    done
  end;
  if !variadic && st.tok <> RPAREN then begin
    Diagnostic.emit st.diags
      (Diagnostic.error "`...` must be the last parameter"
      |> Diagnostic.at (cur_span st));
    while st.tok <> EOF && not (st.tok = RPAREN && st.tok_depth = depth) do
      advance st
    done
  end;
  (* The declaration still stands but a list that never closed hides its tail *)
  (try expect st RPAREN
   with ParseError d ->
     emit_parse_error st d;
     let span = recovery_span st d in
     add (Span.lo span) (Ast.missing_ident span) (error_typ st d));
  (List.rev !params, !variadic)

(* i32 *)
and parse_ret_type st =
  match st.tok with
  | LBRACE | AUTOSEMI | SEMI | EOF | ASSIGN -> None
  | tok when not (is_type_start tok) -> None
  | _ ->
      let depth = st.tok_depth in
      Some (recover_typ st depth (at_one_of [ LBRACE; ASSIGN ]))

(* func NAME(params) ret *)
and parse_signature st =
  expect st FUNC;
  let name = recover_decl_name st LPAREN in
  let params, variadic = parse_params st in
  let ret = parse_ret_type st in
  (name, params, ret, variadic)

(* func add(a: i32, b: i32) i32 { ... } *)
and parse_func_def st mods =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  let name, params, ret, variadic = parse_signature st in
  (* The declaration still stands when the body won't parse *)
  let body =
    (* A signature with a hole in it already said so once *)
    if has_hole params && not (at st LBRACE) then begin
      sync_to_item st depth;
      [ Expr { desc = ErrorExpr; span = cur_span st } ]
    end (* The body never opened so what stands here is junk *)
    else if not (at st LBRACE) then begin
      let d = expected_error st LBRACE in
      emit_parse_error st d;
      sync_past st depth d;
      [ Expr (error_expr st d) ]
    end
    else
      try parse_block st
      with ParseError d ->
        emit_parse_error st d;
        sync_to_item st depth;
        [ Expr (error_expr st d) ]
  in
  let hi = st.prev_end in
  {
    func_name = name;
    params;
    ret;
    body;
    func_modifiers = mods;
    variadic;
    extern_abi = NoAbi;
    func_span = make_span st lo hi;
  }

(* a + b * c *)
and parse_expr ?(no_struct_lit = false) st min_prec =
  let lo = cur_pos st in
  let lhs = ref (parse_prefix ~no_struct_lit st) in

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
        require_expr_start st op_span;
        let next_min_prec =
          match op.assoc with Left -> op.prec + 1 | Right -> op.prec
        in
        if op_tok = DOTDOT then begin
          let rhs = parse_expr ~no_struct_lit st next_min_prec in
          lhs := mk lo st (Range (!lhs, rhs))
        end
        else if op_tok = DOTDOTEQ then begin
          let rhs = parse_expr ~no_struct_lit st next_min_prec in
          lhs := mk lo st (RangeInclusive (!lhs, rhs))
        end
        else begin
          let rhs = parse_expr ~no_struct_lit st next_min_prec in
          lhs :=
            mk lo st
              (match assign_of op_tok with
              | Some base -> Assign (base, !lhs, rhs)
              | None -> BinOp (binop_of op_tok, !lhs, rhs))
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
and parse_prefix ?(no_struct_lit = false) st =
  let lo = cur_pos st in
  let unary desc =
    let span = cur_span st in
    advance st;
    require_expr_start st span;
    mk lo st (UnOp (desc, parse_prefix ~no_struct_lit st))
  in
  match st.tok with
  | BANG -> unary Not
  | PLUS -> unary Pos
  | MINUS -> unary Neg
  | TILDE -> unary BitNot
  | AMP -> unary AddressOf
  | STAR -> unary Deref
  | _ -> parse_postfix ~no_struct_lit st (parse_primary ~no_struct_lit st)

(* x.field, arr[i], f(args) *)
and parse_postfix ?(no_struct_lit = false) st (lhs : expr) =
  let lo = Span.lo lhs.span in
  let continue_with = parse_postfix ~no_struct_lit st in
  match st.tok with
  | DOT -> (
      advance st;
      let name, name_span = expect_ident_span st in
      match lhs.desc with
      | Ident head ->
          (* The member keeps sliding along so whatever is left owns it *)
          let member = ref (name, name_span) in
          let owner_rest = ref [] in
          while at st DOT do
            advance st;
            owner_rest := !member :: !owner_rest;
            member := expect_ident_span st
          done;
          let path =
            {
              owner = Nonempty.make (head, lhs.span) (List.rev !owner_rest);
              member = !member;
            }
          in
          if at st LBRACE && (not no_struct_lit) && opens_struct_lit st then begin
            advance st;
            let fields = parse_struct_lit_fields st in
            expect st RBRACE;
            let base, _ = path.member in
            let path_span = (path_expr path).span in
            continue_with
              (mk lo st (StructLit (path_names path, base, path_span, fields)))
          end
          else continue_with (path_expr path)
      | _ -> continue_with (mk lo st (FieldAccess (lhs, name, name_span))))
  | LBRACKET ->
      advance st;
      let idx = parse_index_arg st in
      expect st RBRACKET;
      continue_with (mk lo st (Index (lhs, idx)))
  | LPAREN ->
      advance st;
      let args = parse_comma_list st RPAREN in
      expect st RPAREN;
      continue_with (mk lo st (Call (lhs, args)))
  | _ -> lhs

(* i, 1..3, 1.., ..3, .. *)
and parse_index_arg st =
  let lo = cur_pos st in
  (* Endpoints parse above `..` so a missing one leaves the loop alone *)
  let endpoint () = parse_expr st 3 in
  if at st DOTDOT then begin
    advance st;
    if at st RBRACKET then mk lo st RangeFull
    else mk lo st (RangeTo (endpoint ()))
  end
  else if at st DOTDOTEQ then begin
    advance st;
    mk lo st (RangeToInclusive (endpoint ()))
  end
  else
    let first = endpoint () in
    if at st DOTDOT then begin
      advance st;
      if at st RBRACKET then mk lo st (RangeFrom first)
      else mk lo st (Range (first, endpoint ()))
    end
    else if at st DOTDOTEQ then begin
      advance st;
      mk lo st (RangeInclusive (first, endpoint ()))
    end
    else first

(* 1, x, "str", foo(a, b) *)
and parse_primary ?(no_struct_lit = false) st =
  let lo = cur_pos st in
  match st.tok with
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
  | LPAREN ->
      advance st;
      if at st RPAREN then begin
        advance st;
        mk lo st Unit
      end
      else
        let e = parse_expr st 1 in
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
  (* bitcast(t) x *)
  | BITCAST ->
      advance st;
      expect st LPAREN;
      let t = parse_typ st in
      expect st RPAREN;
      let operand = parse_prefix ~no_struct_lit st in
      mk lo st (BitCast (operand, t))
  | IDENT name when (peek st).token = COLON ->
      parse_labeled_loop st (Interner.intern name)
  | IDENT name ->
      let name = Interner.intern name in
      let nspan = st.tok_span in
      advance st;
      if at st LBRACE && (not no_struct_lit) && opens_struct_lit st then begin
        advance st;
        let fields = parse_struct_lit_fields st in
        expect st RBRACE;
        mk lo st (StructLit ([], name, nspan, fields))
      end
      else mk lo st (Ident name)
  | STRING s ->
      advance st;
      mk lo st (String s)
  | UNDEFINED ->
      advance st;
      mk lo st Undefined
  | IF -> parse_if st
  | MATCH -> parse_match ~in_header:no_struct_lit st
  | LBRACE when no_struct_lit ->
      raise (ParseError (Diagnostic.expected_expression (cur_span st)))
  | LBRACE -> mk lo st (Block (parse_block st))
  | LOOP -> parse_loop st
  | ELSE ->
      raise
        (ParseError
           (Diagnostic.error
              (Printf.sprintf "`%s` without a matching `if`" (show_token st.tok))
           |> Diagnostic.at (cur_span st)
           |> Diagnostic.label
                (Printf.sprintf "found %s" (show_found_token st.tok))
           |> Diagnostic.help "an `if` used as a value closes at its `}`"))
  | _ -> fail_found st "expected expression"

(* a, b, c *)
and parse_comma_list st stop = comma_sep st stop (fun () -> parse_expr st 1)

(* x: 3, y: 4 or 3, 4 *)
and parse_struct_lit_fields st =
  skip_semi st;
  let field_form () =
    match st.tok with
    | IDENT _ when (peek st).token = COLON -> NamedField
    | _ -> PositionalField
  in
  let form = field_form () in
  let wanted =
    match form with NamedField -> "named" | PositionalField -> "positional"
  in
  let parse_field () =
    (* A token that starts no field at all gets the normal parse error *)
    if is_expr_start st.tok && field_form () <> form then
      raise
        (ParseError
           (Diagnostic.error "mixed struct fields"
           |> Diagnostic.at (cur_span st)
           |> Diagnostic.label ("expected a " ^ wanted ^ " field")));
    match form with
    | NamedField ->
        let name, nspan = expect_ident_span st in
        expect st COLON;
        (Some name, nspan, parse_expr st 1)
    | PositionalField ->
        let e = parse_expr st 1 in
        (None, e.span, e)
  in
  let fields = ref [] in
  while st.tok <> RBRACE do
    fields := parse_field () :: !fields;
    expect_literal_field_sep st
  done;
  List.rev !fields

(* The x < len of if x < len { } *)
and parse_header_expr st =
  match parse_expr ~no_struct_lit:true st 1 with
  | e -> e
  | exception ParseError d when at st LBRACE ->
      emit_parse_error st d;
      error_expr st d

(* x = 1 or f(x) *)
and parse_simple_stmt ?(no_pair = false) st =
  let lo = cur_pos st in
  match st.tok with
  (* const N: i32 = 4 / var x: i32 / var x = 42 / var x *)
  | (CONST | VAR) as tok ->
      let depth = st.tok_depth in
      advance st;
      let kind = match tok with CONST -> Ast.Const | _ -> Ast.Var in
      let name, nspan = expect_binding_name st in
      let ann = optional_annotation st depth in
      (* Only var may omit the value *)
      let e =
        if kind <> Ast.Var || at st ASSIGN then Some (recover_init st depth)
        else None
      in
      mk lo st (Binding (kind, name, nspan, ann, e))
  | BREAK ->
      advance st;
      let target = parse_loop_target st in
      if is_semi st.tok || st.tok = RBRACE || st.tok = EOF || at st COMMA then
        mk lo st (Break (target, None))
      else mk lo st (Break (target, Some (parse_expr st 1)))
  | CONTINUE ->
      advance st;
      mk lo st (Continue (parse_loop_target st))
  | RETURN ->
      (* Return with no value ends at a newline or closing brace *)
      advance st;
      if is_semi st.tok || st.tok = RBRACE || st.tok = EOF || at st COMMA then
        mk lo st (Return None)
      else
        let e = parse_expr st 1 in
        mk lo st (Return (Some e))
  | _ ->
      let first = parse_expr st 1 in
      if at st COMMA && not no_pair then parse_pair_assign st lo first
      else first

(* a, b = b, a *)
and parse_pair_assign state lo ft =
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
and parse_block st = (parse_block_span st).value

(* { return a + b } with the braces kept for a diagnostic to point at *)
and parse_block_span st =
  let lo = cur_pos st in
  expect st LBRACE;
  let body = parse_stmts st in
  expect st RBRACE;
  spanned body (make_span st lo st.prev_end)

(* stmt; stmt *)
and parse_stmts st =
  let stmts = ref [] in
  let depth = st.tok_depth in
  let after_auto_semi = ref false in
  skip_semi st;
  while st.tok <> RBRACE && st.tok <> EOF do
    let line = st.tok_line in
    let start_token = st.tok in
    let start_span = cur_span st in
    let follows_auto_semi = !after_auto_semi in
    after_auto_semi := false;
    try
      let s = parse_stmt st in
      if follows_auto_semi then
        begin match s with
        | Expr e -> diagnose_dropped_continuation st start_token start_span e
        | Decl _ -> ()
        end;
      stmts := s :: !stmts;
      if is_semi st.tok then begin
        after_auto_semi := st.tok = AUTOSEMI;
        skip_semi st
      end
      else if st.tok <> RBRACE && st.tok <> EOF then
        if not (is_next_line_start st line is_stmt_start) then
          fail_found st "expected `;`"
    with ParseError d ->
      after_auto_semi := false;
      (* The block reports the missing closer so eof is its news to tell *)
      if st.tok <> EOF then emit_parse_error st d;
      sync_to_stmt st depth line false;
      stmts := Expr (error_expr st d) :: !stmts;
      skip_semi st
  done;
  List.rev !stmts

(* var n = 1 or if c { } or return x *)
and parse_stmt ?(no_pair = false) st =
  let lo = cur_pos st in
  match st.tok with
  | _ when is_stray_keyword st ->
      drop_stray_keyword st;
      parse_stmt ~no_pair st
  | IF -> Expr (parse_if st)
  | MATCH -> Expr (parse_match st)
  | WHILE -> Expr (parse_while st)
  | FOR -> Expr (parse_for st)
  | LOOP -> Expr (parse_loop st)
  | IDENT name when (peek st).token = COLON ->
      Expr (parse_labeled_loop st (Interner.intern name))
  | LBRACE ->
      let body = parse_block st in
      Expr (mk lo st (Block body))
  | FUNC when (peek st).token = LPAREN -> Expr (parse_simple_stmt ~no_pair st)
  | PUBLIC | FUNC | STRUCT | TYPE | ENUM -> parse_local_decl st
  | _ -> Expr (parse_simple_stmt ~no_pair st)

(* pub type small = i32 inside a body *)
and parse_local_decl st =
  let modifiers = parse_modifiers st in
  let decl =
    match st.tok with
    | STRUCT -> LocalStruct (parse_struct_def st modifiers)
    | TYPE -> LocalTypeAlias (parse_alias_def st modifiers)
    | FUNC -> LocalFunc (parse_func_def st modifiers)
    | ENUM -> LocalEnum (parse_enum_def st modifiers)
    | _ -> fail_found st "expected local declaration"
  in
  Decl decl

(* if x < 0 { return lo } else if x > 0 { 1 } else { 0 } *)
and parse_if st =
  let lo = cur_pos st in
  advance st;
  (* IF *)
  let cond = parse_header_expr st in
  let body = parse_block_span st in
  let rec parse_elseifs acc =
    if st.tok = ELSE then begin
      advance st;
      (* ELSE *)
      if st.tok = IF then begin
        advance st;
        (* IF *)
        let c = parse_header_expr st in
        parse_elseifs ((c, parse_block_span st) :: acc)
      end
      else (List.rev acc, Some (parse_block_span st))
    end
    else (List.rev acc, None)
  in
  let elseifs, else_body = parse_elseifs [] in
  mk lo st (If ((cond, body) :: elseifs, else_body))

(* match c { Color.Red => 0; _ => 1 } *)
and parse_match ?(in_header = false) st =
  let lo = cur_pos st in
  advance st;
  (* MATCH *)
  let scrutinee = parse_header_expr st in
  (* The brace belongs to the header outside so the arms were never here *)
  if in_header && is_error_expr scrutinee then scrutinee
  else parse_arms st lo scrutinee

(* { Color.Red => 0; _ => 1 } *)
and parse_arms st lo scrutinee =
  let outer = st.tok_depth in
  (* No arm is a statement so none of them outlives a missing `{` *)
  match expect st LBRACE with
  | exception ParseError d ->
      emit_parse_error st d;
      sync_past st outer d;
      error_expr st d
  | () ->
      let depth = st.tok_depth in
      let arms = ref [] in
      while st.tok <> RBRACE && st.tok <> EOF do
        let line = st.tok_line in
        let stuck = cur_pos st in
        (try
           arms := parse_arm st :: !arms;
           expect_decl_sep st ~what:"arm"
         with ParseError d ->
           emit_parse_error st d;
           sync_to_depth_token st depth line no_stop;
           skip_semi st);
        if cur_pos st = stuck then advance st
      done;
      expect st RBRACE;
      mk lo st (Match (scrutinee, List.rev !arms))

(* Color.Red => 0 *)
and parse_arm st =
  let lo = cur_pos st in
  let pat = parse_pattern st in
  let arrow = cur_span st in
  let line = st.tok_line in
  expect st FATARROW;
  if st.tok_line > line then
    raise (ParseError (Diagnostic.expected_expression arrow));
  let body_lo = cur_pos st in
  let body =
    spanned [ parse_stmt ~no_pair:true st ] (make_span st body_lo st.prev_end)
  in
  { pat; arm_body = body; arm_span = make_span st lo st.prev_end }

(* _ / n / Color.Red *)
and parse_pattern st =
  let lo = cur_pos st in
  let done_ pdesc = { pdesc; pspan = make_span st lo st.prev_end } in
  match st.tok with
  | UNDERSCORE ->
      advance st;
      done_ PatWild
  | IDENT name when (peek st).token <> DOT ->
      advance st;
      done_ (PatBind (Interner.intern name))
  | _ ->
      let e = parse_expr ~no_struct_lit:true st 1 in
      { pdesc = PatValue e; pspan = e.span }

(* while i < len { } *)
and parse_while ?label st =
  let lo = loop_lo st label in
  advance st;
  (* WHILE *)
  let cond = parse_header_expr st in
  let body = parse_block st in
  mk lo st (While (label, cond, body))

(* break :outer *)
and parse_loop_target st =
  if at st COLON then (
    advance st;
    let name, nspan = expect_ident_span st in
    Some (Ast.spanned name nspan))
  else None

(* for i in 0..len { } *)
and parse_for ?label st =
  let lo = loop_lo st label in
  advance st;
  (* FOR *)
  let name, nspan = expect_ident_span st in
  expect st IN;
  let iter = parse_header_expr st in
  let body = parse_block st in
  mk lo st (For (label, name, nspan, iter, body))

(* loop { } *)
and parse_loop ?label st =
  let lo = loop_lo st label in
  advance st;
  (* LOOP *)
  let body = parse_block st in
  mk lo st (Loop (label, body))

(* outer: for row in grid { } *)
and parse_labeled_loop st name =
  let nspan = cur_span st in
  advance st;
  (* IDENT *)
  advance st;
  (* COLON *)
  let label = Ast.spanned name nspan in
  match st.tok with
  | WHILE -> parse_while ~label st
  | FOR -> parse_for ~label st
  | LOOP -> parse_loop ~label st
  | _ -> fail_found st "expected a loop after a label"

(* let PAGE_SIZE: i32 = 4096 / var n: i32 = 0 / var flag: bool *)
let parse_global st mods =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  let kind = match st.tok with CONST -> Ast.Const | _ -> Ast.Var in
  advance st;
  let name = expect_decl_name st in
  let typ = optional_annotation st depth in
  let init = if at st ASSIGN then Some (recover_init st depth) else None in
  let hi = st.prev_end in
  Global { name; typ; init; kind; modifiers = mods; span = make_span st lo hi }

(* extern "C" func add(a: i32, b: i32) i32 *)
let parse_extern st =
  let lo = cur_pos st in
  advance st;
  (* EXTERN *)
  let extern_abi = parse_abi st in
  let name, params, ret, variadic = parse_signature st in
  let hi = st.prev_end in
  Extern
    {
      func_name = name;
      params;
      ret;
      body = [];
      func_modifiers = [];
      variadic;
      extern_abi;
      func_span = make_span st lo hi;
    }

(* pub func f() i32 { } *)
let parse_decl st =
  let err () =
    match st.tok with
    | RPAREN | RBRACKET | RBRACE -> fail st "unexpected closing delimiter"
    | _ -> fail_found st "expected declaration"
  in
  let mods = parse_modifiers st in
  let rec dispatch mods =
    match (mods, st.tok) with
    | _, _ when is_stray_keyword st ->
        drop_stray_keyword st;
        dispatch (mods @ parse_modifiers st)
    | _, STRUCT -> Struct (parse_struct_def st mods)
    | _, FUNC -> Func (parse_func_def st mods)
    | [], EXTERN when opens_extern st -> parse_extern st
    | _ :: _, EXTERN when opens_extern st ->
        emit_parse_error st (found_error st "expected declaration");
        parse_extern st
    | _, (CONST | VAR) -> parse_global st mods
    | _, TYPE -> TypeAlias (parse_alias_def st mods)
    | _, ENUM -> Enum (parse_enum_def st mods)
    | _ -> err ()
  in
  dispatch mods

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

(* module math *)
let parse_module_header st =
  let lo = cur_pos st in
  advance st;
  let name = expect_ident st in
  let hi = st.prev_end in
  { Ast.name; span = make_span st lo hi }

(* module m; import a.b; then declarations *)
let parse_module st =
  let header = ref None in
  let imports = ref [] in
  let decls = ref [] in
  skip_semi st;
  if st.tok = MODULE then (
    try
      header := Some (parse_module_header st);
      if is_semi st.tok then skip_semi st
      else if st.tok <> EOF then fail_found st "expected `;`"
    with ParseError d ->
      emit_parse_error st d;
      sync_to_item st 0);
  while st.tok <> EOF do
    let line = st.tok_line in
    let stuck = cur_pos st in
    (* No declaration sits inside a delimiter so any still open never closes *)
    st.open_delims <- [];
    st.tok_depth <- 0;
    (* A broken declaration leaves junk but a missing `;` leaves the next one *)
    let parsed =
      try
        if st.tok = MODULE then fail st "`module` must be the first item"
        else if st.tok = IMPORT then imports := parse_import st :: !imports
        else decls := parse_decl st :: !decls;
        true
      with ParseError d ->
        emit_parse_error st d;
        sync_past st 0 d;
        false
    in
    if parsed then
      if is_semi st.tok then skip_semi st
      else if st.tok <> EOF && not (is_next_line_start st line is_item_start)
      then begin
        emit_parse_error st (found_error st "expected `;`");
        sync_to_item st 0
      end;
    if cur_pos st = stuck then advance st
  done;
  { header = !header; imports = List.rev !imports; decls = List.rev !decls }

let stream read lexbuf diags () =
  match read lexbuf with
  | ERROR msg, span, line ->
      Diagnostic.emit_error_at diags span msg;
      { token = ERROR msg; span; line }
  | token, span, line -> { token; span; line }

let parse ~diags (read : Lexing.lexbuf -> Tokens.token * Ast.span * int) lexbuf
    =
  let st =
    {
      tok = EOF;
      tok_span = dummy_span;
      tok_line = 1;
      tok_depth = 0;
      open_delims = [];
      read = stream read lexbuf diags;
      diags;
      prev_end = 0;
      ahead = [];
    }
  in
  advance st;
  parse_module st
