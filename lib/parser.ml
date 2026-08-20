(* SPDX-License-Identifier: Apache-2.0 *)

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
  mutable prev_end : int;
  mutable ahead : token_info option;
}

type chain = Comparison | Range
type assoc = Left | Right
type infix = { prec : int; assoc : assoc; chain : chain option }
type field_form = NamedField | PositionalField

(* Span of the current lookahead token so the caret lands under it *)
let cur_span st = st.tok_span

let peek st =
  match st.ahead with
  | Some info -> info
  | None ->
      let info = st.read () in
      st.ahead <- Some info;
      info

let advance st =
  st.prev_end <- Span.hi st.tok_span;
  let info =
    match st.ahead with
    | Some info ->
        st.ahead <- None;
        info
    | None -> st.read ()
  in
  st.tok <- info.token;
  st.tok_span <- info.span;
  st.tok_line <- info.line;
  st.tok_depth <- info.depth

let at st t = st.tok = t

(* Start of the current lookahead token *)
let cur_pos st = Span.lo st.tok_span

let loop_lo st (label : Ast.loop_label option) : int =
  match label with
  | Some (l : Ast.loop_label) -> Span.lo l.span
  | None -> cur_pos st

let fail st headline =
  raise (ParseError (Diagnostic.error headline |> Diagnostic.at (cur_span st)))

let fail_found st headline =
  raise
    (ParseError
       (Diagnostic.with_found (cur_span st) headline (show_found_token st.tok)))

let is_expr_start : token -> bool = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | TRUE | FALSE | NULL | SIZEOF | BITCAST | LPAREN | LBRACKET
  | UNDEFINED | IF | LBRACE | LOOP | MATCH ->
      true
  | _ -> false

let require_expr_start st span =
  if not (is_expr_start st.tok) then
    raise (ParseError (Diagnostic.expected_expression span))

let is_type_start : token -> bool = function
  | IDENT _ | STAR | LBRACKET | FUNC | EXTERN | LPAREN -> true
  | _ -> false

let is_semi : token -> bool = function AUTOSEMI | SEMI -> true | _ -> false

let skip_semi st =
  while is_semi st.tok do
    advance st
  done

let is_ambiguous_continuation : token -> bool = function
  | PLUS | MINUS | STAR | AMP -> true
  | _ -> false

let is_dereference_assignment (token : token) (e : expr) : bool =
  match e.desc with Assign _ -> token = STAR | _ -> false

let diagnose_dropped_continuation (st : state) (token : token) (span : span)
    (expr : expr) : unit =
  if
    is_ambiguous_continuation token
    && not (is_dereference_assignment token expr)
  then
    Diagnostic.emit st.diags
      (Diagnostic.error "operator starts a new statement after a newline"
      |> Diagnostic.at span
      |> Diagnostic.help "move the operator to the previous line")

let is_stmt_start : token -> bool = function
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | COMPTIME | VAR | RETURN | IF | WHILE | FOR | BREAK | CONTINUE
  | TRUE | FALSE | NULL | SIZEOF | BITCAST | LPAREN | LBRACE | LBRACKET
  | UNDEFINED | LOOP | MATCH ->
      true
  | _ -> false

let is_item_start : token -> bool = function
  | FUNC | EXTERN | STRUCT | PUBLIC | TYPE | IMPORT | COMPTIME | VAR | ENUM ->
      true
  | _ -> false

let is_next_line_start (st : state) (line : int) (is_start : token -> bool) :
    bool =
  st.tok_line > line && is_start st.tok

let rec sync_to_stmt (st : state) (depth : int) (line : int) (after_semi : bool)
    : unit =
  match st.tok with
  | EOF -> ()
  | RBRACE when st.tok_depth = depth -> ()
  | _
    when st.tok_depth = depth && is_stmt_start st.tok
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

let expect_binding_name st =
  match st.tok with
  | UNDERSCORE ->
      let span = st.tok_span in
      advance st;
      (Interner.intern "_", span)
  | _ -> expect_ident_span st

let expect st t =
  if st.tok <> t then
    fail_found st (Printf.sprintf "expected `%s`" (show_token t))
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
    if is_semi st.tok then fail st "missing `,` before newline"
  end;
  List.rev !items

let make_span _st lo hi = Span.make lo hi
let mk lo st desc = { desc; span = make_span st lo st.prev_end }
let mkt lo st tdesc = { tdesc; tspan = make_span st lo st.prev_end }

let recovery_span (st : state) (d : Diagnostic.t) : span =
  Option.value d.Diagnostic.primary ~default:(cur_span st)

let error_expr (st : state) (d : Diagnostic.t) : expr =
  { desc = ErrorExpr; span = recovery_span st d }

let error_typ (st : state) (d : Diagnostic.t) : typ =
  { tdesc = ErrorType; tspan = recovery_span st d }

let rec sync_to_depth_token (st : state) (depth : int) (line : int)
    (stops : token list) : unit =
  if
    st.tok = EOF
    || st.tok_depth = depth
       && (List.mem st.tok stops || is_semi st.tok || st.tok = RBRACE)
    || st.tok_depth = depth && st.tok_line > line
       && if depth = 0 then is_item_start st.tok else is_stmt_start st.tok
  then ()
  else (
    advance st;
    sync_to_depth_token st depth line stops)

let recover (st : state) (depth : int) (stops : token list) (parse : unit -> 'a)
    (on_error : state -> Diagnostic.t -> 'a) : 'a =
  let line = st.tok_line in
  try parse ()
  with ParseError d ->
    Diagnostic.emit st.diags d;
    sync_to_depth_token st depth line stops;
    on_error st d

let expect_decl_sep st =
  match st.tok with
  | AUTOSEMI | SEMI -> skip_semi st
  | RBRACE -> ()
  | _ -> fail_found st "expected `;` or newline between fields"

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
  | EXTERN ->
      advance st;
      parse_func_ptr st lo (parse_abi st)
  | STAR ->
      advance st;
      mkt lo st (Pointer (parse_typ st))
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
and recover_typ st depth stops =
  recover st depth stops (fun () -> parse_typ st) error_typ

and recover_typ_after st depth stops tok =
  recover st depth stops
    (fun () ->
      expect st tok;
      parse_typ st)
    error_typ

and recover_expr st depth stops =
  recover st depth stops (fun () -> parse_expr st 1) error_expr

and optional_annotation st depth =
  if at st COLON then (
    advance st;
    Some (recover_typ st depth [ ASSIGN ]))
  else None

(* func (i32, i32) i32, extern "C" func (i32) i32 *)
and parse_func_ptr st lo abi =
  expect st FUNC;
  expect st LPAREN;
  let params = comma_sep st RPAREN (fun () -> parse_typ st) in
  expect st RPAREN;
  let ret = if is_type_start st.tok then Some (parse_typ st) else None in
  mkt lo st (FuncPtr (abi, params, ret))

(* The "C" in extern "C" func exit(code: i32) never *)
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
      sync_to_depth_token st st.tok_depth st.tok_line [ FUNC ];
      Ast.AbiError

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
  let fields = ref [] in
  let depth = st.tok_depth in
  while st.tok <> RBRACE do
    let name, nspan = expect_ident_span st in
    let t = recover_typ_after st depth [] COLON in
    fields :=
      ({ field_name = name; field_typ = t; field_span = nspan } : field)
      :: !fields;
    expect_decl_sep st
  done;
  List.rev !fields

(* struct point { x: i32, y: i32 } *)
and parse_struct_def st mods =
  let lo = cur_pos st in
  advance st;
  (* STRUCT *)
  let name, name_span = expect_ident_span st in
  expect st LBRACE;
  let fields = parse_fields st in
  expect st RBRACE;
  let hi = st.prev_end in
  {
    struct_name = name;
    struct_name_span = name_span;
    fields;
    struct_modifiers = mods;
    struct_span = make_span st lo hi;
  }

(* enum Color { Red, Green, Blue } *)
and parse_enum_def st mods =
  let lo = cur_pos st in
  advance st;
  (* ENUM *)
  let name, name_span = expect_ident_span st in
  expect st LBRACE;
  let variants = ref [] in
  while st.tok <> RBRACE do
    let vname, vspan = expect_ident_span st in
    variants := { variant_name = vname; variant_span = vspan } :: !variants;
    expect_decl_sep st
  done;
  expect st RBRACE;
  let hi = st.prev_end in
  {
    enum_name = name;
    enum_name_span = name_span;
    variants = List.rev !variants;
    enum_modifiers = mods;
    enum_span = make_span st lo hi;
  }

(* type binop = (i32, i32) i32 *)
and parse_alias_def st mods =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  advance st;
  let name, name_span = expect_ident_span st in
  let typ = recover_typ_after st depth [] ASSIGN in
  let hi = st.prev_end in
  ({
     alias_name = name;
     alias_name_span = name_span;
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
  let parse_one () =
    let lo = cur_pos st in
    let name = expect_ident st in
    let t = recover_typ_after st depth [ COMMA; RPAREN ] COLON in
    let hi = st.prev_end in
    ({ param_name = name; param_typ = t; param_span = make_span st lo hi }
      : param)
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
  if is_semi st.tok then fail st "missing `,` before newline";
  expect st RPAREN;
  (List.rev !params, !variadic)

(* i32 *)
and parse_ret_type st =
  match st.tok with
  | LBRACE | AUTOSEMI | SEMI | EOF | ASSIGN -> None
  | _ ->
      let depth = st.tok_depth in
      Some (recover_typ st depth [ LBRACE; ASSIGN ])

(* func NAME(params) ret *)
and parse_signature st =
  expect st FUNC;
  let name, name_span = expect_ident_span st in
  let params, variadic = parse_params st in
  let ret = parse_ret_type st in
  (name, name_span, params, ret, variadic)

(* func add(a: i32, b: i32) i32 { ... } *)
and parse_func_def st mods =
  let lo = cur_pos st in
  let name, name_span, params, ret, variadic = parse_signature st in
  let body = parse_block st in
  let hi = st.prev_end in
  {
    func_name = name;
    func_name_span = name_span;
    params;
    ret;
    body;
    func_modifiers = mods;
    variadic;
    extern_abi = NoAbi;
    func_span = make_span st lo hi;
  }

(* Postfix binds tighter than infix so a.b + c means (a.b) + c *)

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
          let rev = ref [ (name, name_span); (head, lhs.span) ] in
          while at st DOT do
            advance st;
            rev := expect_ident_span st :: !rev
          done;
          let segs = List.rev !rev in
          if at st LBRACE && not no_struct_lit then begin
            advance st;
            let fields = parse_struct_lit_fields st in
            expect st RBRACE;
            let base, module_path =
              match !rev with
              | (base, _) :: rest -> (base, List.rev_map fst rest)
              | [] -> assert false
            in
            let path_span = (path_expr segs).span in
            continue_with
              (mk lo st (StructLit (module_path, base, path_span, fields)))
          end
          else continue_with (path_expr segs)
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
  (* Range endpoints parse one level above `..` so a missing one leaves the operator loop alone *)
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
      if at st LBRACE && not no_struct_lit then begin
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
  | MATCH -> parse_match st
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

(* IDENT { in an if/while/for header is the body, not a struct literal *)
and parse_header_expr st = parse_expr ~no_struct_lit:true st 1

and parse_simple_stmt ?(no_pair = false) st =
  let lo = cur_pos st in
  match st.tok with
  (* comptime N: i32 = 4 / var x: i32 / var x = 42 / var x *)
  | (COMPTIME | VAR) as tok ->
      let depth = st.tok_depth in
      advance st;
      let kind = match tok with COMPTIME -> Ast.Comptime | _ -> Ast.Var in
      let name, nspan = expect_binding_name st in
      let ann = optional_annotation st depth in
      (* Only var may omit the value *)
      let e =
        if kind <> Ast.Var then (
          expect st ASSIGN;
          Some (recover_expr st depth []))
        else if at st ASSIGN then (
          advance st;
          Some (recover_expr st depth []))
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
and parse_block st = (parse_block_span st).value

(* The braces are the arm a diagnostic points at *)
and parse_block_span st =
  let lo = cur_pos st in
  expect st LBRACE;
  let body = parse_stmts st in
  expect st RBRACE;
  spanned body (make_span st lo st.prev_end)

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
      Diagnostic.emit st.diags d;
      sync_to_stmt st depth line false;
      stmts := Expr (error_expr st d) :: !stmts;
      skip_semi st
  done;
  List.rev !stmts

and parse_stmt ?(no_pair = false) st =
  let lo = cur_pos st in
  match st.tok with
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
  | PUBLIC | FUNC | STRUCT | TYPE | ENUM -> parse_local_decl st
  | _ -> Expr (parse_simple_stmt ~no_pair st)

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

(* match c { Color.Red => 0, _ => 1 } *)
and parse_match st =
  let lo = cur_pos st in
  advance st;
  (* MATCH *)
  let scrutinee = parse_header_expr st in
  expect st LBRACE;
  let arms = ref [] in
  while st.tok <> RBRACE do
    arms := parse_arm st :: !arms;
    expect_decl_sep st
  done;
  expect st RBRACE;
  mk lo st (Match (scrutinee, List.rev !arms))

and parse_arm st =
  let lo = cur_pos st in
  let pat = parse_pattern st in
  expect st FATARROW;
  let body_lo = cur_pos st in
  let body =
    spanned [ parse_stmt ~no_pair:true st ] (make_span st body_lo st.prev_end)
  in
  { pat; arm_body = body; arm_span = make_span st lo st.prev_end }

(* A bare name binds and a dotted one names a constant *)
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
  let kind = match st.tok with COMPTIME -> Ast.Comptime | _ -> Ast.Var in
  advance st;
  let name, name_span = expect_ident_span st in
  let typ = optional_annotation st depth in
  let init =
    if at st ASSIGN then (
      advance st;
      Some (recover_expr st depth []))
    else None
  in
  let hi = st.prev_end in
  Global
    {
      name;
      name_span;
      typ;
      init;
      kind;
      modifiers = mods;
      span = make_span st lo hi;
    }

(* extern "C" func add(a: i32, b: i32) i32 *)
let parse_extern st =
  let lo = cur_pos st in
  advance st;
  (* EXTERN *)
  let extern_abi = parse_abi st in
  let name, name_span, params, ret, variadic = parse_signature st in
  let hi = st.prev_end in
  Extern
    {
      func_name = name;
      func_name_span = name_span;
      params;
      ret;
      body = [];
      func_modifiers = [];
      variadic;
      extern_abi;
      func_span = make_span st lo hi;
    }

let parse_decl st =
  let err () = fail_found st "expected declaration" in
  let mods = parse_modifiers st in
  match (mods, st.tok) with
  | _, STRUCT -> Struct (parse_struct_def st mods)
  | _, FUNC -> Func (parse_func_def st mods)
  (* Any module can declare the same foreign symbol so pub says nothing *)
  | [], EXTERN -> parse_extern st
  | _, (COMPTIME | VAR) -> parse_global st mods
  | _, TYPE -> TypeAlias (parse_alias_def st mods)
  | _, ENUM -> Enum (parse_enum_def st mods)
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

(* module math *)
let parse_module_header (st : state) : Ast.module_header =
  let lo = cur_pos st in
  advance st;
  let name = expect_ident st in
  let hi = st.prev_end in
  { Ast.name; span = make_span st lo hi }

let rec sync_to_item (st : state) : unit =
  match st.tok with
  | EOF -> ()
  | _ when st.tok_depth = 0 && is_item_start st.tok -> ()
  | _ ->
      advance st;
      sync_to_item st

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
      Diagnostic.emit st.diags d;
      sync_to_item st);
  while st.tok <> EOF do
    let line = st.tok_line in
    try
      if st.tok = MODULE then fail st "`module` must be the first item"
      else if st.tok = IMPORT then imports := parse_import st :: !imports
      else decls := parse_decl st :: !decls;
      if is_semi st.tok then skip_semi st
      else if st.tok <> EOF then
        if not (is_next_line_start st line is_item_start) then
          fail_found st "expected `;`"
    with ParseError d ->
      Diagnostic.emit st.diags d;
      sync_to_item st
  done;
  { header = !header; imports = List.rev !imports; decls = List.rev !decls }

let stream read lexbuf diags =
  let stack = ref [] in
  let depth = ref 0 in
  let rec next () =
    match read lexbuf with
    | ERROR msg, sp, _ ->
        Diagnostic.emit_error_at diags sp msg;
        next ()
    | t, sp, line -> (
        let info = { token = t; span = sp; line; depth = !depth } in
        match Bracket_check.step diags !stack t sp with
        | Bracket_check.End -> info
        | Bracket_check.Closed rest ->
            stack := rest;
            decr depth;
            info
        | Bracket_check.Stray | Bracket_check.Other -> info
        | Bracket_check.Open opener ->
            stack := (opener, sp) :: !stack;
            incr depth;
            info)
  in
  next

let parse ~(diags : Diagnostic.sink)
    (read : Lexing.lexbuf -> Tokens.token * Ast.span * int)
    (lexbuf : Lexing.lexbuf) : Ast.module_ =
  let parse_diags = Diagnostic.sink () in
  let st =
    {
      tok = EOF;
      tok_span = dummy_span;
      tok_line = 1;
      tok_depth = 0;
      read = stream read lexbuf diags;
      diags = parse_diags;
      prev_end = 0;
      ahead = None;
    }
  in
  advance st;
  let module_ = parse_module st in
  diags := !parse_diags @ !diags;
  module_
