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
  let found = Printf.sprintf "found %s" (show_found_token st.tok) in
  raise
    (ParseError
       (Diagnostic.error headline
       |> Diagnostic.at (cur_span st)
       |> Diagnostic.label found))

let is_expr_start (tok : token) : bool =
  match tok with
  | INT _ | FLOAT _ | IDENT _ | STRING _ | CHAR _ | PLUS | MINUS | STAR | AMP
  | TILDE | BANG | TRUE | FALSE | NULL | SIZEOF | LPAREN | LBRACKET | UNDEFINED
  | IF ->
      true
  | _ -> false

let require_expr_start st span =
  if not (is_expr_start st.tok) then
    raise (ParseError (Diagnostic.expected_expression span))

let is_type_start (tok : token) : bool =
  match tok with IDENT _ | STAR | LPAREN | LBRACKET -> true | _ -> false

let expect_type_after st span =
  if not (is_type_start st.tok) then
    raise (ParseError (Diagnostic.expected_type span))

let parse_cast_kind st op_span =
  match st.tok with
  | BANG ->
      let bang_span = cur_span st in
      advance st;
      (Checked, Span.make op_span.file op_span.lo bang_span.hi)
  | _ -> (Normal, op_span)

let is_postfix_tok = function DOT | LBRACKET | LPAREN -> true | _ -> false

let is_semi (token : token) : bool =
  match token with AUTOSEMI | SEMI -> true | _ -> false

let skip_semi st =
  while is_semi st.tok do
    advance st
  done

let is_ambiguous_continuation (token : token) : bool =
  match token with PLUS | MINUS | STAR | AMP -> true | _ -> false

let is_dereference_assignment (token : token) (e : expr) : bool =
  match e.desc with
  | BinOp (op, _, _) -> token = STAR && Ast.is_assignment_op op
  | _ -> false

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
  | FUNC | EXTERN | STRUCT | PUBLIC | TYPE | NEWTYPE | IMPORT | LET | COMPTIME
  | VAR ->
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
    if is_semi st.tok then fail st "missing `,` before newline"
  end;
  List.rev !items

let make_span st lo hi = Span.make st.tok_span.file lo hi
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
  | AUTOSEMI | SEMI | RBRACE -> ()
  | _ -> fail_found st "expected `,` or newline between fields");
  skip_semi st

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

(* math.Point, math.vector.Point *)
let rec dotted_name (e : expr) : string list option =
  match e.desc with
  | Ident name -> Some [ name ]
  | FieldAccess (inner, name, _) ->
      Option.map (fun path -> path @ [ name ]) (dotted_name inner)
  | ErrorExpr | Int _ | Float _ | Bool _ | Null | Char _ | String _ | Call _
  | BinOp _ | UnOp _ | Cast _ | SizeOf _ | ArrayLit _ | Index _ | StructLit _
  | Block _ | If _ | While _ | For _ | Binding _ | Return _ | Break | Continue
  | Undefined | Range _ | RangeInclusive _ | PairAssign _ ->
      None

(* i32, *i32, (i32, i32) i32 *)
let rec parse_typ st =
  let lo = cur_pos st in
  match st.tok with
  | STAR ->
      advance st;
      mkt lo st (Pointer (parse_typ st))
  | IDENT name ->
      advance st;
      let path = ref [ name ] in
      while st.tok = DOT do
        advance st;
        path := expect_ident st :: !path
      done;
      let modules, base =
        match !path with base :: rest -> (List.rev rest, base) | [] -> ([], "")
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
      ({
         field_name = name;
         field_typ = t;
         field_modifiers = [];
         field_span = nspan;
       }
        : field)
      :: !fields;
    expect_field_sep st
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

(* type binop = (i32, i32) i32 / newtype Celsius = f32 *)
and parse_alias_def st mods =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  advance st;
  (* TYPE or NEWTYPE *)
  let name, name_span = expect_ident_span st in
  let typ =
    recover_typ_to st depth [] (fun () ->
        expect st ASSIGN;
        parse_typ st)
  in
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
    let t =
      recover_typ_to st depth [ COMMA; RPAREN ] (fun () ->
          expect st COLON;
          parse_typ st)
    in
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
      Some (recover_typ_to st depth [ LBRACE; ASSIGN ] (fun () -> parse_typ st))

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
  let body =
    if st.tok = ASSIGN then begin
      let slo = cur_pos st in
      advance st;
      let e = parse_expr st 1 in
      [ Expr (mk slo st (Return (Some e))) ]
    end
    else parse_block st
  in
  let hi = st.prev_end in
  {
    func_name = name;
    func_name_span = name_span;
    params;
    ret;
    body;
    func_modifiers = mods;
    variadic;
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
        if op_tok <> AS then require_expr_start st op_span;
        let next_min_prec =
          match op.assoc with Left -> op.prec + 1 | Right -> op.prec
        in
        if op_tok = AS then begin
          let kind, cast_op_span = parse_cast_kind st op_span in
          expect_type_after st cast_op_span;
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
          let rhs = parse_expr ~no_struct_lit st next_min_prec in
          lhs := mk lo st (Range (!lhs, rhs))
        end
        else if op_tok = DOTDOTEQ then begin
          let rhs = parse_expr ~no_struct_lit st next_min_prec in
          lhs := mk lo st (RangeInclusive (!lhs, rhs))
        end
        else begin
          let rhs = parse_expr ~no_struct_lit st next_min_prec in
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
  let lo = lhs.span.lo in
  let continue_with = parse_postfix ~no_struct_lit st in
  match st.tok with
  | DOT -> (
      advance st;
      let name, name_span = expect_ident_span st in
      let access = mk lo st (FieldAccess (lhs, name, name_span)) in
      (* The brace means this path names a type from another module *)
      match dotted_name access with
      | Some path when at st LBRACE && not no_struct_lit ->
          advance st;
          let fields = parse_struct_lit_fields st in
          expect st RBRACE;
          let path = List.rev path in
          let name, module_path =
            match path with
            | name :: module_path -> (name, List.rev module_path)
            | [] -> assert false
          in
          continue_with
            (mk lo st (StructLit (module_path, name, access.span, fields)))
      | Some _ | None -> continue_with access)
  | LBRACKET ->
      advance st;
      let idx = parse_expr st 1 in
      expect st RBRACKET;
      continue_with (mk lo st (Index (lhs, idx)))
  | LPAREN ->
      advance st;
      let args = parse_comma_list st RPAREN in
      expect st RPAREN;
      continue_with (mk lo st (Call (lhs, args)))
  | _ -> lhs

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

(* A block is a value too and where it sits decides if the value is used *)
and parse_value st =
  let lo = cur_pos st in
  match st.tok with
  | LBRACE -> mk lo st (Block (parse_block st))
  | _ -> parse_expr st 1

(* x: 3, y: 4 *)
and parse_struct_lit_fields st =
  skip_semi st;
  let fields = ref [] in
  while st.tok <> RBRACE do
    let name, nspan = expect_ident_span st in
    expect st COLON;
    let e = parse_expr st 1 in
    fields := (name, nspan, e) :: !fields;
    expect_literal_field_sep st
  done;
  List.rev !fields

(* IDENT { in an if/while/for header is the body, not a struct literal *)
and parse_header_expr st = parse_expr ~no_struct_lit:true st 1

and parse_simple_stmt st =
  let lo = cur_pos st in
  match st.tok with
  (* let x: i32 = 42 / const N: i32 = 4 / var x: i32 / var x = 42 / var x *)
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
      if is_semi st.tok || st.tok = RBRACE || st.tok = EOF then
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
      if st.recovered then (
        st.recovered <- false;
        skip_semi st)
      else if is_semi st.tok then begin
        after_auto_semi := st.tok = AUTOSEMI;
        skip_semi st
      end
      else if st.tok <> RBRACE && st.tok <> EOF then
        fail_found st "expected `;`"
    with ParseError d ->
      after_auto_semi := false;
      Diagnostic.emit st.diags d;
      sync_to_stmt st depth line false;
      stmts := Expr (error_expr st d) :: !stmts;
      skip_semi st
  done;
  List.rev !stmts

and parse_stmt st =
  let lo = cur_pos st in
  match st.tok with
  | IF -> Expr (parse_if st)
  | WHILE -> Expr (parse_while st)
  | FOR -> Expr (parse_for st)
  | LBRACE ->
      let body = parse_block st in
      Expr (mk lo st (Block body))
  | PUBLIC | FUNC | STRUCT | TYPE | NEWTYPE -> parse_local_decl st
  | _ -> Expr (parse_simple_stmt st)

and parse_local_decl st =
  let modifiers = parse_modifiers st in
  let decl =
    match st.tok with
    | STRUCT -> LocalStruct (parse_struct_def st modifiers)
    | TYPE -> LocalTypeAlias (parse_alias_def st modifiers)
    | NEWTYPE -> LocalNewtype (parse_alias_def st modifiers)
    | FUNC -> LocalFunc (parse_func_def st modifiers)
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

(* let PAGE_SIZE: i32 = 4096 / var n: i32 = 0 / var flag: bool *)
let parse_global st mods =
  let lo = cur_pos st in
  let depth = st.tok_depth in
  let kind =
    match st.tok with LET -> Ast.Let | COMPTIME -> Ast.Comptime | _ -> Ast.Var
  in
  advance st;
  let name, name_span = expect_ident_span st in
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

(* extern func add(a: i32, b: i32) i32 *)
let parse_extern st =
  let lo = cur_pos st in
  advance st;
  (* EXTERN *)
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
  | _, (LET | COMPTIME | VAR) -> parse_global st mods
  | _, TYPE -> TypeAlias (parse_alias_def st mods)
  | _, NEWTYPE -> Newtype (parse_alias_def st mods)
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
    try
      if st.tok = MODULE then fail st "`module` must be the first item"
      else if st.tok = IMPORT then imports := parse_import st :: !imports
      else decls := parse_decl st :: !decls;
      if st.recovered then (
        st.recovered <- false;
        skip_semi st)
      else if is_semi st.tok then skip_semi st
      else if st.tok <> EOF then fail_found st "expected `;`"
    with ParseError d ->
      Diagnostic.emit st.diags d;
      sync_to_item st
  done;
  { header = !header; imports = List.rev !imports; decls = List.rev !decls }

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
      prev_end = 0;
      recovered = false;
    }
  in
  advance st;
  parse_module st
