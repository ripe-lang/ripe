(* SPDX-License-Identifier: GPL-2.0-only *)

%{
open Ast

let mk_expr desc = { desc; span = dummy_span }
let mk_stmt sdesc  = { sdesc; span = dummy_span }
%}

(* TODO(a1c1): It's going to be a long time before I boot strap this. So I'll probably rewrite this to be a recursive descent parser *)

%token <int>    INT
%token <float>  FLOAT
%token <string> IDENT
%token <string> STRING_PART
%token STRING_START STRING_END INTERP_START INTERP_END
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT GT LTE GTE
%token LSHIFT RSHIFT
%token AMP PIPE TILDE
%token AND OR BANG
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN
%token INCR DECR
%token LET VAR RETURN
%token IF ELSEIF ELSE WHILE FOR IN
%token BREAK CONTINUE
%token STRUCT EXTERN INLINE PUBLIC
%token CARET AT
%token TRUE FALSE NULL
%token AS SIZEOF
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA COLON DOTDOT DOT SEMI
%token EOF

(* TODO(2b10): add remaining compound assignments *)
(* precedence, lowest to highest *)
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN
%left  DOTDOT
%left  OR
%left  AND
%left  PIPE
%left  TILDE
%left  AMP
%nonassoc EQ NEQ
%nonassoc LT GT LTE GTE
%left  LSHIFT RSHIFT
%left  PLUS MINUS
%left  STAR SLASH PERCENT
%left  AS
%right BANG UMINUS UBITNOT PREFIX UAT
%left  INCR DECR CARET
%left  DOT

%start <Ast.decl list> program

%%

program:
  | list(SEMI); decls = list(decl); EOF { decls }

ret_type:
  | { None }
  | COLON; t = typ { Some t }
  | COLON { None }

modifier:
  | PUBLIC { Pub }
  | INLINE { Inline }

decl:
  | mods = list(modifier); s = struct_decl
    { match s with Struct sd -> Struct { sd with modifiers = mods } | d -> d }
  (* extern name(params): ret with no body *)
  | EXTERN; name = IDENT; LPAREN; params = separated_list(COMMA, param); RPAREN;
    ret = ret_type; list(SEMI)
    { Extern { name; params; ret; body = []; modifiers = [] } }
  (* [modifiers] name(...) { ... } *)
  | mods = list(modifier); name = IDENT; LPAREN; params = separated_list(COMMA, param); RPAREN;
    ret = ret_type;
    list(SEMI); LBRACE; body = stmts; RBRACE; list(SEMI)
    { Func { name; params; ret; body; modifiers = mods } }

param:
  | name = IDENT; COLON; t = typ { ({ name; typ = t } : Ast.param) }

field:
  | mods = list(modifier); name = IDENT; COLON; t = typ
    { ({ name; typ = t; modifiers = mods } : Ast.field) }

nonempty_fields:
  (* last field, no trailing comma *)
  | f = field                                                    { [f] }
  (* last field, optional trailing comma *)
  | f = field; COMMA; list(SEMI)                                 { [f] }
  (* more fields follow *)
  | f = field; COMMA; list(SEMI); rest = nonempty_fields         { f :: rest }

struct_decl:
  (* empty struct: struct name { } *)
  | STRUCT; name = IDENT; list(SEMI);
    LBRACE; RBRACE; list(SEMI)
    { Struct { name; fields = []; modifiers = [] } }
  (* non-empty struct: struct name { fields } *)
  | STRUCT; name = IDENT; list(SEMI);
    LBRACE; fs = nonempty_fields; list(SEMI); RBRACE; list(SEMI)
    { Struct { name; fields = fs; modifiers = [] } }

typ:
  | name = IDENT   { Named name }
  | CARET; t = typ { Pointer t }

block:
  | LBRACE; body = stmts; RBRACE; list(SEMI) { body }

elseif_clause:
  | ELSEIF; cond = expr; list(SEMI); body = block { (cond, body) }

else_clause:
  | ELSE; list(SEMI); body = block { body }

stmts:
  |                                                        { [] }
  | s = simple_stmt; nonempty_list(SEMI); rest = stmts     { s :: rest }
  | s = block_stmt;  rest = stmts                          { s :: rest }
  | SEMI; rest = stmts                                     { rest }

simple_stmt:
  | LET; name = IDENT; t = option(preceded(COLON, typ)); ASSIGN; e = expr
    { mk_stmt (Let (name, t, e)) }
  | VAR; name = IDENT; t = option(preceded(COLON, typ)); ASSIGN; e = expr
    { mk_stmt (Var (name, t, e)) }
  | RETURN; e = expr { mk_stmt (Return (Some e)) }
  | RETURN           { mk_stmt (Return None) }
  | BREAK            { mk_stmt Break }
  | CONTINUE         { mk_stmt Continue }
  | e = expr         { mk_stmt (Expr e) }

block_stmt:
  | IF; cond = expr; list(SEMI); body = block;
    elseifs = list(elseif_clause);
    else_body = option(else_clause)
    { mk_stmt (If ((cond, body) :: elseifs, Option.value else_body ~default:[])) }
  | WHILE; cond = expr; list(SEMI); body = block
    { mk_stmt (While (cond, body)) }
  | FOR; name = IDENT; IN; iter = expr; list(SEMI); body = block
    { mk_stmt (For (name, iter, body)) }
  | FOR; init = simple_stmt; SEMI; cond = expr; SEMI; post = expr; list(SEMI); body = block
    { mk_stmt (CFor (init, cond, post, body)) }
  | body = block
    { mk_stmt (Block body) }

string_part:
  | s = STRING_PART { Lit s }
  | INTERP_START; e = expr; INTERP_END { Interp e }

expr:
  | n = INT    { mk_expr (Int n) }
  | f = FLOAT  { mk_expr (Float f) }
  | STRING_START; parts = list(string_part); STRING_END { mk_expr (InterpString parts) }
  | TRUE       { mk_expr (Bool true) }
  | FALSE      { mk_expr (Bool false) }
  | NULL       { mk_expr Null }
  | SIZEOF; LPAREN; t = typ; RPAREN { mk_expr (SizeOf t) }
  | name = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN { mk_expr (Call (name, args)) }
  | s = IDENT  { mk_expr (Ident s) }
  | l = expr; PLUS;         r = expr { mk_expr (BinOp (Add,       l, r)) }
  | l = expr; MINUS;        r = expr { mk_expr (BinOp (Sub,       l, r)) }
  | l = expr; STAR;         r = expr { mk_expr (BinOp (Mul,       l, r)) }
  | l = expr; SLASH;        r = expr { mk_expr (BinOp (Div,       l, r)) }
  | l = expr; PERCENT;      r = expr { mk_expr (BinOp (Mod,       l, r)) }
  | l = expr; EQ;           r = expr { mk_expr (BinOp (Eq,        l, r)) }
  | l = expr; NEQ;          r = expr { mk_expr (BinOp (Neq,       l, r)) }
  | l = expr; LT;           r = expr { mk_expr (BinOp (Lt,        l, r)) }
  | l = expr; GT;           r = expr { mk_expr (BinOp (Gt,        l, r)) }
  | l = expr; LTE;          r = expr { mk_expr (BinOp (Lte,       l, r)) }
  | l = expr; GTE;          r = expr { mk_expr (BinOp (Gte,       l, r)) }
  | l = expr; AND;          r = expr { mk_expr (BinOp (And,       l, r)) }
  | l = expr; OR;           r = expr { mk_expr (BinOp (Or,        l, r)) }
  | l = expr; AMP;          r = expr { mk_expr (BinOp (BitAnd,    l, r)) }
  | l = expr; PIPE;         r = expr { mk_expr (BinOp (BitOr,     l, r)) }
  | l = expr; TILDE;        r = expr { mk_expr (BinOp (BitXor,    l, r)) }
  | l = expr; LSHIFT;       r = expr { mk_expr (BinOp (Lshift,    l, r)) }
  | l = expr; RSHIFT;       r = expr { mk_expr (BinOp (Rshift,    l, r)) }
  | l = expr; ASSIGN;       r = expr { mk_expr (BinOp (Assign,    l, r)) }
  | l = expr; PLUS_ASSIGN;  r = expr { mk_expr (BinOp (AddAssign, l, r)) }
  | l = expr; MINUS_ASSIGN; r = expr { mk_expr (BinOp (SubAssign, l, r)) }
  | l = expr; STAR_ASSIGN;  r = expr { mk_expr (BinOp (MulAssign, l, r)) }
  | l = expr; SLASH_ASSIGN; r = expr { mk_expr (BinOp (DivAssign, l, r)) }
  | l = expr; DOTDOT;       r = expr { mk_expr (Range (l, r)) }
  | BANG;  e = expr %prec BANG       { mk_expr (UnOp (Not,       e)) }
  | MINUS; e = expr %prec UMINUS     { mk_expr (UnOp (Neg,       e)) }
  | TILDE; e = expr %prec UBITNOT    { mk_expr (UnOp (BitNot,    e)) }
  | INCR;  e = expr %prec PREFIX     { mk_expr (UnOp (PreInc,    e)) }
  | DECR;  e = expr %prec PREFIX     { mk_expr (UnOp (PreDec,    e)) }
  | AT;    e = expr %prec UAT        { mk_expr (UnOp (AddressOf, e)) }
  | e = expr; INCR                   { mk_expr (UnOp (PostInc, e)) }
  | e = expr; DECR                   { mk_expr (UnOp (PostDec, e)) }
  | e = expr; CARET                  { mk_expr (UnOp (Deref,   e)) }
  | LPAREN; e = expr; RPAREN         { e }
  | e = expr; DOT; fname = IDENT     { mk_expr (FieldAccess (e, fname)) }
  | e = expr; AS; t = typ            { mk_expr (Cast (e, t)) }
