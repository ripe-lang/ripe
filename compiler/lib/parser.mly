(* SPDX-License-Identifier: GPL-2.0-only *)

%{
open Ast
%}

(* TODO: It's going to be a long time before I boot strap this. So I'll probably rewrite this to be a recursive descent parser *)

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
%token COMMA COLON NEWLINE DOTDOT DOT SEMI
%token EOF

(* TODO: add remaining compound assignments *)
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
  | list(NEWLINE); decls = list(decl); EOF { decls }

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
    ret = ret_type; list(NEWLINE)
    { Extern { name; params; ret; body = []; modifiers = [] } }
  (* [modifiers] name(...) { ... } *)
  | mods = list(modifier); name = IDENT; LPAREN; params = separated_list(COMMA, param); RPAREN;
    ret = ret_type;
    list(NEWLINE); LBRACE; list(NEWLINE); body = stmts; RBRACE; list(NEWLINE)
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
  | f = field; COMMA; list(NEWLINE)                              { [f] }
  (* more fields follow *)
  | f = field; COMMA; list(NEWLINE); rest = nonempty_fields      { f :: rest }

struct_decl:
  (* empty struct: struct name { } *)
  | STRUCT; name = IDENT; list(NEWLINE);
    LBRACE; list(NEWLINE); RBRACE; list(NEWLINE)
    { Struct { name; fields = []; modifiers = [] } }
  (* non-empty struct: struct name { fields } *)
  | STRUCT; name = IDENT; list(NEWLINE);
    LBRACE; list(NEWLINE); fs = nonempty_fields; list(NEWLINE); RBRACE; list(NEWLINE)
    { Struct { name; fields = fs; modifiers = [] } }

typ:
  | name = IDENT   { Named name }
  | CARET; t = typ { Pointer t }

block:
  (* { stmts } *)
  | list(NEWLINE); LBRACE; list(NEWLINE); body = stmts; RBRACE; list(NEWLINE)
    { body }

elseif_clause:
  | ELSEIF; cond = expr; body = block { (cond, body) }

else_clause:
  | ELSE; body = block { body }

(* BUG: simple_stmt requires a newline so single line blocks fail e.g. if x { return a } *)
stmts:
  |                                                        { [] }
  | s = simple_stmt; nonempty_list(NEWLINE); rest = stmts  { s :: rest }
  | s = block_stmt;  rest = stmts                          { s :: rest }

simple_stmt:
  | LET; name = IDENT; t = option(preceded(COLON, typ)); ASSIGN; e = expr { Let (name, t, e) }
  | VAR; name = IDENT; t = option(preceded(COLON, typ)); ASSIGN; e = expr { Var (name, t, e) }
  | RETURN; e = expr { Return (Some e) }
  | RETURN           { Return None }
  | BREAK            { Break }
  | CONTINUE         { Continue }
  | e = expr         { Expr e }

block_stmt:
  | IF; cond = expr; body = block;
    elseifs = list(elseif_clause);
    else_body = option(else_clause)
    { If ((cond, body) :: elseifs, Option.value else_body ~default:[]) }
  | WHILE; cond = expr; body = block
    { While (cond, body) }
  | FOR; name = IDENT; IN; iter = expr; body = block
    { For (name, iter, body) }
  | FOR; init = simple_stmt; SEMI; cond = expr; SEMI; post = expr; body = block
    { CFor (init, cond, post, body) }
  | body = block
    { Block body }

string_part:
  | s = STRING_PART { Lit s }
  | INTERP_START; e = expr; INTERP_END { Interp e }

expr:
  | n = INT    { Int n }
  | f = FLOAT  { Float f }
  | STRING_START; parts = list(string_part); STRING_END { InterpString parts }
  | TRUE       { Bool true }
  | FALSE      { Bool false }
  | NULL       { Null }
  | SIZEOF; LPAREN; t = typ; RPAREN { SizeOf t }
  | name = IDENT; LPAREN; args = separated_list(COMMA, expr); RPAREN { Call (name, args) }
  | s = IDENT  { Ident s }
  | l = expr; PLUS;         r = expr { BinOp (Add,       l, r) }
  | l = expr; MINUS;        r = expr { BinOp (Sub,       l, r) }
  | l = expr; STAR;         r = expr { BinOp (Mul,       l, r) }
  | l = expr; SLASH;        r = expr { BinOp (Div,       l, r) }
  | l = expr; PERCENT;      r = expr { BinOp (Mod,       l, r) }
  | l = expr; EQ;           r = expr { BinOp (Eq,        l, r) }
  | l = expr; NEQ;          r = expr { BinOp (Neq,       l, r) }
  | l = expr; LT;           r = expr { BinOp (Lt,        l, r) }
  | l = expr; GT;           r = expr { BinOp (Gt,        l, r) }
  | l = expr; LTE;          r = expr { BinOp (Lte,       l, r) }
  | l = expr; GTE;          r = expr { BinOp (Gte,       l, r) }
  | l = expr; AND;          r = expr { BinOp (And,       l, r) }
  | l = expr; OR;           r = expr { BinOp (Or,        l, r) }
  | l = expr; AMP;          r = expr { BinOp (BitAnd,    l, r) }
  | l = expr; PIPE;         r = expr { BinOp (BitOr,     l, r) }
  | l = expr; TILDE;        r = expr { BinOp (BitXor,    l, r) }
  | l = expr; LSHIFT;       r = expr { BinOp (Lshift,    l, r) }
  | l = expr; RSHIFT;       r = expr { BinOp (Rshift,    l, r) }
  | l = expr; ASSIGN;       r = expr { BinOp (Assign,    l, r) }
  | l = expr; PLUS_ASSIGN;  r = expr { BinOp (AddAssign, l, r) }
  | l = expr; MINUS_ASSIGN; r = expr { BinOp (SubAssign, l, r) }
  | l = expr; STAR_ASSIGN;  r = expr { BinOp (MulAssign, l, r) }
  | l = expr; SLASH_ASSIGN; r = expr { BinOp (DivAssign, l, r) }
  | l = expr; DOTDOT;       r = expr { Range (l, r) }
  | BANG;  e = expr %prec BANG       { UnOp (Not,       e) }
  | MINUS; e = expr %prec UMINUS     { UnOp (Neg,       e) }
  | TILDE; e = expr %prec UBITNOT    { UnOp (BitNot,    e) }
  | INCR;  e = expr %prec PREFIX     { UnOp (PreInc,    e) }
  | DECR;  e = expr %prec PREFIX     { UnOp (PreDec,    e) }
  | AT;    e = expr %prec UAT        { UnOp (AddressOf, e) }
  | e = expr; INCR                   { UnOp (PostInc, e) }
  | e = expr; DECR                   { UnOp (PostDec, e) }
  | e = expr; CARET                  { UnOp (Deref,   e) }
  | LPAREN; e = expr; RPAREN         { e }
  | e = expr; DOT; fname = IDENT     { FieldAccess (e, fname) }
  | e = expr; AS; t = typ            { Cast (e, t) }
