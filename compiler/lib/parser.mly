%{
open Ast
%}

%token <int>    INT
%token <string> IDENT
%token PLUS MINUS STAR SLASH PERCENT
%token EQ NEQ LT GT LTE GTE
%token LSHIFT RSHIFT
%token AMP PIPE TILDE
%token AND OR BANG
%token ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN
%token INCR DECR
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token EOF

(* precedence, lowest to highest *)
%right ASSIGN PLUS_ASSIGN MINUS_ASSIGN STAR_ASSIGN SLASH_ASSIGN
%left OR
%left AND
%left PIPE
%left TILDE
%left AMP
%nonassoc EQ NEQ
%nonassoc LT GT LTE GTE
%left LSHIFT RSHIFT
%left PLUS MINUS
%left STAR SLASH PERCENT
%right BANG UMINUS UBITNOT INCR DECR

%start <Ast.decl list> program

%%

program:
  | decls = list(decl); EOF { decls }

decl:
  | name = IDENT; LPAREN; params = separated_list(COMMA, param); RPAREN;
    LBRACE; body = expr; RBRACE
    { Func { name; params; body } }

param:
  | name = IDENT { { name } }

expr:
  | n = INT                         { Int n }
  | s = IDENT                       { Ident s }
  | l = expr; PLUS;  r = expr       { BinOp (Add, l, r) }
  | l = expr; MINUS; r = expr       { BinOp (Sub, l, r) }
  | l = expr; STAR;  r = expr       { BinOp (Mul, l, r) }
  | l = expr; SLASH; r = expr       { BinOp (Div, l, r) }
  | l = expr; EQ;    r = expr       { BinOp (Eq,  l, r) }
  | l = expr; NEQ;   r = expr       { BinOp (Neq, l, r) }
  | l = expr; LT;    r = expr       { BinOp (Lt,  l, r) }
  | l = expr; GT;    r = expr       { BinOp (Gt,  l, r) }
  | l = expr; LTE;   r = expr       { BinOp (Lte, l, r) }
  | l = expr; GTE;   r = expr       { BinOp (Gte, l, r) }
  | l = expr; AND;          r = expr  { BinOp (And,       l, r) }
  | l = expr; OR;           r = expr  { BinOp (Or,        l, r) }
  | l = expr; AMP;          r = expr  { BinOp (BitAnd,    l, r) }
  | l = expr; PIPE;         r = expr  { BinOp (BitOr,     l, r) }
  | l = expr; TILDE;        r = expr  { BinOp (BitXor,    l, r) }
  | l = expr; LSHIFT;       r = expr  { BinOp (Lshift,    l, r) }
  | l = expr; RSHIFT;       r = expr  { BinOp (Rshift,    l, r) }
  | l = expr; PERCENT;      r = expr  { BinOp (Mod,       l, r) }
  | l = expr; ASSIGN;       r = expr  { BinOp (Assign,    l, r) }
  | l = expr; PLUS_ASSIGN;  r = expr  { BinOp (AddAssign, l, r) }
  | l = expr; MINUS_ASSIGN; r = expr  { BinOp (SubAssign, l, r) }
  | l = expr; STAR_ASSIGN;  r = expr  { BinOp (MulAssign, l, r) }
  | l = expr; SLASH_ASSIGN; r = expr  { BinOp (DivAssign, l, r) }
  | BANG;  e = expr %prec BANG        { UnOp  (Not,    e) }
  | MINUS; e = expr %prec UMINUS      { UnOp  (Neg,    e) }
  | TILDE; e = expr %prec UBITNOT     { UnOp  (BitNot, e) }
  | INCR;  e = expr %prec INCR        { UnOp  (PreInc, e) }
  | DECR;  e = expr %prec DECR        { UnOp  (PreDec, e) }
  | LPAREN; e = expr; RPAREN          { e }
