%{
open Ast
%}

%token <int>    INT
%token <string> IDENT
%token PLUS MINUS STAR SLASH
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COMMA
%token EOF

%left PLUS MINUS
%left STAR SLASH

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
  | n = INT                        { Int n }
  | s = IDENT                      { Ident s }
  | l = expr; PLUS;  r = expr      { BinOp (Add, l, r) }
  | l = expr; MINUS; r = expr      { BinOp (Sub, l, r) }
  | l = expr; STAR;  r = expr      { BinOp (Mul, l, r) }
  | l = expr; SLASH; r = expr      { BinOp (Div, l, r) }
  | LPAREN; e = expr; RPAREN       { e }
