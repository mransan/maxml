
%token EOF
%token <float> Lfloat
%token Lplus
%token Lminus
%token Ltimes
%token Ldiv
%token Lleftp
%token Lrightp
%token <string> Lvar
%token Llet
%token Lin
%token Leq
%start main  
%type <Ast.exp> main 

%left Lin 
%left Lplus  Lminus
%left Ltimes Ldiv

%%

main:
  exp EOF          { $1 } 

exp :
  | Lfloat                    { Ast.Float $1 }
  | Lvar                      { Ast.Var   $1 }
  | Llet Lvar Leq exp Lin exp { Ast.Let ($2, $4, $6) } 
  | Lleftp exp Lrightp        { $2 }
  | exp Ltimes exp            { Ast.Times ($1, $3) }
  | exp Ldiv   exp            { Ast.Div   ($1, $3) }
  | exp Lplus  exp            { Ast.Plus  ($1, $3) }
  | exp Lminus exp            { Ast.Minus ($1, $3) }
;
%%
