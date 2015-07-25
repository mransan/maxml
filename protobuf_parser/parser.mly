
%token RBRACE
%token LBRACE
%token RBRACKET
%token LBRACKET 
%token RPAREN  
%token LPAREN
%token RANGLEB  
%token LANGLEB
%token EQUAL
%token SEMICOLON
%token COMMA
%token <string> STRING 
%token <int>    INT
%token <float>  FLOAT
%token <string> IDENT
%token EOF

%start default_
%type <Ast.default_value> default_

%%

default_: default EOF {$1}  

default : 
  LBRACKET IDENT EQUAL default_value RBRACKET EOF {
    if $2 <> "default"
    then failwith "invalid identifier in default clause"
    else $4
  }; 

default_value : 
  | INT        {Ast.Default_int $1 }
  | FLOAT      {Ast.Default_float $1}
  | IDENT      {match $1 with 
    | "true"  -> Ast.Default_bool true 
    | "false" -> Ast.Default_bool false 
    | _ -> failwith "invalid default value"
  }
  | STRING     {Ast.Default_string $1}; 
%%
