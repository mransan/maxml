
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
%type <Ast.field_options> default_

%%

default_: default EOF {$1}  

default : 
    LBRACKET field_options RBRACKET { $2 }; 
  | LBRACKET RBRACKET { [] }; 

field_options : 
  | field_option                     { [$1] } 
  | field_option COMMA field_options { $1::$3 }

field_option :
  IDENT EQUAL constant { ($1, $3) } 

constant : 
  | INT        { Ast.Constant_int $1 }
  | FLOAT      { Ast.Constant_float $1 }
  | IDENT      { match $1 with 
    | "true"  -> Ast.Constant_bool true 
    | "false" -> Ast.Constant_bool false 
    | _ -> failwith "invalid default value"
  }
  | STRING     { Ast.Constant_string $1 }; 
%%
