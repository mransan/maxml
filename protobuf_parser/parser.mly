
%token REQUIRED
%token OPTIONAL
%token REPEATED

%token ONE_OF

%token MESSAGE

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

%start field_options_
%type <Ast.field_options> field_options_

%start normal_field_ 
%type <Ast.field> normal_field_

%start oneof_
%type <Ast.oneof> oneof_

%start message_
%type <Ast.message> message_
%%

field_options_ : field_options EOF {$1}  
normal_field_  : normal_field  EOF {$1}
oneof_         : oneof EOF         {$1} 
message_       : message EOF        {$1} 


/*
message = "message" messageName messageBody
messageBody = "{" { field | enum | message | extend | extensions | group |
option | oneof | mapField | reserved | emptyStatement } "}"
*/

message : 
  MESSAGE IDENT LBRACE message_body_content_list RBRACE { 
    Ast_util.message ~content:$4 $2
  } 

message_body_content_list:
  | message_body_content  { [$1] }
  | message_body_content message_body_content_list { $1::$2 }

message_body_content :
  | normal_field { Ast_util.message_body_field  $1 }
  | oneof        { Ast_util.message_body_oneof_field $1 }
  | message      { Ast_util.message_body_sub $1 }

oneof :
  ONE_OF IDENT LBRACE oneof_field_list RBRACE { 
    Ast_util.oneof ~fields:$4 $2 
  }  

oneof_field_list :
  | oneof_field                  { [$1]   }
  | oneof_field oneof_field_list { $1::$2 } 

oneof_field : 
  | IDENT IDENT EQUAL INT field_options SEMICOLON { 
    Ast_util.oneof_field ~type_:$1 ~number:$4 ~options:$5 $2  
  } 
  | IDENT IDENT EQUAL INT SEMICOLON               { 
    Ast_util.oneof_field ~type_:$1 ~number:$4 $2  
  } 

normal_field : 
  | label IDENT IDENT EQUAL INT field_options SEMICOLON { 
    Ast_util.field ~label:$1 ~type_:$2 ~number:$5 ~options:$6 $3
  } 
  | label IDENT IDENT EQUAL INT SEMICOLON { 
    Ast_util.field ~label:$1 ~type_:$2 ~number:$5 $3 
  } 

label :
  | REQUIRED { Ast.Field_label_required }  
  | REPEATED { Ast.Field_label_repeated }
  | OPTIONAL { Ast.Field_label_optional }

field_options : 
    LBRACKET field_option_list RBRACKET { $2 }; 
  | LBRACKET RBRACKET { [] }; 

field_option_list : 
  | field_option                          { [$1] } 
  | field_option COMMA field_option_list  { $1::$3 }

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
