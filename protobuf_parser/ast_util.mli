



val field : 
  ?options:Ast.field_options ->
  label:Ast.field_label-> 
  number:int -> 
  type_:string -> 
  string -> 
  Ast.field

val oneof_field : 
  ?options:Ast.field_options ->
  number:int -> 
  type_:string -> 
  string -> 
  Ast.oneof_field

val oneof :
  fields:Ast.oneof_field list -> 
  string -> 
  Ast.oneof 

val message_body_field : Ast.field  -> Ast.message_body_content  
val message_body_oneof_field  : Ast.oneof -> Ast.message_body_content 
val message_body_sub : Ast.message -> Ast.message_body_content

val message : 
  content:Ast.message_body_content list -> 
  string -> 
  Ast.message
