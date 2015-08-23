(** This module defines convenient function to create and manipulate
    the types define in the Ast module.
  *)

(** {2 Creators } *) 

val field : 
  ?options:Ast.field_options ->
  label:Ast.field_label-> 
  number:int -> 
  type_:string -> 
  string -> 
  Ast.field_label Ast.field

val oneof_field : 
  ?options:Ast.field_options ->
  number:int -> 
  type_:string -> 
  string -> 
  Ast.oneof_label Ast.field

val oneof :
  fields:Ast.oneof_label Ast.field list -> 
  string -> 
  Ast.oneof 

val message_body_field : 
  Ast.field_label Ast.field  -> 
  Ast.message_body_content  

val message_body_oneof_field  : 
  Ast.oneof -> 
  Ast.message_body_content 

val enum_value :
  int_value:int -> 
  string -> 
  Ast.enum_value 

val enum : 
  ?enum_values:Ast.enum_value list -> 
  string -> 
  Ast.enum 

val message_body_sub : 
  Ast.message -> 
  Ast.message_body_content

val message_body_enum: 
  Ast.enum -> 
  Ast.message_body_content

val message : 
  content:Ast.message_body_content list -> 
  string -> 
  Ast.message

val proto : ?package:string -> Ast.message list -> Ast.proto

(** {2 Miscellaneous functionality } *)

val message_printer :?level:int -> Ast.message -> unit 

