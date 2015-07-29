
val field_name   : 'a Astc.field -> string 

val field_number : 'a Astc.field -> int

val compile_field_p1: Ast.field -> Astc.unresolved Astc.field 

val compile_oneof_field_p1: Ast.oneof_field -> Astc.unresolved Astc.oneof_field

type error = 
  | Unresolved_type of {
    field_name: string; 
    type_:string; 
    message_name:string 
  }

exception Compilation_error of error  

val compile_oneof_p1: Ast.oneof -> Astc.unresolved Astc.oneof

val compile_message_p1 : 
  Astc.message_scope -> 
  Ast.message ->
  Astc.unresolved Astc.message list -> 
  Astc.unresolved Astc.message list  

val find_all_message_in_field_scope : 
  'a Astc.message list -> 
  Astc.field_scope-> 
  'a Astc.message list  

val compile_message_p2: 
  Astc.unresolved Astc.message list -> 
  Astc.unresolved Astc.message -> 
  Astc.resolved Astc.message 
(** [compile_message_p2] resolved all the fields in the given message. 
  *)  
