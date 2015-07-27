
val field_name   : Astc.field -> string 

val field_number : Astc.field -> int

val compile_field_p1: Ast.field -> Astc.field 

val compile_oneof_field_p1: Ast.oneof_field -> Astc.oneof_field

type error = 
  | Unresolved_type of {
    field_name: string; 
    type_:string; 
    message_name:string 
  }

exception Compilation_error of error  

val compile_oneof_p1: Ast.oneof -> Astc.oneof

val compile_message_p1 : Astc.message_scope -> Ast.message -> Astc.message list -> Astc.message list  

val find_all_message_in_field_scope : Astc.message list -> Astc.field_scope-> Astc.message list  

val compile_message_p2: 
  Astc.message list -> 
  Astc.message -> 
  unit 
(** [compile_message_p2] resolved all the fields in the given message. 
  *)  
