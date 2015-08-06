
(** {2 Accessors for Astc.field type} *)

val field_name   : ('a, 'b)  Astc.field -> string 
(** [field_name field] returns the name [field] *)

val field_number : ('a, 'b)  Astc.field -> int
(** [field_number field] returns the number of [field] *)

val field_type   : ('a, 'b)  Astc.field -> 'a Astc.field_type
(** [field_type field] returns the type of [field] *)

val field_label  : ('a, 'b)  Astc.field -> 'b 
(** [field_label field] returns the label of [field] *)

(** {2 Compilation errors } *)

type error = 
  | Unresolved_type of {
    field_name: string; 
    type_:string; 
    message_name:string 
  } (** When the type of a field could not be resolved *) 
  | Duplicated_field_number of {
    field_name: string; 
    previous_field_name  : string;
    message_name: string; 
  } (** When there are 2 field with either identical number or name *)
  | Invalid_default_value of {
    field_name: string; 
    info: string; 
  } (** When a default value type type does not match the field type *)

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)

(** {2 Compilation routines} *) 

(** Compilation is done in 2 phases. 
    {ul 
    {- Phase 1 focuses on flattenning the nested messages and doing a first round
    of type checking for the various message field. The field type will be
    either matched with a basic type or parsed into the [unresolved] data
    structure. This step simply verify that the type definition is well formed
    but does not check the field type is pointing to an existing type. }

    {- Phase 2 focuses on type resolution. This phase implement the scoping
    rules defined in the protocol buffer specification to resolve a field type 
    to a previously defined message. This phase additionally check that field
    numbers and field names are unique within a message but this logic should be
    moved to Phase 1}
    }
 *)

val compile_message_p1 : 
  Astc.message_scope -> 
  Ast.message ->
  Astc.unresolved Astc.message list  

val compile_message_p2: 
  Astc.unresolved Astc.message list -> 
  Astc.unresolved Astc.message -> 
  Astc.resolved Astc.message 
(** [compile_message_p2] resolved all the fields in the given message. 
  *)  

(** {2 For testing only} *) 

val compile_oneof_p1: Ast.oneof -> Astc.unresolved Astc.oneof

val compile_field_p1: 'a Ast.field -> (Astc.unresolved, 'a ) Astc.field 

val find_all_message_in_field_scope : 
  'a Astc.message list -> 
  Astc.field_scope-> 
  'a Astc.message list  

