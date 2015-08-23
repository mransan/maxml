
(** {2 Accessors for Astc.field type} *)

val field_name   : ('a, 'b)  Astc.field -> string 
(** [field_name field] returns the name [field] *)

val field_number : ('a, 'b)  Astc.field -> int
(** [field_number field] returns the number of [field] *)

val field_type   : ('a, 'b)  Astc.field -> 'a Astc.field_type
(** [field_type field] returns the type of [field] *)

val field_label  : ('a, 'b)  Astc.field -> 'b 
(** [field_label field] returns the label of [field] *)

val string_of_message : 'a Astc.message -> string 

(** {2 Accessor for Astc.type *) 

val type_id_of_type : 'a Astc.proto_type -> int 
val type_name_of_type : 'a Astc.proto_type -> string
val type_scope_of_type : 'a Astc.proto_type -> Astc.type_scope

(** {2 Creator} *) 

val empty_scope : Astc.type_scope 

val scope_of_package : string option -> Astc.type_scope

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
  Astc.type_scope -> 
  Ast.message ->
  Astc.unresolved Astc.proto

val compile_message_p2: 
  Astc.unresolved Astc.proto -> 
  Astc.unresolved Astc.message -> 
  Astc.resolved Astc.message 
(** [compile_message_p2] resolved all the fields in the given message. 
  *)  

val compile_type_p2: 
  Astc.unresolved Astc.proto -> 
  Astc.unresolved Astc.proto_type -> 
  Astc.resolved Astc.proto_type

(** {2 For testing only} *) 

val compile_oneof_p1: Ast.oneof -> Astc.unresolved Astc.oneof

val compile_field_p1: 'a Ast.field -> (Astc.unresolved, 'a ) Astc.field 

val find_all_types_in_field_scope : 
  'a Astc.proto -> 
  Astc.field_scope-> 
  'a Astc.proto

