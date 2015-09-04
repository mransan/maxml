
(** {2 Accessors for Pbtt.field type} *)

val field_name   : ('a, 'b)  Pbtt.field -> string 
(** [field_name field] returns the name [field] *)

val field_number : ('a, 'b)  Pbtt.field -> int
(** [field_number field] returns the number of [field] *)

val field_type   : ('a, 'b)  Pbtt.field -> 'a Pbtt.field_type
(** [field_type field] returns the type of [field] *)

val field_label  : ('a, 'b)  Pbtt.field -> 'b 
(** [field_label field] returns the label of [field] *)

val type_of_id : 'a Pbtt.proto -> int -> 'a Pbtt.proto_type 
(** [type_of_id all_types id] returns the type associated with the given id, 
    @raise [Not_found] if the type is not in the all_types. 
  *)

val string_of_message : 'a Pbtt.message -> string 

(** {2 Accessor for Pbtt.type *) 

val type_id_of_type : 'a Pbtt.proto_type -> int 
val type_name_of_type : 'a Pbtt.proto_type -> string
val type_scope_of_type : 'a Pbtt.proto_type -> Pbtt.type_scope

(** {2 Creator} *) 

val empty_scope : Pbtt.type_scope 

val scope_of_package : string option -> Pbtt.type_scope

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
  Pbtt.type_scope -> 
  Pbpt.message ->
  Pbtt.unresolved Pbtt.proto

val compile_message_p2: 
  Pbtt.unresolved Pbtt.proto -> 
  Pbtt.unresolved Pbtt.message -> 
  Pbtt.resolved Pbtt.message 
(** [compile_message_p2] resolved all the fields in the given message. 
  *)  

val compile_type_p2: 
  Pbtt.unresolved Pbtt.proto -> 
  Pbtt.unresolved Pbtt.proto_type -> 
  Pbtt.resolved Pbtt.proto_type

val group: Pbtt.resolved Pbtt.proto -> Pbtt.resolved Pbtt.proto list 

(** {2 For testing only} *) 

val compile_oneof_p1: Pbpt.oneof -> Pbtt.unresolved Pbtt.oneof

val compile_field_p1: 'a Pbpt.field -> (Pbtt.unresolved, 'a ) Pbtt.field 

val find_all_types_in_field_scope : 
  'a Pbtt.proto -> 
  Pbtt.field_scope-> 
  'a Pbtt.proto
