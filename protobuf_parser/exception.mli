
type programmatic_error =
  | Recursive_one_of 
  | Invalid_string_split 
  | Unexpect_field_type 

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
  | Unsupported_field_type of {
    field_name: string; 
    field_type: string; 
    backend_name:string;
  } (** When a particular backend does not support the field type *)
  | Programatic_error of programmatic_error 

exception Compilation_error of error  
(** Exception raised when a compilation error occurs *)


val unresolved_type : 
  field_name:string -> 
  type_:string  ->
  message_name:string -> 
  unit -> exn

val duplicated_field_number : 
  field_name:string ->
  previous_field_name:string -> 
  message_name:string -> 
  unit -> exn 

val invalid_default_value : 
  field_name:string -> 
  info:string ->
  unit -> exn

val unsupported_field_type : 
  field_name:string ->
  field_type:string -> 
  backend_name:string ->
  unit -> exn

val programmatic_error : programmatic_error -> exn
