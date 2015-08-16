
(** {2 Compilation errors } *)

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

let () =
  Printexc.register_printer (fun exn ->
    match exn with
    | Compilation_error (Unresolved_type { field_name; _ }) -> 
        Some (Printf.sprintf "unresolved type for field : %s" field_name)
    | Compilation_error (Duplicated_field_number _ ) -> 
        Some "duplicated field number"
    | Compilation_error (Invalid_default_value _ ) -> 
        Some "invalid default value"
    | _         -> None)

let unresolved_type ~field_name ~type_ ~message_name () = 
  (Compilation_error (Unresolved_type {
    field_name; 
    type_; 
    message_name
  }))

let duplicated_field_number ~field_name ~previous_field_name ~message_name  () = 
  (Compilation_error (Duplicated_field_number {
    field_name; 
    previous_field_name; 
    message_name;
  }))

let invalid_default_value ~field_name ~info () = 
  (Compilation_error (Invalid_default_value {field_name; info} ))

let unsupported_field_type ~field_name ~field_type ~backend_name () = 
  Compilation_error (Unsupported_field_type {
    field_name;
    field_type;
    backend_name;
  })

let programmatic_error e = Compilation_error (Programatic_error e) 
