
type field_scope = string list 

type unresolved = {
  scope     : field_scope; 
  type_name : string; 
  from_root : bool;  (** from_root indicates that the scope for the type is
                         from the root of the type system. (ie starts with '.')
                      *) 
}

type resolved = int 

type 'a field_type = 
 | Field_type_double 
 | Field_type_float 
 | Field_type_int32 
 | Field_type_int64 
 | Field_type_uint32 
 | Field_type_uint64
 | Field_type_sint32 
 | Field_type_sint64 
 | Field_type_fixed32 
 | Field_type_fixed64 
 | Field_type_sfixed32 
 | Field_type_sfixed64
 | Field_type_bool 
 | Field_type_string 
 | Field_type_bytes 
 | Field_type_message of 'a 

type 'a field = {
  field_parsed : Ast.field; 
  field_type : 'a field_type; 
  field_default : Ast.constant option; 
}

type 'a oneof_field = {
  oneof_field_parsed : Ast.oneof_field; 
  oneof_field_type : 'a field_type; 
  oneof_field_default : Ast.constant option;
}

type 'a oneof = {
  oneof_name : string; 
  oneof_fields : 'a oneof_field list; 
}

type message_scope_item = 
  | Namespace of string 
  | Message_name of string 

type message_scope = message_scope_item list 

type 'a message_body_content = 
  | Message_field of 'a field 
  | Message_oneof_field of 'a oneof 

and 'a message = {
  id : int ; 
  message_scope : message_scope;
  message_name : string; 
  body_content : 'a message_body_content list; 
}
