

type constant = 
  | Constant_string of string 
  | Constant_bool   of bool 
  | Constant_int    of int 
  | Constant_float  of float 

type field_option  = string * constant 

type field_options = field_option list 

type field_label = [ 
  | `Optional 
  | `Required 
  | `Repeated 
]

type 'a field = {
  field_name  : string; 
  field_number: int; 
  field_label : 'a ; 
  field_type : string; 
  field_options : field_options; 
}

type oneof_label = [ `Oneof ] 

type oneof = {
  oneof_name : string; 
  oneof_fields : oneof_label field list; 
}

type message_body_content = 
  | Message_field of field_label field 
  | Message_oneof_field of oneof 
  | Message_sub of message 

and message = {
  id : int; 
  message_name : string; 
  body_content : message_body_content list; 
}
