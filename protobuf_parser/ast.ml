
(** This module defines the Raw AST for the Protobuffer specification
    
    This is what is outputed by the parser.
 *)

(** field constant *) 
type constant = 
  | Constant_string of string 
  | Constant_bool   of bool 
  | Constant_int    of int 
  | Constant_float  of float 

(** field can have a list of options attached to 
    them, for example the most widely used is [default]:

    [required int32 my_field = [default=1]]
  *) 
type field_option  = string * constant 

type field_options = field_option list 

(** A field property defining its occurence
 *)
type field_label = [ 
  | `Optional 
  | `Required 
  | `Repeated 
]

(** message field. 
    
   Note this field is parametrized with the label type 
   so that it can be used both by normal field and one of 
   field since the only difference between the 2 is 
   the label.
 *)
type 'a field = {
  field_name  : string; 
  field_number: int; 
  field_label : 'a ; 
  field_type : string; 
  field_options : field_options; 
}

type oneof_label = [ `Oneof ] 

(** oneof entity *)
type oneof = {
  oneof_name : string; 
  oneof_fields : oneof_label field list; 
}

(** Body content defines all the possible consituant 
    of a message. 
  *)
type message_body_content = 
  | Message_field of field_label field 
  | Message_oneof_field of oneof 
  | Message_sub of message 

(** Message entity. 
 
    Note the ID is simply for uniquely (and easily) identifying a type. It is
    expected to be generated by a parser. The later compilation 
    functions expects this id to be unique.
  *)
and message = {
  id : int; 
  message_name : string; 
  message_body : message_body_content list; 
}

(** Definition of a protobuffer message file. 
 *)
type proto = {
  package : string option; 
  messages : message list; 
}
