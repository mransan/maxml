
type attribute = string * string 

val string_of_attributes : attribute list -> string 

type tag = { 
  name : string; 
  attributes : attribute list 
} 

val string_of_tag : tag -> string 

type token = 
  | EOF 
  | Start_tag of tag
  | End_tag   of tag
  | Text      of string 
  | Comment   of string 
  | CData     of string 
  | Lexing_error 

val string_of_token : token -> string 

val is_eof : token -> bool

type attribute_output = 
  | Attribute_error 
  | Attribute_values of attribute list 
