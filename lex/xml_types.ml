
type attribute = string * string 

type tag = { 
  name : string; 
  attributes : attribute list 
} 

type token = 
  | EOF 
  | Start_tag of tag
  | End_tag   of tag
  | Text      of string 
  | Comment   of string 
  | CData     of string 
  | Lexing_error 

type attribute_output = 
  | Attribute_error 
  | Attribute_values of attribute list 

let string_of_attributes attributes = 
  List.fold_left (fun s (n,v) -> Printf.sprintf "%s (%s,%s), " s n v) "" attributes

let string_of_tag {name; attributes} = 
  Printf.sprintf "name:%s, attributes: [%s]" name (string_of_attributes
  attributes) 

let string_of_token = function  
  | EOF -> "EOF"
  | Start_tag tag -> Printf.sprintf "Start_tag(%s)" (string_of_tag tag)
  | End_tag   tag -> Printf.sprintf "End_tag(%s)"   (string_of_tag tag) 
  | Text s -> Printf.sprintf "Text(%s)" s
  | Comment s -> Printf.sprintf "Comment(%s)" s
  | CData s -> Printf.sprintf "CData(%s)" s
  | Lexing_error -> "Lexing_error"  

let is_eof = function | EOF -> true | _ -> false 
