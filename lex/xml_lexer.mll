{
(** Lex2 header section *) 
open Xml_types
}

(** {2 Fundamental regular expression } *)
let white_space  = [' ' '\t']
let white_spaces = white_space+

let letter = ['a'-'z''A'-'Z']

let digit  = ['0'-'9']

let end_of_line  = '\n'

(** {2 Core Markup } *)

let start_tag = '<'
let end_tag   = "</"
let rab       = '>'

(** {2 Element tag } *) 

let name_char = letter | digit | ['.' '-' '_' ':']
let name      = (letter | '_' | ':') (name_char*) 


(** {2 Attributes } *) 

let attribute_value_double_quote = '"' ([^ '<' '&' '"']*) '"'
let attribute_value_single_quote = ''' ([^ '<' '&' ''']*) '''
let attribute_value = attribute_value_double_quote | attribute_value_single_quote 

let text_char = _ # start_tag  # rab 
let text      = text_char*

(** {2 Comments } *)

(** Reference for comments can be found at: 
    http://www.w3.org/TR/REC-xml/#sec-comments 
 *)
let comment_start_tag_name = "!--"
let comment_end   = "-->"

let comment_char  = (_ # '-') | ('-' (_ # '-'))
(** Note that we use this regular expresion as recommended by the standard
 * because the string '--' is illegal in a commnent. 
 *)


(** {2 CDATA} *) 

(** Reference for CDATA Section can be found at: 
    http://www.w3.org/TR/REC-xml/#sec-cdata-sect
 *)

let cdata_start_tag_name = "![CDATA["

let cdata_first_end_char = ']'
(** Note it is not really an illegal character but it is part of the definition
    of the end of the CDATA and therefore the lexer must stop
 *)

let cdata_char = _ # cdata_first_end_char

let cdata_end  = "]>"
(** the end sequence of character is ]]> but since the first ']' character is
    matched previously we only need to match the remaning part. 

    The technique used here is to simply break down the cdata string into sub
    string each delimeted by the char ']' If the following string is the
    c_data_end then we know we have reached the end of the cdata section
    otherwise it's just a regular character. 

    We could also reduce the breaking down of the CDATA section by only checking
    ']]' string using similar regular expression as for the comments. 
 *)



(** {2 Rules } *) 

rule tokenize = parse 
  | start_tag  { tokenize_tag (fun x -> Start_tag x) lexbuf }
  | end_tag    { tokenize_tag (fun x -> End_tag x) lexbuf }  
  | text       { Text (Lexing.lexeme lexbuf) } 
  | _          { Lexing_error } 
  | eof        { EOF }

and tokenize_tag f = parse 
  | name  { 
    let name = Lexing.lexeme lexbuf in 
    match tokenize_attributes (None, []) lexbuf with 
    | Attribute_error -> Lexing_error 
    | Attribute_values attributes -> f {name; attributes}
  }
  | comment_start_tag_name { 
    match tokenize_comment lexbuf with 
    | Some x -> Comment x 
    | None   -> Lexing_error 
  }   
  | cdata_start_tag_name {
    match tokenize_cdata "" lexbuf with 
    | Some x -> CData x 
    | None   -> Lexing_error 
  }
  
  | _     { Lexing_error } 
  | eof   { Lexing_error } 

and tokenize_attributes acc = parse 
  | white_spaces { tokenize_attributes acc lexbuf }
  | name         { 
    let name, attributes  = acc in 
    match name with 
    | Some x -> Attribute_error 
    | None -> 
      let name = Lexing.lexeme lexbuf in 
      tokenize_attributes (Some name, attributes) lexbuf 
  }

  | '='  {
    let name, attributes = acc in 
    match name with 
    | None -> Attribute_error 
    | Some name -> 
      match tokenize_attribute_value lexbuf with 
      | None -> Attribute_error 
      | Some value -> tokenize_attributes (None, (name, value)::attributes) lexbuf 
  }
  | rab { Attribute_values (List.rev @@ snd acc)}
  | _   | eof {Attribute_error} 

and tokenize_attribute_value = parse 
  | white_spaces { tokenize_attribute_value lexbuf }
  | attribute_value { 
    let s = Lexing.lexeme lexbuf in 
    Some (String.sub s 1 (String.length s - 2))
  }
  | _ | eof  {None}

and tokenize_comment = parse 
  | comment_char* comment_end { 
    let s = Lexing.lexeme lexbuf in 
    Some (String.sub s 0 (String.length s - 3))
  }
  | _ | eof  {None} 

and tokenize_cdata b = parse 
  | cdata_char* | cdata_first_end_char { 
    tokenize_cdata (b ^ Lexing.lexeme lexbuf)  lexbuf 
    (** TODO.. check is using Buffer would be faster *) 
  }
  | cdata_end { 
    Some (String.sub b 0 (String.length b - 1)) 
  } 
  | _ | eof {None} 

{
(** Lex2 footer section *)  
}
