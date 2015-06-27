{
(** Lex2 header section *) 
type token = 
  | Word of string 
  | EOF 
  | Lexing_error  
}

let white_space  = [' ' '\t']
let white_spaces = white_space+

let end_of_line  = '\n'

let valid_char   = (_ # white_space) # end_of_line  
let word         = valid_char+

rule tokenize = parse 
  | white_spaces { tokenize lexbuf } 
  | word         { Word (Lexing.lexeme lexbuf) }
  | end_of_line  { tokenize lexbuf }  
  | _            { Lexing_error } 
  | eof          { EOF }

{
(** Lex2 footer section *)  

let string_of_token   = function  
  | Word s       -> Printf.sprintf "Word(%s)" s 
  | EOF          -> "EOF"
  | Lexing_error -> "Lexing Error"

let is_eof = function | EOF -> true | _ -> false 

}
