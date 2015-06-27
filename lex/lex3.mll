{
(** Lex2 header section *) 
type token = 
  | White_space of char 
  | Valid_char  of char 
  | EOF 
  | Lexing_error  

}

let white_space = [' ' '\t']
let valid_char  = _ # white_space 

rule tokenize = parse 
  | white_space { White_space  (Lexing.lexeme_char lexbuf 0)   }
  | valid_char  { Valid_char   (Lexing.lexeme_char lexbuf 0)   }
  | _           { Lexing_error } 
  | eof         { EOF }

{
(** Lex2 footer section *)  

let string_of_token   = function  
  | White_space c  -> Printf.sprintf "White_space(%s)" (Char.escaped c)
  | Valid_char  c  -> Printf.sprintf "Valid_char(%s)" (Char.escaped c)
  | EOF            -> "EOF"
  | Lexing_error   -> "Lexing Error"

let is_eof = function | EOF -> true | _ -> false 
}
