{
(** Lex1 header section *) 
type token = 
  | Digit of string 
  | EOF 
  | Any_char of char 

}

let digit = ['0'-'9']+ 

rule tokenize = parse 
  | digit   { Digit    (Lexing.lexeme lexbuf)   }
  | _       { Any_char (Lexing.lexeme_char lexbuf 0) }
  | eof     { EOF }

{

(** Lex1 footer section *)  

let string_of_token   = function  
  | Digit s    ->  Printf.sprintf "Digit(%s)" s
  | EOF        -> "EOF"
  | Any_char c -> Printf.sprintf "Char(%s)" (Char.escaped c) 

let is_eof = function | EOF -> true | _ -> false 
}
