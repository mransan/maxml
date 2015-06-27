{
(** Lex2 header section *) 
type token = 
  | Int    of string 
  | Float  of string 
  | EOF 
  | Any_char of char 

}

let digit  = ['0'-'9']
let digits = digit+ 
let float_ = (digit*) '.' (digit+)

rule tokenize = parse 
  | digits  { Int   (Lexing.lexeme lexbuf)   }
  | float_  { Float (Lexing.lexeme lexbuf)   }
  | _       { Any_char (Lexing.lexeme_char lexbuf 0) }
  | eof     { EOF }

{
(** Lex2 footer section *)  

let string_of_token   = function  
  | Int s      -> Printf.sprintf "Int(%s)" s
  | Float s    -> Printf.sprintf "Float(%s)" s
  | EOF        -> "EOF"
  | Any_char c -> Printf.sprintf "Char(%s)" (Char.escaped c) 

let is_eof = function | EOF -> true | _ -> false 

}
