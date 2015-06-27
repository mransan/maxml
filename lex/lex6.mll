{
(** Lex2 header section *) 

(*
&lt; represents \\"<\\"
&gt; represents \\">\\"
&amp; represents \\"&\\"
&apos; represents \\'
&quot; represents Quote character
*)

(*
&#20013; or &#x4e2d;
*)

type token = 
 | More 
 | End 

}

let ampersand = '&'
let semicolon = ';'
let text_char = (_ # ampersand)
let text = text_char*

let digit  = ['0'-'9'] 
let digits = digit+ 

let hex_char = ['0'-'9''a'-'f''A'-'F']
let hex_chars = hex_char+

let pound = '#' 

let decimal    = digits ';'
let hexdecimal = 'x' hex_chars ';'

rule tokenize b string_of_char_int = parse 
  | text      { Buffer.add_string b (Lexing.lexeme lexbuf); More } 
  | ampersand { 
    tokenise_special_char b string_of_char_int lexbuf; 
    More
  } 
  | _         { failwith "Lexing Error"} 
  | eof       { End }
and tokenise_special_char b string_of_char_int = parse 
  | "lt;"   { Buffer.add_char b '<' }
  | "gt;"   { Buffer.add_char b '>' }
  | "amp;"  { Buffer.add_char b '&' }
  | "apos;" { Buffer.add_char b '\'' }
  | "quot;" { Buffer.add_char b '\"' }
  | pound   { tokenise_number b string_of_char_int lexbuf } 
  | _ 
  | eof     { () } 

and tokenise_number b string_of_char_int = parse 
  | hexdecimal { 
    let b' = Buffer.create 32 in 
    Buffer.add_char b' '0'; 
    let s = Lexing.lexeme lexbuf in 
    Buffer.add_substring b' s 0 (String.length s -1); 
    let char_int = int_of_string (Buffer.contents b') in 
    Buffer.add_string b (string_of_char_int char_int); 
  }  
  | decimal  { 
    let s = Lexing.lexeme lexbuf in 
    let char_int = int_of_string (String.sub s 0 (String.length s - 1)) in 
    Buffer.add_string b (string_of_char_int char_int)
  }  
  | _
  | eof  { failwith "Lexing Error" }


{


}
