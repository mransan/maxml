{
(** Lex2 header section *) 
type token = 
  | Text of string 
  | Hexadecimal of int 
  | Decimal of int 
  | Less_than
  | Greater_than
  | Ampercent
  | Single_quote
  | Double_quote
  | EOF 

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

rule tokenize = parse 
  | text { Text (Lexing.lexeme lexbuf) }
  | ampersand { 
    tokenise_special_char lexbuf
  } 
  | _         { failwith "Lexing Error"} 
  | eof       { EOF }
and tokenise_special_char = parse 
  | "lt;"   { Less_than }
  | "gt;"   { Greater_than }
  | "amp;"  { Ampercent }
  | "apos;" { Single_quote }
  | "quot;" { Double_quote }
  | pound   { tokenise_number lexbuf } 
  | _ 
  | eof     { failwith "Lexing Error" }

and tokenise_number = parse 
  | hexdecimal { 
    let b = Buffer.create 32 in 
    Buffer.add_char b '0'; 
    let s = Lexing.lexeme lexbuf in 
    Buffer.add_substring b s 0 (String.length s -1); 
    Hexadecimal (int_of_string (Buffer.contents b))
  }  
  | decimal  { 
    let s = Lexing.lexeme lexbuf in 
    Decimal (int_of_string (String.sub s 0 (String.length s - 1))) 
  }  
  | _
  | eof  { failwith "Lexing Error" }


{

(** Lex2 footer section *)  
let string_of_token t = 
  let sp = Printf.sprintf in 
  match t with 
  | Text s         -> sp "Text(%s)" s 
  | Hexadecimal i  -> sp "Hexadecimal(%i)" i 
  | Decimal i      -> sp "Decimal(%i)" i 
  | Less_than      -> "(<)"
  | Greater_than   -> "(>)"
  | Ampercent      -> "(&)"
  | Single_quote   -> "(')"
  | Double_quote   -> "(\")"
  | EOF            -> "(EOF)" 

let is_eof = function | EOF -> true | _ -> false 

}
