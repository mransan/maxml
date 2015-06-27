
let float_regex = ['0'-'9']+ ('.' ['0'-'9']*)? 
let var_regex   = ['a'-'z']+ 
rule lexer = parse
  | [ ' ' ]         { lexer lexbuf  }
  | [ '*' ]         { Parser.Ltimes }
  | [ '/' ]         { Parser.Ldiv   }
  | [ '+' ]         { Parser.Lplus  }
  | [ '-' ]         { Parser.Lminus }
  | [ '(' ]         { Parser.Lleftp }
  | [ ')' ]         { Parser.Lrightp }
  | "let"           { Parser.Llet }
  | "in"            { Parser.Lin } 
  | [ '=' ]         { Parser.Leq }  
  | float_regex     { Parser.Lfloat (float_of_string (Lexing.lexeme lexbuf)) } 
  | var_regex       { Parser.Lvar   (Lexing.lexeme lexbuf) } 
  | [ '\n' ]        { Parser.EOF }
  | eof             { Parser.EOF }

