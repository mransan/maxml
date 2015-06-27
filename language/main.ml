




let () = 

  let p s = 
    let exp = Parser.main (Lexer.lexer) (Lexing.from_string s) in 
    let sexp = Ast.simplify [] exp in  
    Printf.printf 
      "Parsed expression : %s = %s \n" 
      (Ast.string_of_exp exp)
      (Ast.string_of_exp sexp);
  in

  p "1.8";
  p "1.8 + 1.0";
  p "1.8 + 1.0 * 2";
  p "1.8 - 1.0 - 2";
  p "1.8/2 - 1.0 - 2";
  p "1.8/2 - (1.0 - 2) * 2";
  p "x";
  p "x + y";
  p "x * 2+ y";
  p "let x = 2. in let y = 1 in x * 2 + y + c ";

 

