

let string_of_token = function 
  | Parser.RBRACE       ->  "RBRACE" 
  | Parser.LBRACE       ->  "LBRACE" 
  | Parser.RBRACKET     ->  "RBRACKET"
  | Parser.LBRACKET     ->  "LBRACKET"
  | Parser.RPAREN       ->  "RPAREN"
  | Parser.LPAREN       ->  "LPAREN"
  | Parser.RANGLEB      ->  "RANGLEB"
  | Parser.LANGLEB      ->  "LANGLEB"
  | Parser.EQUAL        ->  "EQUAL"
  | Parser.SEMICOLON    ->  "SEMICOLON"
  | Parser.COMMA        ->  "COMMA"
  | Parser.STRING s     -> Printf.sprintf "string(%s)" s   
  | Parser.INT i        -> Printf.sprintf "int(%i)" i
  | Parser.FLOAT f      -> Printf.sprintf "float(%f)" f
  | Parser.IDENT s      -> Printf.sprintf "ident(%s)" s        
  | Parser.EOF          -> "eof" 

open Printf 

let () = 
  let proto = "
message SearchRequest {
  required string query = 1;
  optional int32 page_number = 2 [default = 1]; // Which page number do we want?
  optional int32 result_per_page = 3; // Number of results to return per page.
  map < string, Project > projects = 3 ;
  optional double time = 5 [default = 1.6]; // How long it took
} " in 

  let rec loop lexbuf = 
    match Lexer.lexer lexbuf with 
    | Parser.EOF -> () 
    | _ as t -> 
      print_endline @@ string_of_token t; 
      loop lexbuf
  in  

  (*
  loop @@ Lexing.from_string proto;  
  loop @@ Lexing.from_channel @@ open_in "unittest.proto";
  *)
  (* 
   [default =  41    ] 
   [default = -50    ]
   [default = -0.45  ]
   [default = true   ]
   [default = "hello"]
  *)

  let test_default s = 
    Printf.printf "---- %s ----\n" s;
    loop @@ Lexing.from_string s; 
    List.assoc "default" @@ Parser.default_ Lexer.lexer (Lexing.from_string s) in 
  
  assert (Ast.Constant_int 1    = test_default "[default = 1 ]"); 
  assert (Ast.Constant_int (-1) = test_default "[default = -1]"); 
  assert (Ast.Constant_string "hello" = test_default "[default = \"hello\"]"); 
  assert (Ast.Constant_string "he'l'lo" = test_default "[default = \"he'l'lo\"]"); 
  assert (Ast.Constant_string "he\"l\"lo" = test_default "[default = \"he\\\"l\\\"lo\"]"); 
  assert (Ast.Constant_float 1.23 = test_default "[default = 1.23]"); 
  assert (Ast.Constant_float (-. 1.23) = test_default "[default = -1.23]"); 
  assert (Ast.Constant_bool true  = test_default "[default = true]"); 
  assert (Ast.Constant_bool false = test_default "[default = false]"); 
  ()
