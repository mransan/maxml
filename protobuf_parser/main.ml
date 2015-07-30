

let string_of_token = function 
  | Parser.REQUIRED     ->  "REQUIRED"
  | Parser.OPTIONAL     ->  "OPTIONAL" 
  | Parser.REPEATED     ->  "REPEATED" 
  | Parser.ONE_OF       ->  "ONE_OF"
  | Parser.MESSAGE      ->  "MESSAGE"
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

  let parse f s  = 
    f Lexer.lexer (Lexing.from_string s)
  in 

  let test_default s = 
    (*
    Printf.printf "---- %s ----\n" s;
    loop @@ Lexing.from_string s; 
     *)
    List.assoc "default" @@ parse Parser.field_options_ s  
  in 
  
  assert (Ast.Constant_int 1    = test_default "[default = 1 ]"); 
  assert (Ast.Constant_int (-1) = test_default "[default = -1]"); 
  assert (Ast.Constant_string "hello" = test_default "[default = \"hello\"]"); 
  assert (Ast.Constant_string "he'l'lo" = test_default "[default = \"he'l'lo\"]"); 
  assert (Ast.Constant_string "he\"l\"lo" = test_default "[default = \"he\\\"l\\\"lo\"]"); 
  assert (Ast.Constant_float 1.23 = test_default "[default = 1.23]"); 
  assert (Ast.Constant_float (-. 1.23) = test_default "[default = -1.23]"); 
  assert (Ast.Constant_bool true  = test_default "[default = true]"); 
  assert (Ast.Constant_bool false = test_default "[default = false]"); 

  let () = 
    let field_options = parse Parser.field_options_ "[]" in 
    assert (field_options = [])
  in 
  let () = 
    let field_options = parse Parser.field_options_ "[a=1,b=true]" in 
    assert (List.length field_options = 2)
  in 

  let () =
    let {
      Ast.field_name; 
      field_type; 
      field_label; 
      field_options; 
      field_number
    } = parse Parser.normal_field_ "optional int32 x = 1 [default=1];" in 
    assert (field_name = "x"); 
    assert (field_label = `Optional); 
    assert (field_type = "int32"); 
    assert (field_number = 1); 
    assert (List.length field_options = 1)
  in
  let () =
    let {
      Ast.field_name; 
      field_type; 
      field_label; 
      field_options; 
      field_number
    } = parse Parser.normal_field_ "optional .M1 x = 1;" in 
    assert (field_name = "x"); 
    assert (field_label = `Optional); 
    assert (field_type = ".M1"); 
    assert (field_number = 1); 
    assert (List.length field_options = 0)
  in

  let () = 
    let s = "
      oneof foo {
        string name = 4;
        SubMessage sub_message = 9 [a=1];
      }"
    in 
    let {
      Ast.oneof_name; 
      Ast.oneof_fields; 
    } = parse Parser.oneof_ s in
    assert (oneof_name = "foo"); 
    assert (List.length oneof_fields = 2);
    let {
      Ast.field_name; 
      Ast.field_number; 
      Ast.field_type; 
      Ast.field_options; 
    } = List.nth oneof_fields 0 in 
    assert (field_name = "name"); 
    assert (field_type = "string"); 
    assert (field_number= 4); 
    assert (List.length field_options = 0); 
    let {
      Ast.field_name; 
      Ast.field_number; 
      Ast.field_type; 
      Ast.field_options; 
    } = List.nth oneof_fields 1 in 
    assert (field_name = "sub_message"); 
    assert (field_type = "SubMessage"); 
    assert (field_number= 9); 
    assert (List.length field_options = 1); 
    ()
  in
  let () = 
    let s = "
    message Outer {
      required int64 ival = 1;
      required string sval = 2;
      
      message Inner { 
        required int64 inner_ival = 1;
        required string inner_sval = 2;
      }
      
      required Inner inner = 3; 
    }"
    in 
    (*
    Printf.printf "---- MESSAGE ----\n";
    *)
    let {
      Ast.message_name; 
      Ast.body_content;
    } = parse Parser.message_ s in 
    assert (message_name  = "Outer");
    assert (List.length body_content = 4);
    ()
  in
  let () = 
    let s = "
    message Test {
      required int64 ival  = 1;
      required string sval = 2;
    }"
    in 
    let ast  = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in  
    assert (List.length all_messages = 1);
    let {
      Astc.message_scope; 
      Astc.message_name;
      Astc.body_content; 
    } = List.hd all_messages in 
    assert ([] = message_scope);
    assert ("Test" = message_name); 
    assert (2 = List.length body_content); 
    

    let test_fields body_content = 
      let f1 = List.nth body_content 0 in 
      let f1 = match f1 with 
        | Astc.Message_field f -> f
        | _ -> assert(false)
      in 
      assert (Astc_util.field_name f1 = "ival"); 
      assert (f1.Astc.field_type  = Astc.Field_type_int64);
      assert (Astc_util.field_number f1 = 1); 
      assert (None = f1.Astc.field_default); 
      
      let f2 = List.nth body_content 1 in 
      let f2 = match f2 with 
        | Astc.Message_field f -> f
        | _ -> assert(false)
      in 
      assert (Astc_util.field_name f2 = "sval"); 
      assert (f2.Astc.field_type  = Astc.Field_type_string);
      assert (Astc_util.field_number f2 = 2); 
      assert (None = f2.Astc.field_default); 
      ()
    in 
    test_fields body_content; 
    let s = "
    message Test {
      message Inner {
        required int64 ival  = 1;
        required string sval = 2;
      }
    }"
    in 
    let ast  = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in  
    assert (List.length all_messages = 2);
    let {
      Astc.message_scope; 
      Astc.message_name;
      Astc.body_content; 
    } = List.hd all_messages in 
    assert ([] = message_scope);
    assert ("Test" = message_name); 
    assert (0 = List.length body_content); 
    let {
      Astc.message_scope; 
      Astc.message_name;
      Astc.body_content; 
    } = List.nth all_messages 1 in 
    assert (1 = List.length message_scope);
    assert ("Inner" = message_name); 
    assert (2 = List.length body_content); 
    test_fields body_content; 
    let expected_scope = [ Astc.Message_name "Test" ] in 
    assert(expected_scope = message_scope);
    ()
  in
  let () = 
    let s = "
    message Test {
      required Msg1.Msg2.SubMessage mval  = 1;
    }"
    in 
    let ast  = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in  
    assert (List.length all_messages = 1);
    let {
      Astc.message_scope; 
      Astc.message_name;
      Astc.body_content; 
    } = List.hd all_messages in 
    assert ([] = message_scope);
    assert ("Test" = message_name); 
    assert (1 = List.length body_content); 
    let f1 = List.nth body_content 0 in 
    let f1 = match f1 with | Astc.Message_field f -> f | _ -> assert(false) in 
    assert ("mval" = Astc_util.field_name f1); 
    assert (1 = Astc_util.field_number f1); 
    let unresolved = {
      Astc.scope     = ["Msg1";"Msg2"];
      Astc.type_name = "SubMessage";
      Astc.from_root = false;
    } in 
    assert ((Astc.Field_type_message unresolved) = f1.Astc.field_type); 
    ()
  in 

  let () = 
    let s = "
    message M1 { 
      message M2 { message M21 { } } 
      message M3 { message M31 { message M311 { } } } 
    }
    " in 
    let ast = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in 
    assert (6 = List.length all_messages); 
    let filtered = Astc_util.find_all_message_in_field_scope all_messages [] in 
    assert (1 = List.length filtered);
    let filtered = Astc_util.find_all_message_in_field_scope all_messages ["M1"] in 
    assert (2 = List.length filtered);
    let filtered = Astc_util.find_all_message_in_field_scope all_messages ["M1";"M2"] in 
    assert (1 = List.length filtered);
    let filtered = Astc_util.find_all_message_in_field_scope all_messages ["M1";"M3"] in 
    assert (1 = List.length filtered);
    let filtered = Astc_util.find_all_message_in_field_scope all_messages ["M1";"M3";"M31"] in 
    assert (1 = List.length filtered);
    ()
  in 
  let () = 
    let s = "
    message M1 {
      message M2 {
        message M21 {
        }
      }
      message M3 { 
        required M1.M2      x1 = 1; 
        required M1.M2.M21  x2 = 2; 
        required M1.M3      x3 = 3; 
        required M1         x4 = 4; 
        required M3         x5 = 5; 
        required M2         x6 = 6; 
        required M2.M21     x7 = 7; 
        oneof x {
          M1.M2      x1  = 11; 
          M1.M2.M21  x2  = 12; 
          M1.M3      x3  = 13; 
          M1         x4  = 14; 
          M3         x5  = 15; 
          M2         x6  = 16; 
          M2.M21     x7  = 17; 
          .M1        x8  = 18; 
          .M1.M2     x9  = 19; 
          .M1.M2.M21 x10 = 110; 
        }
      }
    }
    " in 
    let ast = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in 
    ignore @@ List.map (fun m -> Astc_util.compile_message_p2 all_messages m) all_messages; 
    ()
  in 

  let assert_unresolved f = 
    match ignore @@ f () with 
    | exception (Astc_util.Compilation_error (Astc_util.Unresolved_type _) )-> () 
    | _ -> assert(false)
  in

  let test_unresolved_msg s = 
    let ast = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in 
    assert_unresolved (fun () -> 
      ignore @@ List.map (fun m -> Astc_util.compile_message_p2 all_messages m) all_messages
    )
  in

  let () = 
    let s = "
    message M1 {
      required Dont.Exist x1 = 1; 
    }
    " in 
    test_unresolved_msg s;
    ()
  in 
  let () = 
    let s = "
    message M1 {
      required M1.M2 x1 = 1; 
    }
    " in 
    test_unresolved_msg s; 
    ()
  in 
  let () = 
    let s = "
    message M1 {
      message M2 {} 
      required M1.M3 x1 = 1; 
    }
    " in 
    test_unresolved_msg s; 
    ()
  in 
  let () = 
    let s = "
    message M1 {
      message M2 {} 
      required .M2 x1 = 1; 
    }
    " in 
    test_unresolved_msg s; 
    ()
  in 

  let assert_duplicate f = 
    match ignore @@ f () with 
    | exception (Astc_util.Compilation_error (Astc_util.Duplicated_field_number _) )-> () 
    | _ -> assert(false)
  in

  let test_duplicate s = 
    let ast = parse Parser.message_ s in 
    let all_messages = Astc_util.compile_message_p1 [] ast in 
    assert_duplicate (fun () -> 
      ignore @@ List.map (fun m -> Astc_util.compile_message_p2 all_messages m) all_messages
    )
  in
  let () = 
    let s = "
    message M1 {
      required uint32 x = 1; 
      required uint32 y = 1; 
    }
    " in 
    test_duplicate s; 
    ()
  in 
  let () = 
    let s = "
    message M1 {
      required uint32 x = 100;
      required uint32 y = 1; 
      required uint32 z = 100; 
    }
    " in 
    test_duplicate s; 
    ()
  in 
  let () = 
    let s = "
    message M1 {
      required uint32 x = 1;
      oneof o {
        uint32 y = 1;
      }
    }
    " in 
    test_duplicate s; 
    ()
  in 
  let () = 
    let s = "
    message M1 {
      oneof o {
        uint32 y = 1;
      }
      required uint32 x = 1;
    }
    " in 
    test_duplicate s; 
    ()
  in 
  let () = 
    let s = "
    message M1 {
      oneof o {
        uint32 x = 1;
      }
      oneof o {
        uint32 y = 1;
      }
    }
    " in 
    test_duplicate s; 
    ()
  in 
  print_endline "\n--- Good ---";
  ()
