
let print_banner s = 
  print_endline "-----------------------------"; 
  Printf.printf "--- %15s       ---\n" s ; 
  print_endline "-----------------------------" 

let fold_i f e0 (b, e) = 
  let inc = if b>e then (-) else (+) in 
  let rec loop acc = function 
    | i when i = e -> f acc i 
    | i -> loop (f acc i) (inc i 1) 
  in 
  loop e0 b 
(** [fold_i (fun sum i -> sum + i) 0 (0 20)] iterate from 
    b to e while accumulating in a fold_left fashion
 *)

module type Tokenizer_sig =  sig 
  type token 

  val is_eof : token -> bool
  val tokenize : Lexing.lexbuf -> token 
  val string_of_token : token -> string 
end 

module Tester = functor (T:Tokenizer_sig) -> struct 
  let rec loop counter buffer = 
     let token  = T.tokenize buffer in 
     Printf.printf "[%2d] %s \n" counter (T.string_of_token token); 
     if T.is_eof token 
     then () 
     else loop (counter + 1) buffer 

  let tokenize_string s = loop 0 (Lexing.from_string s) 
  
  let print s = 
    let open Printf in 
    printf "-- [%s] -- \n" s; 
    tokenize_string s
end

module Test1 = Tester(Lex1)
module Test2 = Tester(Lex2)
module Test3 = Tester(Lex3)
module Test4 = Tester(Lex4)
module Test5 = Tester(Lex5)


module Testable_xml = struct 
  
  type token = Xml_types.token 

  let string_of_token = Xml_types.string_of_token
  let is_eof = Xml_types.is_eof
  
  let tokenize = Xml_lexer.tokenize 
end


module XmlTest = Tester(Testable_xml)

let () = 

  (*
  print_banner "Lex1"; 
  Test1.print "a123b";
  Test1.print "\n";
  Test1.print "\\n";
  
  print_banner "Lex2"; 
  Test2.print "a123b";
  Test2.print "a123.0b";
  
  print_banner "Lex3"; 
  Test3.print "\t\t\t   1 2 2   ";
  
  print_banner "Lex4"; 
  Test4.print "\t\tMy  \t Name  is Max ";
  Test4.print "\t\tMy  \n";

  print_banner "XML Lexer";
  XmlTest.print "<abc>";
  XmlTest.print "<abc><def>";
  XmlTest.print "<>";
  XmlTest.print "</abc></def>";
  XmlTest.print "<abc><def></def></abc>";
  XmlTest.print "<a:b:c>";
  XmlTest.print "<1abc>";
  XmlTest.print "<a>my super text</a>";

  let attribute_value_of_string s = 
    let lexbuf = Lexing.from_string s in 
    match Xml_lexer.tokenize_attribute_value lexbuf with 
    | None -> "No attribute"
    | Some x -> x 
  in 
  Printf.printf "attribute: %s\n" (attribute_value_of_string "'Y'");
  Printf.printf "attribute: %s\n" (attribute_value_of_string "\"Max\"");
  Printf.printf "attribute: %s\n" (attribute_value_of_string "\">>\"");

  let attributes_of_string s = 
    let lexbuf = Lexing.from_string s in 
    match Xml_lexer.tokenize_attributes (None, []) lexbuf with 
    | Xml_types.Attribute_error -> "Attribute_error"
    | Xml_types.Attribute_values values -> (
      Xml_types.string_of_attributes values 
    )
  in
  Printf.printf "attributes: %s\n" (attributes_of_string "max='me'>");
  Printf.printf "attributes: %s\n" (attributes_of_string "max = 'me'>");
  Printf.printf "attributes: %s\n" (attributes_of_string "  max='me'>");
  Printf.printf "attributes: %s\n" (attributes_of_string "max='me'  >");
  Printf.printf "attributes: %s\n" (attributes_of_string "max='me'");
  
  XmlTest.print "<abc   a='b' \t c='dd  d >' \t >";

  XmlTest.print "<!-- This is a nice <> comment \n >><< -->";
  XmlTest.print "</blah <!-- This is a nice <> comment \n >><< --> >";
  XmlTest.print "<abc><![CDATA[blah <xml> this is !! Cool \t] ] ]]></abc>";
  *)

  Test5.print "iam a test string 12 ... 2324"; 
  Test5.print "iam a quote &quot;";
  Test5.print "iam a <  &lt;";
  Test5.print "iam a > &gt;";
  Test5.print "iam a ' &apos;";
  Test5.print "iam a \" &quot;";
  Test5.print "first string &quot; second string";
  Test5.print "&#123;";
  Test5.print "&#xff;";
  Test5.print "&quot;&#123;&quot;";
  Test5.print "&quot;&#xff;&quot;";


  let tokenize s = 
    let b = Buffer.create 128 in 
    let string_of_char_int i = 
      Char.chr i |> String.make 1 
    in 
    let lexbuf = Lexing.from_string s in 

    let rec loop () = 
      match Lex6.tokenize b string_of_char_int lexbuf with 
      | Lex6.More -> loop () 
      | Lex6.End -> ()
    in 
    loop (); 
    Buffer.contents b  
  in 

  let test s = 
    Printf.printf "%s\n >>%s\n" s (tokenize s)
  in 

  test "Pure text";
  test "123";
  test "this is a less than: &lt;";
  test "this is a greater than: &gt;";
  test "this is an ampersand: &amp;";
  test "this is a double quote: &quot;";
  test "this is a single  quote: &apos;";

  let f () i = 
    let s = Printf.sprintf "This is the character %i='&#%i;'" i i in 
    test s 
  in  
  fold_i f () (0, 155);
  ();;

module Make (Ord:Map.OrderedType)  = 
   struct  
    type t = Ord.t 
    type m = t list 

    let length (m:m) = List.length m  
end


let () = 

  let module StringCmp = struct 
      type t = string 
      let compare s1 s2 = 
        Pervasives.compare (String.uppercase s1) (String.uppercase s2)
  end in 

  let module SMap = Map.Make(StringCmp) in 
  let v = SMap.empty in  
  ()

    
