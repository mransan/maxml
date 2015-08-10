
(** [parse_args ()] parses the command line argument 
    and returns [(in_channel, out_channel)] where 
    in_channel is where the protobuf definition can be read from 
    and out_channel where the generated code should be outputed.
  *)
let parse_args () = 
  let proto_file_name = ref "" in  
  let out_file_name   = ref "" in 
  let cmd_line_args = [
    ("-out", Arg.Set_string out_file_name, "out file name");  
  ] in 
  let anon_fun  = (fun proto_file -> 
    proto_file_name := proto_file
  )  in 
  let usage = "protobufo.tsk -out <file_name> <file_name>.proto" in  
  Arg.parse cmd_line_args anon_fun usage;
  assert(!proto_file_name <> ""); 
  let oc = match !out_file_name with 
    | "" -> stdout 
    | _  -> open_out !out_file_name in 
  (open_in !proto_file_name, oc)  

let () = 

  let ic, oc = parse_args () in 
  let ast_msg = 
    Parser.message_ Lexer.lexer (Lexing.from_channel ic)
  in 
  let astc_msgs = Astc_util.compile_message_p1 [] ast_msg in 
  let astc_msgs = List.map (Astc_util.compile_message_p2 astc_msgs) astc_msgs in 
  let module BO = Backend_ocaml in 
  let otypes = List.fold_left (fun otypes m -> 
    otypes @ BO.compile astc_msgs m 
  ) [] astc_msgs in 
  let s = Backend_ocaml_static.prefix_payload_to_ocaml_t in 
  let s = s ^ Backend_ocaml_static.prefix_decode_f in 
  let s = s ^ "\n" in  
  let s = List.fold_left (fun s -> function 
    | BO.Record r -> 
      s ^ 
      BO.Print.gen_record_type r ^ "\n\n" ^ 
      BO.Print.gen_decode r ^ "\n\n" 
    | BO.Variant v -> 
      s ^ 
      BO.Print.gen_variant_type v ^ "\n\n"
  ) s otypes in 
  output_string oc s 
  
   


