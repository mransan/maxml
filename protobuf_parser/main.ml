
module L = Logger 

(** [parse_args ()] parses the command line argument 
    and returns [(in_channel, out_channel)] where 
    in_channel is where the protobuf definition can be read from 
    and out_channel where the generated code should be outputed.
  *)
let parse_args () = 
  let proto_file_name = ref "" in  
  let out_file_name   = ref "" in 
  let debug           = ref false in  
  let cmd_line_args = [
    ("-out", Arg.Set_string out_file_name, "out file name");  
    ("-debug", Arg.Set debug, "enable debugging");  
  ] in 
  let anon_fun  = (fun proto_file -> 
    proto_file_name := proto_file
  )  in 
  let usage = "protobufo.tsk -out <file_name> <file_name>.proto" in  
  Arg.parse cmd_line_args anon_fun usage;
  assert(!proto_file_name <> ""); 
  let struct_oc = match !out_file_name with 
    | "" -> stdout 
    | _  -> open_out (!out_file_name ^ ".ml") in 
  let sig_oc = match !out_file_name with 
    | "" -> stdout 
    | _  -> open_out (!out_file_name ^ ".mli") in 
  (open_in !proto_file_name, sig_oc, struct_oc, !debug)  

let () = 

  let ic, sig_oc, struct_oc, enable_debugging = parse_args () in 

  if enable_debugging
  then L.setup_from_out_channel stdout;
    
  (* -- Compilation -- *)
  let proto = 
    Parser.proto_ Lexer.lexer (Lexing.from_channel ic)
  in 
  let scope     = Astc_util.scope_of_package proto.Ast.package in 
  let astc_msgs = List.fold_left (fun astc_msgs ast_msg -> 
    astc_msgs @ Astc_util.compile_message_p1 scope ast_msg
  ) [] proto.Ast.messages in 
  L.log "-- Phase 1 --\n"; 
  List.iter (fun msg -> 
    L.endline @@ Astc_util.string_of_message msg
  ) astc_msgs; 
  let astc_msgs = List.map (Astc_util.compile_message_p2 astc_msgs) astc_msgs in 

  (* -- OCaml Backend -- *)

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
      BO.Codegen.gen_record_type r ^ "\n\n" ^ 
      BO.Codegen.gen_decode r ^ "\n\n" ^  
      BO.Codegen.gen_encode r ^ "\n\n" ^  
      BO.Codegen.gen_string_of r ^ "\n\n" 
    | BO.Variant v -> 
      s ^ 
      BO.Codegen.gen_variant_type v ^ "\n\n"
  ) s otypes in 
  output_string struct_oc s;  
  let s = List.fold_left (fun s -> function 
    | BO.Record r -> 
      s ^ 
      BO.Codegen.gen_record_type r ^ "\n\n" ^ 
      BO.Codegen.gen_decode_sig  r ^ "\n\n" ^  
      BO.Codegen.gen_encode_sig  r ^ "\n\n" ^  
      BO.Codegen.gen_string_of_sig r ^ "\n\n" 
    | BO.Variant v -> 
      s ^ 
      BO.Codegen.gen_variant_type v ^ "\n\n"
  ) "" otypes in
  output_string sig_oc s  
