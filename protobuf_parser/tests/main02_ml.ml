
module Pc = Protobuf_codec 
module T  = Test02_types 

module Debug = struct 

  let string_of_abt {
    T.o01 ;
    T.o02 ;
    T.o03 ;
    T.o04 ;
    T.o05 ;
    T.o06 ;
    T.o07 ;
    T.o08 ;
    T.o09 ;
    T.o10 ;
    (*
    T.o11 ;
    T.o12 ;
    *)
    T.o15 ;
    T.o13 ;
    T.o14 ;
  } =  

    Printf.sprintf "
    T.o01=%f,  
    T.o02=%f,  
    T.o03=%i,  
    T.o04=%i,  
    T.o05=%i,  
    T.o06=%i,  
    T.o07=%i, 
    T.o08=%i,  
    T.o09=%i,  
    T.o10=%i,  
    T.o11=,  
    T.o12=,  
    T.o13=%b,  
    T.o14=%s,  
    T.o15=%s"  
    o01 
    o02 
    o03 
    o04 
    o05 
    o06 
    o07 
    o08 
    o09 
    o10 
    (*
    o11 
    o12 
    *)
    o13 
    o14 
    o15 
end 


module Task_util = struct 
  type mode = 
    | Encode 
    | Decode 

  let mode_of_string  = function 
    | "encode" -> Encode
    | "decode" -> Decode
    | _ -> failwith "Invalid mode, must be [encode|decode]"

  let parse_args () = 
    let mode          = ref "" in  
    let cmd_line_args = [ ] in 
    let anon_fun  = (fun arg_mode -> 
      mode := arg_mode
    )  in 
    let usage = "test01_ml.tsk [encode|decode]" in  
    Arg.parse cmd_line_args anon_fun usage;
    mode_of_string !mode 

end 

let decode_ref_data () =
  ()

let decode () = 
  let ic     = open_in_bin "test02.c2ml.data" in 
  let buffer = Bytes.create 1024 in 
  let size   = ref 0 in 
  try 
    while true do  
      Bytes.set buffer !size (input_char ic); 
      size := !size + 1
    done
  with | End_of_file -> (); 
  Printf.printf "Done reading data, size=%i\n" !size ;

  let buffer = Bytes.sub buffer 0 !size in 

  let decoder = Pc.Decoder.of_bytes buffer in 
  let abt = T.decode_allbasicstypes decoder in  
  (*
  if cp = decode_ref_data () 
  then (
    print_endline "-- Good --"; 
    exit 0
  )
  else (
    print_endline "-- Test Failed --";  
  *)
    print_endline @@ Debug.string_of_abt abt;
    flush stdout;
    exit 0
  (* ) *)

let encode () = 
  (*
  let cp = decode_ref_data () in 
  let encoder = Pc.Encoder.create () in 
  T.encode_couple cp encoder; 
  let oc = open_out_bin "test01.ml2c.data" in 
  output_bytes oc @@ Pc.Encoder.to_bytes encoder 
  *)
  ()
   

let () = 

  let mode   = Task_util.parse_args () in 

  match mode with 
  | Task_util.Decode -> decode () 
  | Task_util.Encode -> encode ()

