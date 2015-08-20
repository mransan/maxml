
module Pc = Protobuf_codec 
module T  = Test02_types 

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

let decode_ref_data () = {
  T.o01 = 1.0;
  T.o02 = 2.0;
  T.o03 = (- 123);
  T.o04 = 456;
  T.o05 = 123;
  T.o06 = 456; 
  T.o07 = (- 123);
  T.o08 = (- 456); 
  T.o09 = 123;
  T.o10 = 456;
  (*
  T.o11 ;
  T.o12 ;
  *)
  T.o13 = true;
  T.o14 = "Iam a test string";
  T.o15 = "Iam a test byte"; 
}


let inc_size = 16 

let get_binary_file_content file_name = 
  let ic     = open_in_bin file_name in 
  let rec loop b offset remaining = 
    match input ic b offset remaining with
    | 0 -> (b, offset)
    | i when i = remaining -> (
      let b = Bytes.extend b 0 inc_size in 
      loop b (offset + remaining) inc_size
    )
    | i -> (
      (b, offset + i) 
    ) 
  in 
  loop (Bytes.create inc_size) 0 inc_size


let decode () = 
  let buffer, size = get_binary_file_content "test02.c2ml.data" in 
  Printf.printf "Done reading data, size=%i\n" size ;

  let buffer = Bytes.sub buffer 0 size in 
  let decoder = Pc.Decoder.of_bytes buffer in 
  
  let abt = T.decode_allbasicstypes decoder in  
  if abt = decode_ref_data () 
  then (
    print_endline "ML: -- Good --"; 
    exit 0
  )
  else (
    print_endline "ML: -- Test Failed --";  
    print_endline @@ T.string_of_allbasicstypes abt;
    exit 1
  )

let encode () = 
  let abt = decode_ref_data () in 
  let encoder = Pc.Encoder.create () in 
  T.encode_allbasicstypes abt encoder; 
  let oc = open_out_bin "test02.ml2c.data" in 
  output_bytes oc @@ Pc.Encoder.to_bytes encoder 
   
let () = 

  let mode   = Task_util.parse_args () in 

  match mode with 
  | Task_util.Decode -> decode () 
  | Task_util.Encode -> encode ()

