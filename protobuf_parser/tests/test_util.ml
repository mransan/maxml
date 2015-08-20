
module Pc = Protobuf_codec 

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

let decode file_name f_decode f_to_string ref_data  = 
  let buffer, size = get_binary_file_content file_name in 
  Printf.printf "Done reading data, size=%i\n" size ;

  let buffer = Bytes.sub buffer 0 size in 
  let decoder = Pc.Decoder.of_bytes buffer in 
  
  let x = f_decode decoder in  
  if  x = ref_data 
  then (
    print_endline "ML: -- Good --"; 
    exit 0
  )
  else (
    print_endline "ML: -- Test Failed --";  
    print_endline @@ f_to_string x;
    exit 1
  )

let encode file_name f_encode ref_data = 
  let encoder = Pc.Encoder.create () in 
  f_encode ref_data encoder; 
  let oc = open_out_bin file_name in 
  output_bytes oc @@ Pc.Encoder.to_bytes encoder 
