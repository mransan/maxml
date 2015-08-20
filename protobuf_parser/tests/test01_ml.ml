
module Pc = Protobuf_codec 
module T  = Test01_types 

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
    T.p1 = {
      T.first_name = "John";
      T.last_name  = "Doe";
      T.date_of_birth = 19820429; 
      T.tel_number = None; 
      T.employment = T.Employed_by "Google";
    }; 
    T.p2 = {
      T.first_name = "Marie";
      T.last_name  = "Dupont";
      T.date_of_birth = 19820306; 
      T.tel_number = Some {T.area_code = 917; T.number = 1111111};
      T.employment = T.Employed_by "INRIA";
    };
    T.contact_numbers = {
      T.area_code = 917;
      T.number    = 123450;
    } :: {
      T.area_code = 917;
      T.number    = 123451;
    } :: []
  } 

let decode () = 
  let ic     = open_in_bin "test01.c2ml.data" in 
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
  let cp = T.decode_couple decoder in  
  print_endline @@ T.string_of_couple cp;
  if cp = decode_ref_data () 
  then (
    print_endline "-- Good --"; 
    exit 0
  )
  else (
    print_endline "-- Test Failed --";  
    print_endline @@ T.string_of_couple cp;
    exit 1
  )

let encode () = 
  let cp = decode_ref_data () in 
  let encoder = Pc.Encoder.create () in 
  T.encode_couple cp encoder; 
  let oc = open_out_bin "test01.ml2c.data" in 
  output_bytes oc @@ Pc.Encoder.to_bytes encoder 
   

let () = 

  let mode   = Task_util.parse_args () in 

  match mode with 
  | Task_util.Decode -> decode () 
  | Task_util.Encode -> encode ()

