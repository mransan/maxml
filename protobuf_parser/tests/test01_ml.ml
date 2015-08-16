
module Pc = Protobuf_codec 
module T  = Test01_types 

module Debug = struct 
  let string_of_tel_number {T.area_code; T.number} = 
    Printf.sprintf "{area_code:%i, number:%i}"
      area_code number
  
  let string_of_employment = function
    | T.Self_employed _ -> "Self_employed"
    | T.Employed_by s -> 
      Printf.sprintf "Employed_by(%s)" s

  let string_of_option f  = function
    | None   -> "None"
    | Some x -> f x  

  let string_of_person {
    T.first_name; 
    T.last_name; 
    T.date_of_birth; 
    T.tel_number; 
    T.employment; 
  } =
    Printf.sprintf "{first_name:%s, last_name:%s, date_of_birth:%i, tel_number:%s, employment:%s}"
      first_name
      last_name
      date_of_birth
      (string_of_option string_of_tel_number tel_number)
      (string_of_employment employment)

  let string_of_couple {T.p1; T.p2; T.contact_number} = 
    Printf.sprintf "{\n  p1:%s, \n  p2:%s, \n  contact_number:%s\n}" 
      (string_of_person p1)
      (string_of_person p2)
      (string_of_option string_of_tel_number contact_number)

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
    T.contact_number = None;
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
  if cp = decode_ref_data () 
  then (
    print_endline "-- Good --"; 
    exit 0
  )
  else (
    print_endline "-- Test Failed --";  
    print_endline @@ Debug.string_of_couple cp;
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

