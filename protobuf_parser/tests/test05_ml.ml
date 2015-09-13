module Pc = Protobuf_codec 
module T  = Test05_pb 

let inc = 2147

let decode_ref_data () = 
  let rec loop l = function
    | i when i < Int32.to_int Int32.max_int  - (2 * inc) -> (
      loop (i::l) (i + inc)
    )
    | i -> (
      l 
    ) 
  in 
  { T.l = List.rev @@ loop [] (Int32.to_int Int32.min_int) } 

let () = 

  let mode   = Test_util.parse_args () in 

  Printf.printf "min int: %i, max_int : %i \n" 
    (Int32.to_int Int32.min_int)
    (Int32.to_int Int32.max_int);

  match mode with 
  | Test_util.Decode -> ( 
      Test_util.decode ~noprint:() "test05.c2ml.data" T.decode_intlist T.string_of_intlist (decode_ref_data ()) 
  )
  | Test_util.Encode -> 
      Test_util.encode "test05.ml2c.data" T.encode_intlist (decode_ref_data ())
