module Pc = Protobuf_codec
module T  = Test04_types 

let decode_ref_data () = {
  T.j = 456;
}

let () = 

  let mode   = Test_util.parse_args () in 

  match mode with 
  | Test_util.Decode -> 
      Test_util.decode "test03.c2ml.data" T.decode_test T.string_of_test (decode_ref_data ()) 
  | Test_util.Encode -> 
      Test_util.encode "test04.ml2c.data" T.encode_test (decode_ref_data ())
