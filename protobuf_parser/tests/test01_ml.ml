
module Pc = Protobuf_codec 
module Types = Test01_types 

let () = 

  let ic = open_in_bin "test01.data" in 
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
  let n = Types.decode_n decoder in  
  let m = n.Types.n2 in 
  Printf.printf "n.n1 = %f, m.v1 = %i, m.v2 = %s\n" n.Types.n1 m.Types.v1 m.Types.v2
