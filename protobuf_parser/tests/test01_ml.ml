
module Pc = Protobuf_codec 
module T  = Test01_types 

module Util = struct 
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
  let cp = T.decode_couple decoder in  
  print_endline @@ Util.string_of_couple cp 
