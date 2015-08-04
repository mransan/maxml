
module Pc = Protobuf_codec 
module Pr = Printf

let string_of_payload = function 
    | Pc.Varint -> "Varint" 
    | Pc.Bits32 -> "Bits32"
    | Pc.Bits64 -> "Bits64"
    | Pc.Bytes  -> "Bytes"
 
type ocaml_value = 
  | Int of int 
  | Float of float 
  | String of string 

let as_string = function 
  | String s -> s 
  | _ -> failwith "not a string"

let as_int = function 
  | Int i -> i
  | _ -> failwith "not a int"

let as_float = function 
  | Float f -> f
  | _ -> failwith "not a float"

let decode_int32_as_int decoder = function
  | Pc.Varint -> 
      Int (Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder)  
  | _ -> failwith "decode error int32 -> int"

let decode_string_as_string decoder = function
  | Pc.Bytes -> 
      let b = Pc.Decoder.bytes decoder in 
      String (Bytes.to_string b) 
  | _ -> failwith "decode error int32 -> int"


let rec decode decoder mappings values = 
  match Pc.Decoder.key decoder with 
  | None -> values 
  | Some (number, payload_kind) -> (
    try 
      let mapping = List.assoc number mappings in 
      decode decoder mappings ((number, mapping decoder payload_kind) :: values) 
    with | Not_found -> values 
  )

(** --------------- *) 

type m = {
  v1: int; 
  v2: string; 
}

let m_mappings = [
  (1, decode_int32_as_int);
  (2, decode_string_as_string);  
]

let required number l = 
  List.assoc number l 

let optional number l = 
  try 
    Some (List.assoc number l) 
  with | Not_found -> None 

let m_decode l = {
  v1 = as_int    @@ required 1 l; 
  v2 = as_string @@ required 2 l;
}
   
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

  let l = decode decoder m_mappings [] in 
  let m = m_decode l in 
  Printf.printf "m.v1 = %i, m.v2 = %s\n" m.v1 m.v2
