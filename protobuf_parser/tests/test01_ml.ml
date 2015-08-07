
module Pc = Protobuf_codec 
module Pr = Printf

let string_of_payload = function 
    | Pc.Varint -> "Varint" 
    | Pc.Bits32 -> "Bits32"
    | Pc.Bits64 -> "Bits64"
    | Pc.Bytes  -> "Bytes"
 
let decode_bits32_as_int decoder = 
  let b = Pc.Decoder.varint decoder in 
  `Int (Pc.Decoder.int_of_int64 "" b)

let decode_string_as_string decoder = 
  let b = Pc.Decoder.bytes decoder in 
  `String (Bytes.to_string b) 

let decode_bits32_as_float decoder = 
  let b = Pc.Decoder.bits32 decoder in 
  `Float (Int32.float_of_bits b) 

let rec decode decoder mappings values = 
  match Pc.Decoder.key decoder with 
  | None -> values 
  | Some (number, payload_kind) -> (
    try 
      let mapping = List.assoc number mappings in 
      decode decoder mappings ((number, mapping decoder) :: values) 
    with | Not_found -> values 
  )

let required number l = 
  List.assoc number l 

let optional number l = 
  try 
    Some (List.assoc number l) 
  with | Not_found -> None 

let sub_decoder d = 
  Pc.Decoder.of_bytes @@ Pc.Decoder.bytes d 

let decode_sub f d = 
  Pc.Decoder.decode_exn f @@ Pc.Decoder.bytes d

let e () = failwith "programmatic error"

(** --------------- *) 

type m = {
  v1: int; 
  v2: string; 
}

let decode_m =  
  let m_mappings = [
    (1, decode_bits32_as_int);
    (2, decode_string_as_string);  
  ] in 
  (fun d -> 
    let l = decode d m_mappings  [] in  {
      v1 = (match required 1 l with | `Int __v -> __v  | _ -> e ());
      v2 = (match required 2 l with | `String __v -> __v | _ -> e ());
    } 
  )

type n = {
  n1 : float; 
  n2 : m; 
}

let decode_n =
  let n_mappings = [
    (1, decode_bits32_as_float);
    (2, (fun d -> `M (decode_sub decode_m d)));
  ]
  in 
  (fun d ->  
    let l = decode d n_mappings [] in {
      n1 = (match required 1 l with | `Float __v -> __v | _ -> e());
      n2 = (match required 2 l with | `M __v -> __v | _ -> e());
    }
  )

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
  let n = decode_n decoder in  
  let m = n.n2 in 
  Printf.printf "n.n1 = %f, m.v1 = %i, m.v2 = %s\n" n.n1 m.v1 m.v2
