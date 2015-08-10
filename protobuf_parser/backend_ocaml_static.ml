
let prefix_payload_to_ocaml_t  = {|

module Pc = Protobuf_codec 

let decode_varint_as_int decoder = 
  Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder 

let decode_bits32_as_int decoder = 
  Pc.Decoder.int_of_int32 "" @@ Pc.Decoder.bits32 decoder 

let decode_bits64_as_int decoder = 
  Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.bits64 decoder 

let decode_varint_as_bool decoder = 
  Pc.Decoder.bool_of_int64 "" @@ Pc.Decoder.varint decoder

let decode_bits32_as_float decoder = 
  Int32.float_of_bits @@ Pc.Decoder.bits32 decoder 
  
let decode_bits64_as_float decoder = 
  Int64.float_of_bits @@ Pc.Decoder.bits64 decoder 

let decode_bytes_as_string decoder = 
  Bytes.to_string @@ Pc.Decoder.bytes decoder 

let decode_bytes_as_bytes  = Pc.Decoder.bytes |}
  
let prefix_decode_f = {|

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

let oneof numbers l = 
  let ret = List.fold_left (fun x number -> 
    match x with 
    | Some _ -> x 
    | None -> optional number l 
 ) None numbers in 
 match ret with 
 | Some x -> x 
 | None -> failwith "None of oneof value could be found." 

let decode_sub f d = 
  Pc.Decoder.decode_exn f @@ Pc.Decoder.bytes d

let e () = failwith "programmatic error" 
|}
