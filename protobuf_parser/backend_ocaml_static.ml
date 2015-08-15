
let prefix_payload_to_ocaml_t  = {|

module Pc = Protobuf_codec 

let decode_varint_as_int decoder = 
  Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.varint decoder 

let encode_int_as_varint v encoder = 
  Pc.Encoder.varint (Int64.of_int v) encoder  

let decode_bits32_as_int decoder = 
  Pc.Decoder.int_of_int32 "" @@ Pc.Decoder.bits32 decoder 

let encode_int_as_bits32 v encoder = 
  Pc.Encoder.bits32 (Pc.Encoder.int32_of_int "" v) encoder

let decode_bits64_as_int decoder = 
  Pc.Decoder.int_of_int64 "" @@ Pc.Decoder.bits64 decoder 

let encode_int_as_bit64 v encoder = 
  Pc.Encoder.bits64 (Int64.of_int v) encoder

let decode_varint_as_bool decoder = 
  Pc.Decoder.bool_of_int64 "" @@ Pc.Decoder.varint decoder

let encode_bool_as_varint v encoder = 
  Pc.Encoder.varint (Int64.of_int @@ if v  then 1 else 0) encoder 

let decode_bits32_as_float decoder = 
  Int32.float_of_bits @@ Pc.Decoder.bits32 decoder 

let encode_float_as_bits32 v encoder =
  Pc.Encoder.bits32 (Int32.bits_of_float v) encoder 
  
let decode_bits64_as_float decoder = 
  Int64.float_of_bits @@ Pc.Decoder.bits64 decoder 

let encode_float_as_bits64 v encoder =
  Pc.Encoder.bits64 (Int64.bits_of_float v) encoder

let decode_bytes_as_string decoder = 
  Bytes.to_string @@ Pc.Decoder.bytes decoder 

let encode_string_as_bytes v encoder = 
  Pc.Encoder.bytes (Bytes.of_string v) encoder 

let decode_bytes_as_bytes  = Pc.Decoder.bytes 

let encode_bytes_as_bytes  = Pc.Encoder.bytes

|}

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

let optional number l f = 
  try 
    Some (f @@ List.assoc number l) 
  with | Not_found -> None 

external identity: 'a -> 'a = "%identity"

let oneof numbers l = 
  let ret = List.fold_left (fun x number -> 
    match x with 
    | Some _ -> x 
    | None   -> optional number l identity  
 ) None numbers in 
 match ret with 
 | Some x -> x 
 | None -> failwith "None of oneof value could be found." 

let decode_sub f d = 
  Pc.Decoder.decode_exn f @@ Pc.Decoder.bytes d

let e () = failwith "programmatic error" 
|}
