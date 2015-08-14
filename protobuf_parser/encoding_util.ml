
module Pc = Protobuf_codec 

let string_of_payload_kind = function 
  | Pc.Varint -> "varint"
  | Pc.Bits32 -> "bits32"
  | Pc.Bits64 -> "bits64"
  | Pc.Bytes  -> "bytes"
  
let payload_kind_of_field_type = function 
  | Astc.Field_type_double  -> Pc.Bits64
  | Astc.Field_type_float  -> Pc.Bits32 
  | Astc.Field_type_int32  -> Pc.Varint
  | Astc.Field_type_int64  -> Pc.Varint
  | Astc.Field_type_uint32  -> Pc.Varint 
  | Astc.Field_type_uint64 -> Pc.Varint
  | Astc.Field_type_sint32  -> Pc.Varint
  | Astc.Field_type_sint64  -> Pc.Varint
  | Astc.Field_type_fixed32  -> Pc.Bits32
  | Astc.Field_type_fixed64  -> Pc.Bits64
  | Astc.Field_type_sfixed32  -> Pc.Bits32
  | Astc.Field_type_sfixed64 -> Pc.Bits64
  | Astc.Field_type_bool  -> Pc.Varint 
  | Astc.Field_type_string  -> Pc.Bytes
  | Astc.Field_type_bytes  -> Pc.Bytes
  | Astc.Field_type_message i -> Pc.Bytes 
