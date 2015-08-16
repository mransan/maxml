
module Pc = Protobuf_codec 

type payload_kind = 
  | Varint of bool (** zigzag *)  
  | Bits32
  | Bits64
  | Bytes 

let string_of_payload_kind = function 
  | Varint _ -> "varint"
  | Bits32   -> "bits32"
  | Bits64   -> "bits64"
  | Bytes    -> "bytes"

let payload_kind_of_field_type = function 
  | Astc.Field_type_double  -> Bits64
  | Astc.Field_type_float  -> Bits32 
  | Astc.Field_type_int32  -> Varint false
  | Astc.Field_type_int64  -> Varint false
  | Astc.Field_type_uint32  -> Varint false
  | Astc.Field_type_uint64 -> Varint false
  | Astc.Field_type_sint32  -> Varint true
  | Astc.Field_type_sint64  -> Varint true 
  | Astc.Field_type_fixed32  -> Bits32
  | Astc.Field_type_fixed64  -> Bits64
  | Astc.Field_type_sfixed32  -> Bits32
  | Astc.Field_type_sfixed64 -> Bits64
  | Astc.Field_type_bool  -> Varint false 
  | Astc.Field_type_string  -> Bytes
  | Astc.Field_type_bytes  -> Bytes
  | Astc.Field_type_message i -> Bytes 
