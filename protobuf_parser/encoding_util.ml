
module Pc = Protobuf_codec 


type payload_kind = 
  | Varint of bool (** zigzag *)  
  | Bits32
  | Bits64
  | Bytes 

type field_encoding = {
  field_number:int; 
  payload_kind:payload_kind; 
  nested : bool;
}

let string_of_payload_kind = function 
  | Varint _ -> "varint"
  | Bits32   -> "bits32"
  | Bits64   -> "bits64"
  | Bytes    -> "bytes"

let payload_kind_of_field_type all_types (field:(Astc.resolved, 'a)Astc.field) = 
  let pk, nested = match Astc_util.field_type field with 
    | Astc.Field_type_double  -> (Bits64,false)
    | Astc.Field_type_float  -> (Bits32 ,false)
    | Astc.Field_type_int32  -> (Varint false,false)
    | Astc.Field_type_int64  -> (Varint false,false)
    | Astc.Field_type_uint32  -> (Varint false,false)
    | Astc.Field_type_uint64 -> (Varint false,false)
    | Astc.Field_type_sint32  -> (Varint true,false)
    | Astc.Field_type_sint64  -> (Varint true ,false)
    | Astc.Field_type_fixed32  -> (Bits32,false)
    | Astc.Field_type_fixed64  -> (Bits64,false)
    | Astc.Field_type_sfixed32  -> (Bits32,false)
    | Astc.Field_type_sfixed64 -> (Bits64,false)
    | Astc.Field_type_bool  -> (Varint false ,false)
    | Astc.Field_type_string  -> (Bytes,false)
    | Astc.Field_type_bytes  -> (Bytes,false)
    | Astc.Field_type_type id -> 
      match Astc_util.type_of_id all_types id with 
      | Astc.Enum    _ -> (Varint false, false)  
      | Astc.Message _ -> (Bytes, true) 
  in {
    payload_kind = pk;
    nested; 
    field_number = Astc_util.field_number field
  }
