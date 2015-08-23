
type payload_kind = 
  | Varint of bool (** zigzag *)  
  | Bits32
  | Bits64
  | Bytes 

type field_encoding = {
  field_number : int; 
  payload_kind : payload_kind; 
  nested : bool;
}

val string_of_payload_kind : payload_kind -> string 

val encoding_of_field_type : Astc.resolved Astc.proto -> (Astc.resolved, 'a) Astc.field -> field_encoding 
