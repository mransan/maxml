
module Pc = Protobuf_codec

let string_of_payload_kind = function 
  | Pc.Varint -> "varint"
  | Pc.Bits32 -> "bits32"
  | Pc.Bits64 -> "bits64"
  | Pc.Bytes  -> "bytes"

type field_type = 
  | String 
  | Float 
  | Int 
  | Bytes
  | Bool
  | User_defined of string 

let string_of_field_type is_option field_type = 
  let s = match field_type with 
    | String -> "string"
    | Float  -> "float"
    | Int    -> "int"
    | Bytes  -> "bytes"
    | Bool   -> "bool"
    | User_defined t -> t in 
  if is_option
  then s ^ " option"
  else s  

type field_name = string 

type encoding_type = 
  | Regular_field of {
    field_number:int ; 
    payload_kind:Pc.payload_kind; 
  }
  | One_of 

type field = {
  field_type : field_type; 
  field_name : field_name; 
  is_option  : bool;
  encoding_type : encoding_type;
}

type record = {
  record_name: string; 
  fields : field list; 
}

type variant = {
  variant_name : string; 
  constructors : field list;
}

type type_ = 
  | Record of record 
  | Variant of variant

let type_name message_scope name = 
  let module S = String in  
  let all_names =  message_scope @ [name] in 
  match all_names with 
  | []     -> failwith "Programmatic error"
  | hd::[] -> S.lowercase_ascii hd 
  | _      -> S.concat "_" @@ List.map S.lowercase_ascii all_names

let constructor_name s =
  String.capitalize_ascii @@ String.lowercase_ascii s 

let record_field_name s =
  String.lowercase_ascii s 

let type_name_of_message message_scope message_name = 
  let module S = String in 

  match message_scope with 
  | [] -> S.lowercase_ascii message_name
  | _  -> 
    let all_namespaces, all_message_names = List.partition (function
      | Astc.Namespace   _  -> true
      | Astc.Message_name _ -> false 
    ) message_scope in 

    let to_ocaml_name = function
      | Astc.Namespace s -> S.capitalize_ascii @@ S.lowercase_ascii s 
      | Astc.Message_name s -> S.lowercase_ascii s 
    in

    let module_prefix = match all_namespaces with
      | [] -> ""
      | _  -> 
        S.concat "." (List.map to_ocaml_name all_namespaces) ^ "."
    in 
    match all_message_names with
    | [] -> module_prefix^ (S.lowercase_ascii message_name)
    | _  -> 
      module_prefix ^ 
      S.concat "_" (List.map to_ocaml_name all_message_names) ^ 
      "_" ^
      S.lowercase_ascii message_name 

let get_type_name_from_all_messages all_messages i = 
  let module S = String in 
  try 
    let {Astc.message_scope; Astc.message_name; _ } = List.find (fun {Astc.id; _ } -> id = i) all_messages in 
    type_name_of_message message_scope message_name
  with | Not_found -> failwith "Programmatic error could not find type"

let process_field ?as_constructor ?is_option all_messages field = 
  let field_name = Astc_util.field_name field in 
  let encoding_type = Astc_util.field_type field in 

  let field_name = match as_constructor with
    | Some _ -> constructor_name field_name 
    | None   -> record_field_name field_name 
  in 

  let field_type, payload_kind = match encoding_type with
    | Astc.Field_type_double  -> Float, Pc.Bits64
    | Astc.Field_type_float  ->  Float, Pc.Bits32 
    | Astc.Field_type_int32  ->  Int, Pc.Varint
    | Astc.Field_type_int64  ->  Int, Pc.Varint
    | Astc.Field_type_uint32  -> Int, Pc.Varint 
    | Astc.Field_type_uint64 -> Int, Pc.Varint
    | Astc.Field_type_sint32  -> Int, Pc.Varint
    | Astc.Field_type_sint64  -> Int, Pc.Varint
    | Astc.Field_type_fixed32  -> Int, Pc.Bits32
    | Astc.Field_type_fixed64  -> Int, Pc.Bits64
    | Astc.Field_type_sfixed32  -> Int, Pc.Bits32
    | Astc.Field_type_sfixed64 -> Int, Pc.Bits64
    | Astc.Field_type_bool  -> Bool, Pc.Varint 
    | Astc.Field_type_string  -> String, Pc.Bytes
    | Astc.Field_type_bytes  -> Bytes, Pc.Bytes
    | Astc.Field_type_message i -> User_defined ( 
      get_type_name_from_all_messages all_messages i
    ), Pc.Bytes
  in 
  {
    field_type; 
    field_name; 
    is_option = (match is_option with | Some _ -> true | None -> false);
    encoding_type = Regular_field {
      field_number = Astc_util.field_number field;
      payload_kind;
    }; 
  }

let compile_oneof all_messages message_scope {Astc.oneof_name ; Astc.oneof_fields } = 
  let variant_name = type_name message_scope oneof_name in 
  let constructors = List.map (fun field -> 
    process_field ~as_constructor:() all_messages field 
  ) oneof_fields in 
  { variant_name; constructors; }

let compile 
  (all_messages: Astc.resolved Astc.message list) 
  (message: Astc.resolved Astc.message ) :
  type_ list   = 

  let {
    Astc.message_scope;
    Astc.message_name; 
    Astc.body_content; 
  } = message in 

  let message_scope = List.rev @@ List.fold_left (fun acc -> function
    | Astc.Namespace _ -> acc
    | Astc.Message_name s -> s::acc
  ) [] message_scope in 

  let record_name = type_name message_scope message_name in 
  let variants, fields = List.fold_left (fun (variants, fields) -> function
    | Astc.Message_field f -> (
      let is_option = match Astc_util.field_label f with 
        | `Optional -> Some () 
        | `Required -> None 
        | `Repeated -> failwith "Repeated not supported"
      in 
      (variants, (process_field ?is_option all_messages f)::fields)
    )
    | Astc.Message_oneof_field f -> (
      let variant = compile_oneof all_messages (message_scope @ [message_name]) f in 
      let field   = {
        field_type =  User_defined (variant.variant_name); 
        field_name =  record_field_name f.Astc.oneof_name;
        is_option  = false;
        encoding_type = One_of;
      } in 
      ((Variant variant)::variants, field::fields) 
    )
  ) ([], []) body_content in 

  List.rev (Record {
    record_name; 
    fields = List.rev fields;
  } :: variants)

module Print = struct 
  module P = Printf

  let gen_record {record_name; fields } = 
    let s = P.sprintf "type %s = {" record_name in 
    let s = List.fold_left (fun s {field_name; field_type; is_option; _ } -> 
      let type_name = string_of_field_type is_option field_type in 
      s ^ P.sprintf "\n  %s : %s;" field_name type_name
    ) s fields in 
    s ^ "\n}"
  
  (*
  let n_mappings = [
    (1, decode_bits32_as_float);
    (2, (fun d -> `M (decode_sub decode_m d)));
  ]
  *)
  
  let gen_mappings {record_name; fields} =
    let s = P.sprintf "let %s_mappings = [" record_name in 
    let s = List.fold_left (fun s {encoding_type;field_type;_ } -> 
      match encoding_type with 
      | Regular_field {field_number; payload_kind } -> (
        let decoding = match field_type with 
          | User_defined t -> 
             P.sprintf "(fun d -> `%s (decode_sub decode_%s d))" (constructor_name t) t  
          | _ -> 
             P.sprintf "decode_%s_as_%s" 
               (string_of_payload_kind payload_kind)
               (string_of_field_type false field_type) 
        in 
        s ^ P.sprintf "\n  (%i, %s);" field_number decoding 
      )
      | One_of -> failwith "One_of not supported"
    ) s fields in 
    s ^ "\n]"
end 