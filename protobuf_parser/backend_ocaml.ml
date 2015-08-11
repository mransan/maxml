
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
  | One_of of variant  

and field = {
  field_type : field_type; 
  field_name : field_name; 
  is_option  : bool;
  encoding_type : encoding_type;
}

and record = {
  record_name: string; 
  fields : field list; 
}

and variant = {
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

let empty = function | [] -> true | _ -> false 

let type_name_of_message message_scope message_name = 
  let module S = String in 

  let {Astc.namespaces; Astc.message_names} = message_scope in 

  if empty namespaces && empty message_names 
  then S.lowercase_ascii message_name 
  else 

    let namespaces = List.map (fun s -> 
      S.capitalize_ascii @@ S.lowercase_ascii s
    ) namespaces in 
    let message_names = List.map S.lowercase_ascii message_names in 

    let module_prefix = match namespaces with
      | [] -> ""
      | _  -> 
        S.concat "." namespaces  ^ "."
    in 
    match message_names with
    | [] -> module_prefix^ (S.lowercase_ascii message_name)
    | _  -> 
      module_prefix ^ 
      S.concat "_" message_names ^ 
      "_" ^
      S.lowercase_ascii message_name 

let get_type_name_from_all_messages all_messages i = 
  let module S = String in 
  try 
    let {Astc.message_scope; Astc.message_name; _ } = List.find (fun {Astc.id; _ } -> id = i) all_messages in 
    type_name_of_message message_scope message_name
  with | Not_found -> failwith "Programmatic error could not find type"

let compile_field ?as_constructor ?is_option all_messages field = 
  let field_name = Astc_util.field_name field in 
  let encoding_type = Astc_util.field_type field in 

  let field_name = match as_constructor with
    | Some _ -> constructor_name field_name 
    | None   -> record_field_name field_name 
  in 

  (** TODO mapping from Astc.field_type to the Pc.payload_kind 
      should be outside of Ocaml_backend module 
      since it is general to ALL language. 
   *)
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
    compile_field ~as_constructor:() all_messages field 
  ) oneof_fields in 
  { variant_name; constructors; }

let compile 
  (all_messages: Astc.resolved Astc.message list) 
  (message: Astc.resolved Astc.message ) :
  type_ list   = 

  let {
    Astc.message_scope = {Astc.message_names; Astc.namespaces = _ } ;
    Astc.message_name; 
    Astc.body_content; 
  } = message in 

  let record_name = type_name message_names message_name in 
  let variants, fields = List.fold_left (fun (variants, fields) -> function
    | Astc.Message_field f -> (
      let is_option = match Astc_util.field_label f with 
        | `Optional -> Some () 
        | `Required -> None 
        | `Repeated -> failwith "Repeated not supported"
      in 
      (variants, (compile_field ?is_option all_messages f)::fields)
    )
    | Astc.Message_oneof_field f -> (
      let variant = compile_oneof all_messages (message_names @ [message_name]) f in 
      let field   = {
        field_type =  User_defined (variant.variant_name); 
        field_name =  record_field_name f.Astc.oneof_name;
        is_option  = false;
        encoding_type = One_of variant; 
      } in 
      ((Variant variant)::variants, field::fields) 
    )
  ) ([], []) body_content in 

  List.rev (Record {
    record_name; 
    fields = List.rev fields;
  } :: variants)

module Codegen = struct 
  module P = Printf

  let add_indentation n s = 
    Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  

  let gen_record_type {record_name; fields } = 
    let s = P.sprintf "type %s = {" record_name in 
    let s = List.fold_left (fun s {field_name; field_type; is_option; _ } -> 
      let type_name = string_of_field_type is_option field_type in 
      s ^ P.sprintf "\n  %s : %s;" field_name type_name
    ) s fields in 
    s ^ "\n}"
  
  let gen_variant_type {variant_name; constructors } = 
    let s = P.sprintf "type %s =" variant_name in 
    List.fold_left (fun s {field_name; field_type; is_option; _ } -> 
      let type_name = string_of_field_type is_option field_type in 
      s ^ P.sprintf "\n  | %s of %s" field_name type_name
    ) s constructors

  let gen_mappings {record_name; fields} =
    let s = P.sprintf "let %s_mappings = [" record_name in 
    let s = List.fold_left (fun s {encoding_type;field_type;_ } -> 
      match encoding_type with 
      | Regular_field {field_number; payload_kind } -> (
        let decoding = match field_type with 
          | User_defined t -> 
             P.sprintf "(fun d -> `%s (decode_sub decode_%s d))" (constructor_name t) t  
          | _ -> 
             let field_type = string_of_field_type false field_type in 
             P.sprintf "(fun d -> `%s (decode_%s_as_%s d))" 
               (constructor_name field_type)
               (string_of_payload_kind payload_kind)
               field_type 
        in 
        s ^ P.sprintf "\n  (%i, %s);" field_number decoding 
      )
      | One_of {variant_name ; constructors; } -> (
        List.fold_left (fun s {encoding_type; field_type; field_name } -> 
          match encoding_type with
          | Regular_field {field_number; payload_kind } -> (
            let decoding  =  match field_type with 
            | User_defined t -> 
               P.sprintf "(fun d -> `%s (%s (decode_sub decode_%s d)))" (constructor_name variant_name) field_name t  
            | _ -> 
               let field_type = string_of_field_type false field_type in 
               P.sprintf "(fun d -> `%s (%s (decode_%s_as_%s d)))" 
                 (constructor_name variant_name)
                 field_name
                 (string_of_payload_kind payload_kind)
                 field_type 
            in 
            s ^ P.sprintf "\n  (%i, %s);" field_number decoding 
          )
          | One_of _ -> failwith "Programatic error"
        ) s constructors
      )
    ) s fields in 
    s ^ "\n]"

  let gen_decode_sig {record_name; _ } = 
    P.sprintf "val decode_%s : Protobuf_codec.Decoder.t -> %s" 
      record_name
      record_name 

  let gen_decode ({record_name; fields } as field) = 
    let s = P.sprintf "let decode_%s =" record_name in
    let s = s ^ P.sprintf "  \n%s" (add_indentation 1 @@ gen_mappings field) in 
    let s = s ^ "\n  in" in 
    let s = s ^ "\n  (fun d ->" in 
    let s = s ^ P.sprintf "\n    let l = decode d %s_mappings []  in  {" record_name in 
    let s = s ^ add_indentation 3 @@ List.fold_left (fun s {encoding_type; field_type; field_name} ->  
      match encoding_type with 
      | Regular_field {field_number; _ } -> 
          let constructor = constructor_name (string_of_field_type false field_type) in  
          s ^ P.sprintf "\n%s = (match required %i l with | `%s __v -> __v | _ -> e());"
              field_name field_number constructor
      | One_of {constructors; variant_name} -> 
          let all_numbers = List.fold_left (fun s {encoding_type; _ } -> 
            match encoding_type with
            | Regular_field {field_number; _ } -> s ^ (P.sprintf "%i;" field_number)
            | One_of _ -> failwith "Programatic error"
          ) "[" constructors in 
          let all_numbers = all_numbers ^ "]" in 
          s ^ P.sprintf "\n%s = (match oneof %sl with | `%s __v -> __v | _ -> e());"
              field_name all_numbers (constructor_name variant_name)
    ) "" fields in 
    let s = s ^ "\n    }" in 
    s ^ "\n  )"
     
end 
