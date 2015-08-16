
module Pc = Protobuf_codec
module E  = Exception 
module L  = Logger 

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

(** utility function used to generate decode/encode function names 
    which are implemented in [Backend_ocaml_static].
 *)
let fname_of_payload_kind = function 
  | Encoding_util.Varint zigzag -> if zigzag then "varint_zigzag" else "varint"
  | Encoding_util.Bits32        -> "bits32"
  | Encoding_util.Bits64        -> "bits64"
  | Encoding_util.Bytes         -> "bytes"

type field_name = string 

type encoding_type = 
  | Regular_field of {
    field_number:int ; 
    payload_kind:Encoding_util.payload_kind; 
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

let type_name_of_message field_message_scope message_scope message_name = 
  let module S = String in 

  let message_scope = 
    (** TODO this is a brute force method which only works for 
        field which are in the same namespaces as their types. 
     *) 
    if field_message_scope.Astc.namespaces = message_scope.Astc.namespaces 
    then {message_scope with 
      Astc.namespaces = [] 
    }
    else message_scope in  

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

let get_type_name_from_all_messages field_message_scope all_messages i = 
  let module S = String in 
  try 
    let {Astc.message_scope; Astc.message_name; _ } = List.find (fun {Astc.id; _ } -> id = i) all_messages in 
    type_name_of_message field_message_scope message_scope message_name
  with | Not_found -> failwith "Programmatic error could not find type"

let compile_field ?as_constructor ?is_option message_scope all_messages field = 
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
  let payload_kind = Encoding_util.payload_kind_of_field_type encoding_type in 
  let field_type   = match encoding_type with
    | Astc.Field_type_double  -> Float
    | Astc.Field_type_float  ->  Float
    | Astc.Field_type_int32  ->  Int
    | Astc.Field_type_int64  ->  Int
    | Astc.Field_type_uint32  -> Int
    | Astc.Field_type_uint64 -> Int
    | Astc.Field_type_sint32  -> Int
    | Astc.Field_type_sint64  -> Int
    | Astc.Field_type_fixed32  -> Int
    | Astc.Field_type_fixed64  -> Int
    | Astc.Field_type_sfixed32  -> 
        raise @@ E.unsupported_field_type ~field_name ~field_type:"sfixed32" ~backend_name:"OCaml" () 
    | Astc.Field_type_sfixed64 -> 
        raise @@ E.unsupported_field_type ~field_name ~field_type:"sfixed64" ~backend_name:"OCaml" () 
    | Astc.Field_type_bool  -> Bool
    | Astc.Field_type_string  -> String
    | Astc.Field_type_bytes  -> Bytes
    | Astc.Field_type_message i -> User_defined ( 
      get_type_name_from_all_messages message_scope all_messages i
    )
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

let compile_oneof all_messages message_scope outer_message_name {Astc.oneof_name ; Astc.oneof_fields } = 
  let {Astc.message_names; _ } = message_scope in 
  let variant_name = type_name (message_names @ [outer_message_name]) oneof_name in 
  let constructors = List.map (fun field -> 
    (** TODO fix hard coding the empty_scope and rather
        pass down the appropriate scope.
      *)
    compile_field ~as_constructor:() message_scope all_messages field 
  ) oneof_fields in 
  {variant_name; constructors; }

let compile 
  (all_messages: Astc.resolved Astc.message list) 
  (message: Astc.resolved Astc.message ) :
  type_ list   = 

  let {
    Astc.message_scope;
    Astc.message_name; 
    Astc.message_body; 
  } = message in 

  let {Astc.message_names; Astc.namespaces = _ } = message_scope in 
  let record_name = type_name message_names message_name in 
  let variants, fields = List.fold_left (fun (variants, fields) -> function
    | Astc.Message_field f -> (
      let is_option = match Astc_util.field_label f with 
        | `Optional -> Some () 
        | `Required -> None 
        | `Repeated -> failwith "Repeated not supported"
      in 
      (variants, (compile_field message_scope ?is_option all_messages f)::fields)
    )
    | Astc.Message_oneof_field f -> (
      let variant = compile_oneof all_messages message_scope message_name f in 
      let field   = {
        field_type =  User_defined (variant.variant_name); 
        field_name =  record_field_name f.Astc.oneof_name;
        is_option  = false;
        encoding_type = One_of variant; 
      } in 
      ((Variant variant)::variants, field::fields) 
    )
  ) ([], []) message_body in 

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
             P.sprintf "(fun d -> `%s (decode_%s (Pc.Decoder.nested d)))" (constructor_name t) t  
          | _ -> 
             let field_type = string_of_field_type false field_type in 
             P.sprintf "(fun d -> `%s (decode_%s_as_%s d))" 
               (constructor_name field_type)
               (fname_of_payload_kind payload_kind)
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
                P.sprintf "(fun d -> `%s (%s (decode_%s (Pc.Decoder.nested d))))" 
                  (constructor_name variant_name) field_name t  
              | _ -> 
                let field_type = string_of_field_type false field_type in 
                P.sprintf "(fun d -> `%s (%s (decode_%s_as_%s d)))" 
                  (constructor_name variant_name)
                  field_name
                  (fname_of_payload_kind payload_kind)
                  field_type 
            in 
            s ^ P.sprintf "\n  (%i, %s);" field_number decoding 
          )
          | One_of _ -> raise @@ E.programmatic_error E.Recursive_one_of
        ) s constructors
      )
    ) s fields in 
    s ^ "\n]"

  let gen_decode_sig {record_name; _ } = 
    P.sprintf "val decode_%s : Protobuf_codec.Decoder.t -> %s" 
      record_name
      record_name 

  let gen_decode ({record_name; fields } as field) = 
    String.concat "" [
      P.sprintf "let decode_%s =" record_name;
      P.sprintf "  \n%s" (add_indentation 1 @@ gen_mappings field); 
      "\n  in";
      "\n  (fun d ->"; 
      P.sprintf "\n    let l = decode d %s_mappings []  in  {" record_name;
      add_indentation 3 @@ List.fold_left (fun s field -> 
        let {
          encoding_type;
          field_type; 
          field_name; 
          is_option;
        } = field in 
        match encoding_type with 
        | Regular_field {field_number; _ } -> ( 
            let constructor = constructor_name (string_of_field_type false field_type) in  
            match is_option with
            | false -> 
              s ^ P.sprintf "\n%s = (match required %i l with | `%s __v -> __v | _ -> e());"
                  field_name field_number constructor
            | true -> 
              s ^ P.sprintf "\n%s = optional %i l (function | `%s __v -> __v | _ -> e());"
                  field_name field_number constructor
        )
        | One_of {constructors; variant_name} -> 
            let all_numbers = List.fold_left (fun s {encoding_type; _ } -> 
              match encoding_type with
              | Regular_field {field_number; _ } -> s ^ (P.sprintf "%i;" field_number)
              | One_of _ -> raise @@ E.programmatic_error E.Recursive_one_of
            ) "[" constructors in 
            let all_numbers = all_numbers ^ "]" in 
            s ^ P.sprintf "\n%s = (match oneof %sl with | `%s __v -> __v | _ -> e());"
                field_name all_numbers (constructor_name variant_name)
      ) "" fields;
      "\n    }";
      "\n  )";
    ]

  (* ----TODO TEST --- *)

  let gen_encode {record_name; fields } = 
    L.log "gen_encode record_name: %s\n" record_name; 

    let gen_encode_field field_number payload_kind field_type = 
      P.sprintf "\nPc.Encoder.key (%i, Pc.%s) encoder; " 
        field_number (constructor_name @@ Encoding_util.string_of_payload_kind payload_kind) ^ 
      match field_type with 
      | User_defined t -> 
        P.sprintf "\nPc.Encoder.nested (encode_%s x) encoder;" t 
      | _ ->  
        P.sprintf "\nencode_%s_as_%s x encoder;"
          (string_of_field_type false field_type) 
          (fname_of_payload_kind payload_kind) 
    in

    let s = P.sprintf "let encode_%s v encoder = " record_name in 
    s ^ add_indentation 1 @@ List.fold_left (fun s field -> 
     L.log "gen_code field_name: %s\n" field.field_name;
     let {
       encoding_type;
       field_type; 
       field_name; 
       is_option;
     } = field in 
     match encoding_type with 
     | Regular_field {field_number; payload_kind } -> ( 
       match is_option with
       | false -> (s ^ 
         P.sprintf "\nlet x = v.%s in " field_name ^ 
         gen_encode_field field_number payload_kind field_type
       )
       | true -> (s ^ 
         P.sprintf "\nmatch v.%s with " field_name ^ 
         P.sprintf "\n| Some x -> (%s" 
           (add_indentation 1 @@ gen_encode_field field_number payload_kind field_type) ^ 
         P.sprintf "\n)" ^ 
         P.sprintf "\n| None -> ();"
       )
     )
     | One_of {constructors; variant_name} -> (  
       s ^ P.sprintf "\nmatch v.%s with" field_name ^ 
       List.fold_left (fun s {encoding_type; field_type; field_name; } ->
         match encoding_type with 
         | Regular_field {field_number; payload_kind} -> (
             s ^ P.sprintf "\n| %s x -> (" field_name ^ 
             (add_indentation 1 @@ gen_encode_field field_number payload_kind field_type) ^ 
             "\n)" 
         )
         | _ -> raise @@ E.programmatic_error E.Recursive_one_of
       ) "" constructors ^ ";"  (* one of fields *) 
     )                          (* one of        *)
    ) "" fields ^               (* record fields *) 
    "\n()"
  
  let gen_encode_sig {record_name; _ } = 
    P.sprintf "val encode_%s : %s -> Protobuf_codec.Encoder.t -> unit"
      record_name
      record_name 
end 
