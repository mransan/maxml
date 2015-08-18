
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


let printf_char_of_field_type = function
  | String    -> 's' 
  | Float     -> 'f'
  | Int       -> 'i'
  | Bytes     -> 's'
  | Bool      -> 'b'
  | User_defined _ -> 's'

(** utility function used to generate decode/encode function names 
    which are implemented in [Backend_ocaml_static].
 *)
let fname_of_payload_kind = function 
  | Encoding_util.Varint zigzag -> if zigzag then "varint_zigzag" else "varint"
  | Encoding_util.Bits32        -> "bits32"
  | Encoding_util.Bits64        -> "bits64"
  | Encoding_util.Bytes         -> "bytes"

type field_name = string 

type type_qualifier = 
  | No_qualifier
  | Option
  | List 

let string_of_field_type type_qualifier field_type = 
  let s = match field_type with 
    | String -> "string"
    | Float  -> "float"
    | Int    -> "int"
    | Bytes  -> "bytes"
    | Bool   -> "bool"
    | User_defined t -> t in 
  match type_qualifier with 
  | No_qualifier -> s 
  | Option       -> s ^ " option"
  | List         -> s ^ " list"

type field_encoding = {
  field_number:int; 
  payload_kind:Encoding_util.payload_kind; 
}

type record_encoding_type = 
  | Regular_field of field_encoding
  | One_of        of variant  

(** the field is parametrized by the encoding_type with could either 
    [field_encoding] or [record_encoding_type] depending 
    if the field is used in a variant or record type
 *) 
and 'a field = {
  field_type : field_type; 
  field_name : field_name; 
  type_qualifier : type_qualifier; 
  encoding_type : 'a;
}

and record = {
  record_name: string; 
  fields : record_encoding_type field list; 
}

and variant = {
  variant_name : string; 
  constructors : field_encoding field list;
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
    (* TODO this is a brute force method which only works for 
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

let compile_field ?as_constructor f type_qualifier message_scope all_messages field = 
  let field_name = Astc_util.field_name field in 
  let encoding_type = Astc_util.field_type field in 

  let field_name = match as_constructor with
    | Some _ -> constructor_name field_name 
    | None   -> record_field_name field_name 
  in 

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
    type_qualifier; 
    encoding_type = f {
      field_number = Astc_util.field_number field;
      payload_kind;
    }; 
  }

let compile_oneof all_messages message_scope outer_message_name {Astc.oneof_name ; Astc.oneof_fields } = 
  let {Astc.message_names; _ } = message_scope in 
  let variant_name = type_name (message_names @ [outer_message_name]) oneof_name in 
  let constructors = List.map (fun field -> 
    (* TODO fix hard coding the empty_scope and rather
        pass down the appropriate scope.
      *)
    compile_field ~as_constructor:() (fun x -> x) No_qualifier message_scope all_messages field 
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
    Astc.id = _ ; 
  } = message in 

  let {Astc.message_names; Astc.namespaces = _ } = message_scope in 
  let record_name = type_name message_names message_name in 
  let variants, fields = List.fold_left (fun (variants, fields) -> function
    | Astc.Message_field f -> (
      let type_qualifier = match Astc_util.field_label f with 
        | `Optional -> Option 
        | `Required -> No_qualifier
        | `Repeated -> List
      in 
      (variants, (compile_field (fun x -> Regular_field x) type_qualifier message_scope all_messages f)::fields)
    )
    | Astc.Message_oneof_field f -> (
      let variant = compile_oneof all_messages message_scope message_name f in 
      let field   = {
        field_type =  User_defined (variant.variant_name); 
        field_name =  record_field_name f.Astc.oneof_name;
        type_qualifier = No_qualifier;
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

  let sp x =  P.sprintf ("\n" ^^ x)  
  (** [sp x] same as sprintf but prefixed with new line *)

  let nl s = "\n" ^ s  
  (** [nl s] appends new line *)

  let concat = String.concat ""
  (** [concat l] concatenate a string list *)

  let add_indentation n s = 
    Str.global_replace (Str.regexp "^" ) (String.make (n * 2) ' ') s  
  (** [add_indentation n s] adds a multiple of 2 spaces indentation to [s] *)


  (** [gen_record_type r] generate the OCaml type declaration for [r]
   *)
  let gen_record_type {record_name; fields } = 
    concat [
      P.sprintf "type %s = {" record_name;
      concat @@ List.map (fun {field_name; field_type; type_qualifier; _ } -> 
        let type_name = string_of_field_type type_qualifier field_type in 
        sp "  %s : %s;" field_name type_name
      ) fields;
      "\n}"
    ]
  
  (** [gen_variant_type v] generate the OCaml type declaration for [v]
   *)
  let gen_variant_type {variant_name; constructors } = 
    concat [
      P.sprintf "type %s =" variant_name; 
      concat @@ List.map (fun {field_name; field_type; type_qualifier; _ } -> 
        let type_name = string_of_field_type type_qualifier field_type in 
        sp "  | %s of %s" field_name type_name
      ) constructors;
    ]

  (** [gen_mappings r] generates a per record variable to hold the 
      mapping between a field number and the associated decoding routine. 

      Because the order of fields inside the protobuffer message is not
      guaranteed, the decoding cannot be done in one step.   
      The decoding code must therefore first collect all the record fields 
      values and then create the value of the OCaml type. 
    *)
  let gen_mappings {record_name; fields} =

    concat [
      P.sprintf "let %s_mappings = [" record_name;
      concat @@ List.map (fun {encoding_type;field_type;_ } -> 
        match encoding_type with 
        | Regular_field {field_number; payload_kind } -> (
          let decoding = match field_type with 
            | User_defined t -> 
               P.sprintf "(fun d -> `%s (decode_%s (Pc.Decoder.nested d)))" (constructor_name t) t  
            | _ -> 
               let field_type = string_of_field_type No_qualifier field_type in 
               P.sprintf "(fun d -> `%s (decode_%s_as_%s d))" 
                 (constructor_name field_type)
                 (fname_of_payload_kind payload_kind)
                 field_type 
          in 
          sp "  (%i, %s);" field_number decoding 
        )
        | One_of {variant_name ; constructors; } -> (
          concat @@ List.map (fun {encoding_type; field_type; field_name; type_qualifier = _ } -> 
            let {field_number; payload_kind;} = encoding_type in 
            let decoding  =  match field_type with 
              | User_defined t -> 
                P.sprintf "(fun d -> `%s (%s (decode_%s (Pc.Decoder.nested d))))" 
                  (constructor_name variant_name) field_name t  
              | _ -> 
                let field_type = string_of_field_type No_qualifier field_type in 
                P.sprintf "(fun d -> `%s (%s (decode_%s_as_%s d)))" 
                  (constructor_name variant_name)
                  field_name
                  (fname_of_payload_kind payload_kind)
                  field_type 
            in 
            sp "  (%i, %s);" field_number decoding 
          ) constructors (* All variant constructors *) 
        )                (* One_of record field *)    
      ) fields ;
      "\n]";
    ]


  let gen_decode ({record_name; fields } as field) = 
    String.concat "" [
      P.sprintf "let decode_%s =" record_name;
      sp "%s" (add_indentation 1 @@ gen_mappings field); 
      sp "  in";
      sp "  (fun d ->"; 
      sp "    let l = decode d %s_mappings []  in  {" record_name;
      add_indentation 3 @@ concat @@ List.map (fun field -> 
        let {
          encoding_type;
          field_type; 
          field_name; 
          type_qualifier;
        } = field in 
        match encoding_type with 
        | Regular_field {field_number; _ } -> ( 
            let constructor = constructor_name (string_of_field_type No_qualifier field_type) in  
            match type_qualifier with
            | No_qualifier -> 
              sp "%s = required %i l (function | `%s __v -> __v | _ -> e());"
                field_name field_number constructor
            | Option -> 
              sp "%s = optional %i l (function | `%s __v -> __v | _ -> e());"
                field_name field_number constructor
            | List -> 
              sp "%s = list_ %i l (function | `%s __v -> __v | _ -> e());"
                field_name field_number constructor
        )
        | One_of {constructors; variant_name} -> 
            let all_numbers = List.fold_left (fun s {encoding_type= {field_number; _ } ; _ } -> 
              s ^ (P.sprintf "%i;" field_number)
            ) "[" constructors in 
            let all_numbers = all_numbers ^ "]" in 
            sp "%s = (match oneof %s l with | `%s __v -> __v | _ -> e());"
              field_name all_numbers (constructor_name variant_name)
      ) fields;
      sp "    }";
      sp "  )";
    ]
  
  let gen_decode_sig {record_name; _ } = 
    concat [
      P.sprintf "val decode_%s : Protobuf_codec.Decoder.t -> %s" 
        record_name record_name;
      sp "(** [decode_%s decoder] decodes a [%s] value from [decoder] *)"
        record_name record_name; 
    ]
  

  let gen_encode {record_name; fields } = 
    L.log "gen_encode record_name: %s\n" record_name; 

    let gen_field ?indent v_name field_number payload_kind field_type = 
      let s = concat [
        sp "Pc.Encoder.key (%i, Pc.%s) encoder; " 
          field_number (constructor_name @@ Encoding_util.string_of_payload_kind payload_kind);
        match field_type with 
        | User_defined t -> 
          sp "Pc.Encoder.nested (encode_%s %s) encoder;" t v_name 
        | _ ->  
          sp "encode_%s_as_%s %s encoder;"
            (string_of_field_type No_qualifier field_type) 
            (fname_of_payload_kind payload_kind) 
            v_name ;
      ] in 
      match indent with 
      | Some _ -> add_indentation 1 @@ s 
      | None   -> s 
    in

    concat [
      P.sprintf "let encode_%s v encoder = " record_name;
      add_indentation 1 @@ concat @@ List.map (fun field -> 
        L.log "gen_code field_name: %s\n" field.field_name;

        let { encoding_type; field_type; field_name; type_qualifier ; } = field in 
        match encoding_type with 
        | Regular_field {field_number; payload_kind } -> ( 
          match type_qualifier with 
          | No_qualifier -> (
            let v_name = P.sprintf "v.%s" field_name in 
            gen_field v_name field_number payload_kind field_type
          )
          | Option -> concat [
            sp "match v.%s with " field_name;
            sp "| Some x -> (%s)"
            (gen_field ~indent:() "x" field_number payload_kind field_type) ;
            sp "| None -> ();" ;
          ]
          | List -> concat [ 
            sp "List.iter (fun x -> ";
            gen_field ~indent:() "x" field_number payload_kind field_type;
            sp ") v.%s;" field_name; 
          ]
        )
        | One_of {constructors; variant_name = _} -> (  
          concat [
            sp "match v.%s with" field_name;
            concat @@ List.map (fun {encoding_type; field_type; field_name; type_qualifier= _ } ->
              let {field_number; payload_kind} = encoding_type in 
                let encode_field  = gen_field ~indent:() "x" field_number payload_kind field_type in 
                sp "| %s x -> (%s\n)" field_name encode_field
            ) constructors;
            ";";
          ]
        )           (* one of        *)
      ) fields;  (* record fields *) 
    "\n  ()"
    ]
  
  let gen_encode_sig {record_name; _ } = 
    concat [
      P.sprintf "val encode_%s : %s -> Protobuf_codec.Encoder.t -> unit"
        record_name
        record_name;
      sp "(** [encode_%s v encoder] encodes [v] with the given [encoder] *)" 
        record_name  
    ]
  
  let gen_string_of {record_name; fields } = 
    L.log "gen_string_of, record_name: %s\n" record_name; 

    let gen_field field_name field_type = 
      match field_type with 
      | User_defined t -> 
        P.sprintf "P.sprintf \"\\n%s: %%s\" @@ string_of_%s x" field_name t  
      | _ ->  
        P.sprintf "P.sprintf \"\\n%s: %%%c\" x"  
          field_name 
          (printf_char_of_field_type field_type)
    in

    concat [
      P.sprintf "let string_of_%s v = " record_name;
      "\n  add_indentation 1 @@ String.concat \"\" [";
      add_indentation 2 @@ concat @@ List.map (fun field -> 
        L.log "gen_string_of field_name: %s\n" field.field_name;
       
        let { field_type; field_name; type_qualifier ; encoding_type} = field in 
        match encoding_type with 
        | Regular_field _ -> ( 
          match type_qualifier with
          | No_qualifier -> 
            let field_string_of = gen_field field_name field_type in 
            sp "(let x = v.%s in %s);" field_name field_string_of 
          | Option -> 
            concat [
              sp "(match v.%s with " field_name;
              sp "| Some x -> (%s)"  (gen_field field_name field_type);
              sp "| None -> \"\\n%s: None\");" field_name;
            ]
          | List -> 
            concat [
              sp "String.concat \"\" @@ List.map (fun x ->";
              nl @@ gen_field field_name field_type; 
              sp ") v.%s;" field_name
            ]
        )
        | One_of {constructors; variant_name = _} -> (
          concat [
            sp "(match v.%s with" field_name;
            concat @@ List.map (fun {encoding_type=_; field_type; field_name;
            type_qualifier= _ } ->
              let field_string_of = gen_field field_name field_type in 
              sp "| %s x -> (%s)" field_name (add_indentation 1 field_string_of)
            ) constructors ;
            "\n);"       (* one of fields *) 
          ]
        )                (* one of        *)
      ) fields;          (* record fields *) 
      "\n  ]";
    ]

  let gen_string_of_sig {record_name; fields = _ } = 
    concat [
      P.sprintf "val string_of_%s : %s -> string " record_name record_name;
      sp "(** [string_of_%s v] returns a debugging string for [v] *)" record_name;
    ]

end  
