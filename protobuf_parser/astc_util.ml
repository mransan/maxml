
  
module E = Exception 
module L = Logger 

let field_name {Astc.field_parsed; _ } = 
  let {Ast.field_name; _ } = field_parsed in 
  field_name 

let field_number {Astc.field_parsed = {Ast.field_number;_}; _ } = 
  field_number 

let field_type {Astc.field_type; _ } = 
  field_type

let field_label {Astc.field_parsed = {Ast.field_label; _ }; _ } = 
  field_label 

let empty_scope  = { Astc.namespaces = []; Astc.message_names = [] } 

let rev_split_by_char c s = 
  let rec loop i l = 
    try 
      let i' = String.index_from s i '.' in 
      let s' = String.sub s i (i' - i)  in 
      loop (i'+1) (if s' = "" then l else s'::l)  
    with Not_found -> (String.sub s i (String.length s - i) ):: l 
  in 
  loop 0 []

let string_of_string_list l = 
  Printf.sprintf "[%s]" (String.concat "," l)

let string_of_unresolved { Astc.scope; Astc.type_name; Astc.from_root }  =
  Printf.sprintf "unresolved:{scope %s, type_name: %s, from_root: %b}"
    (string_of_string_list scope) 
    type_name 
    from_root  

let string_of_message_scope {Astc.namespaces; Astc.message_names}  = 
  Printf.sprintf "scope:{namespaces:%s, message_names:%s}" 
    (string_of_string_list namespaces)
    (string_of_string_list message_names) 

let string_of_message {Astc.id; Astc.message_scope; Astc.message_name; Astc.message_body} = 
  Printf.sprintf "message: {id:%i, message_scope:%s, name:%s, field nb:%i}" 
    id 
    (string_of_message_scope message_scope) 
    message_name
    (List.length message_body)

let scope_of_package = function
  | Some s -> {empty_scope with 
    Astc.namespaces = List.rev @@ rev_split_by_char '.' s
  }
  | None -> empty_scope 

let unresolved_of_string s = 
  match rev_split_by_char '.' s with 
  | [] -> raise @@ E.programmatic_error E.Invalid_string_split
  | hd :: tl -> {
    Astc.scope = (List.rev tl); 
    Astc.type_name = hd;
    Astc.from_root = String.get s 0 = '.';
  }

let field_type_of_string = function
 | "double"    -> Astc.Field_type_double 
 | "float"     -> Astc.Field_type_float 
 | "int32"     -> Astc.Field_type_int32 
 | "int64"     -> Astc.Field_type_int64 
 | "uint32"    -> Astc.Field_type_uint32 
 | "uint64"    -> Astc.Field_type_uint64
 | "sint32"    -> Astc.Field_type_sint32 
 | "sint64"    -> Astc.Field_type_sint64 
 | "fixed32"   -> Astc.Field_type_fixed32 
 | "fixed64"   -> Astc.Field_type_fixed64 
 | "sfixed32"  -> Astc.Field_type_sfixed32 
 | "sfixed64"  -> Astc.Field_type_sfixed64
 | "bool"      -> Astc.Field_type_bool 
 | "string"    -> Astc.Field_type_string 
 | "bytes"     -> Astc.Field_type_bytes 
 | s  -> Astc.Field_type_message (unresolved_of_string s) 

let map_field_type : 'a Astc.field_type  -> 'b Astc.field_type = function  
 | Astc.Field_type_double     -> Astc.Field_type_double    
 | Astc.Field_type_float      -> Astc.Field_type_float 
 | Astc.Field_type_int32      -> Astc.Field_type_int32 
 | Astc.Field_type_int64      -> Astc.Field_type_int64 
 | Astc.Field_type_uint32     -> Astc.Field_type_uint32 
 | Astc.Field_type_uint64     -> Astc.Field_type_uint64
 | Astc.Field_type_sint32     -> Astc.Field_type_sint32 
 | Astc.Field_type_sint64     -> Astc.Field_type_sint64 
 | Astc.Field_type_fixed32    -> Astc.Field_type_fixed32 
 | Astc.Field_type_fixed64    -> Astc.Field_type_fixed64 
 | Astc.Field_type_sfixed32   -> Astc.Field_type_sfixed32 
 | Astc.Field_type_sfixed64   -> Astc.Field_type_sfixed64
 | Astc.Field_type_bool       -> Astc.Field_type_bool 
 | Astc.Field_type_string     -> Astc.Field_type_string 
 | Astc.Field_type_bytes      -> Astc.Field_type_bytes 
 | _ -> raise @@ E.programmatic_error E.Unexpect_field_type 

let compile_default field_name constant = function  
  | Astc.Field_type_double 
  | Astc.Field_type_float -> (
    match constant with 
    | Ast.Constant_int i -> Ast.Constant_float (float_of_int i)
    | _ -> 
      raise @@ E.invalid_default_value 
        ~field_name ~info:"invalid default type (float/int expected)" ()
  )
  | Astc.Field_type_int32 
  | Astc.Field_type_int64 
  | Astc.Field_type_sint32 
  | Astc.Field_type_sint64 
  | Astc.Field_type_fixed32 
  | Astc.Field_type_fixed64 
  | Astc.Field_type_sfixed32 
  | Astc.Field_type_sfixed64 -> (
    match constant with 
    | Ast.Constant_int _ -> constant  
    | _ -> 
      raise @@ E.invalid_default_value 
        ~field_name ~info:"invalid default type (int expected)" ()
  )
  | Astc.Field_type_uint32 
  | Astc.Field_type_uint64 -> (
    match constant with 
    | Ast.Constant_int i -> if i >=0 
      then constant 
      else raise @@ E.invalid_default_value 
        ~field_name ~info:"negative default value for unsigned int" () 
    | _ -> raise @@ E.invalid_default_value
        ~field_name ~info:"invalid default type (int expected)" ()
  )
  | Astc.Field_type_bool -> (
    match constant with 
    | Ast.Constant_bool _ -> constant
    | _ -> raise @@ E.invalid_default_value 
      ~field_name ~info:"invalid default type (bool expected)" ()
  ) 
  | Astc.Field_type_string -> (
   match constant with 
   | Ast.Constant_string _ -> constant
    | _ -> raise @@ E.invalid_default_value 
      ~field_name ~info:"invalid default type (string expected)" ()
  ) 
  | Astc.Field_type_bytes -> raise @@ E.invalid_default_value 
    ~field_name ~info:"default value not supported for bytes" ()
  | Astc.Field_type_message _ -> raise @@ E.invalid_default_value 
    ~field_name ~info:"default value not supported for message" ()

let get_default field_name field_options field_type = 
  match List.assoc "default" field_options with
  | constant -> Some (compile_default field_name constant field_type)
  | exception Not_found -> None 

let compile_field_p1 ({
  Ast.field_name;
  Ast.field_number;
  Ast.field_label;
  Ast.field_type;
  Ast.field_options;
} as field_parsed) = 
  
  let field_type    = field_type_of_string field_type in 
  let field_default = get_default field_name field_options field_type in 
  {
    Astc.field_parsed;
    Astc.field_type;
    Astc.field_default;
  }

let compile_oneof_p1 ({
  Ast.oneof_name; 
  Ast.oneof_fields;
}) = {
  Astc.oneof_name; 
  Astc.oneof_fields = List.map compile_field_p1 oneof_fields; 
}
  
let not_found f : bool = 
  try f () ; false 
  with | Not_found -> true 

let rec list_assoc2 x = function
    [] -> raise Not_found
  | (a,b)::l -> if compare b x = 0 then a else list_assoc2 x l

let rec compile_message_p1 message_scope ({
  Ast.id;
  Ast.message_name; 
  Ast.message_body; 
})  = 
  
  let {Astc.message_names; _ } = message_scope in  
  let sub_scope = {message_scope with 
    Astc.message_names = message_names @ [ message_name] 
  } in 
  
  let message_body, all_sub = List.fold_left (fun (message_body, all_messages) -> function  
    | Ast.Message_field f -> 
        let sub = Astc.Message_field (compile_field_p1 f) in 
        (sub :: message_body, all_messages)
    | Ast.Message_oneof_field o -> 
        let sub = Astc.Message_oneof_field (compile_oneof_p1 o) in 
        (sub :: message_body, all_messages)
    | Ast.Message_sub m -> 
        let all_sub = compile_message_p1 sub_scope m in 
        (message_body,  all_messages @ all_sub)
  ) ([], []) message_body in
  
  let message_body = List.rev message_body in 
  
  (* Both field name and field number must be unique 
     within a message scope. This includes the field in a 
     oneof field inside the message. 

     This function verifies this constrain and raises
     the corresponding Duplicated_field_number exception in 
     case it is violated. 
  *)
  let validate_duplicate (number_index:(int*string) list) field = 
    let number = field_number field in 
    let name   = field_name   field in 
    if not_found (fun () -> ignore @@ List.assoc  number number_index) && 
       not_found (fun () -> ignore @@ list_assoc2 name   number_index)
    then 
      (number, name)::number_index
    else 
      raise @@ E.duplicated_field_number 
        ~field_name:name ~previous_field_name:"" ~message_name ()
  in
  ignore @@ List.fold_left (fun number_index -> function 
    | Astc.Message_field f -> validate_duplicate number_index f
    | Astc.Message_oneof_field {Astc.oneof_fields; _ } ->
       List.fold_left validate_duplicate number_index oneof_fields
  ) [] message_body ;  

  all_sub @ [ {
    Astc.id; 
    Astc.message_scope;
    Astc.message_name; 
    Astc.message_body;
  }] 

let find_all_message_in_field_scope messages scope = 
  List.filter (fun { Astc.message_scope = {Astc.namespaces; Astc.message_names} ;_ } -> 
    let dec_scope = namespaces @ message_names in 
    dec_scope = scope
  ) messages 

let compile_message_p2 messages ({
  Astc.message_name; 
  Astc.message_scope = {Astc.namespaces ; Astc.message_names; }; 
  Astc.message_body} as message)  = 

  (* stringify the message scope so that it can 
     be used with the field scope. 
     
     see `Note on scoping` in the README.md file
   *)
  let message_scope = namespaces @ message_names @ [message_name] in 

  let process_field_in_scope messages scope type_name = 
    let messages  = find_all_message_in_field_scope messages scope in 
    try 
      let {Astc.id ; _ }  =  List.find (fun {Astc.message_name; _ } -> 
        type_name = message_name 
      ) messages in
      Some id 
     with | Not_found -> None 
  in 

  (* this method returns all the scope to search for a type starting 
     by the most innner one first. 

     if [message_scope] = ['Msg1'; 'Msg2'] and [field_scope] = ['Msg3'] then 
     the following scopes will be returned:
     [
       ['Msg1'; 'Msg2'; 'Msg3'];  // This would be the scope of the current msg
       ['Msg1'; 'Msg2'; ];        // Outer message scope
       ['Msg1'; ]                 // Outer message scope 
       [ ]                        // Top level scope
     ]
  *)
  let search_scopes (field_scope:string list) from_root : (string list) list = 
    if from_root
    then [field_scope]
    else 
      let rec loop scopes = function
        | [] -> field_scope::scopes 
        | (_::tl as l) -> 
          loop ((List.rev l @ field_scope)::scopes) tl 
      in 
      List.rev @@ loop [] (List.rev message_scope) 
  in 
    
  let process_field_type message_name field = 
    let field_name = field_name field in 
    L.log "field_name: %s\n" field_name; 
    match field.Astc.field_type with 
    | Astc.Field_type_message ({Astc.scope; Astc.type_name; Astc.from_root} as unresolved) -> ( 
      L.endline @@ string_of_unresolved unresolved ; 
      
      let search_scopes = search_scopes scope from_root in 

      L.log "message scope: %s\n" @@ string_of_string_list message_scope;
      List.iteri (fun i scope -> 
        L.log "search_scope[%2i] : %s\n" i @@ string_of_string_list scope 
      ) search_scopes;

      let id  = List.fold_left (fun id scope -> 
        match id with 
        | Some _ -> id 
        | None   -> process_field_in_scope messages scope type_name
      ) None search_scopes in 
      
      match id with 
      | Some id -> (Astc.Field_type_message id:Astc.resolved Astc.field_type) 
      | None    -> 
        raise @@ E.unresolved_type ~field_name ~type_:type_name ~message_name () 
    )
    | field_type -> map_field_type field_type
  in

  let message_body = List.fold_left (fun message_body  -> function
    | Astc.Message_field field ->
      let field_type = process_field_type message_name field in 
      Astc.Message_field {field with Astc.field_type} :: message_body
    | Astc.Message_oneof_field ({Astc.oneof_fields; _ } as oneof )  -> 
      let oneof_fields = List.fold_left (fun oneof_fields field -> 
        let field_type = process_field_type message_name field in  
        {field with Astc.field_type }:: oneof_fields 
      ) [] oneof_fields in  
      let oneof_fields = List.rev oneof_fields in 
      Astc.Message_oneof_field {oneof with Astc.oneof_fields } :: message_body 
  ) [] message_body in 
  let message_body = List.rev message_body in 
  {message with Astc.message_body; }
