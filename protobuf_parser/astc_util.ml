
let field_name {Astc.field_parsed; _ } = 
  let {Ast.field_name; _ } = field_parsed in 
  field_name 

let field_number {Astc.field_parsed = {Ast.field_number;_}; _ } = 
  field_number 

let unresolved_of_string s = 
  let rec loop i l = 
    try 
      let i' = String.index_from s i '.' in 
      let s' = String.sub s i (i' - i)  in 
      loop (i'+1) (if s' = "" then l else s'::l)  
    with Not_found -> (String.sub s i (String.length s - i) ):: l 
  in 
  let l = loop 0 [] in 
  match l with 
  | [] -> failwith "Programmatic error"
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
 | s  -> Astc.Field_type_unresolved (unresolved_of_string s) 

let compile_default constant = function  
  | Astc.Field_type_double 
  | Astc.Field_type_float -> (
    match constant with 
    | Ast.Constant_int i -> Ast.Constant_float (float_of_int i)
    | _ -> failwith "unsuported default 1"
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
    | _ -> failwith "unsuported default 2"
  )
  | Astc.Field_type_uint32 
  | Astc.Field_type_uint64 -> (
    match constant with 
    | Ast.Constant_int i -> if i >=0 
      then constant 
      else failwith "unsuported default 3"
    | _ -> failwith "unsuported default 4"
  )
  | Astc.Field_type_bool -> (
    match constant with 
    | Ast.Constant_bool _ -> constant
    | _ -> failwith "unsuported default 5"
  ) 
  | Astc.Field_type_string -> (
   match constant with 
   | Ast.Constant_string _ -> constant
   | _ -> failwith "unsuported default 6"
  ) 
  | Astc.Field_type_bytes -> failwith "unsuported default 7" 
  | Astc.Field_type_unresolved _ -> failwith "unsuported default 8" 

let get_default field_options field_type = 
  match List.assoc "default" field_options with
  | constant -> Some (compile_default constant field_type)
  | exception Not_found -> None 
  
let compile_field_p1 ({
  Ast.field_name;
  Ast.field_number;
  Ast.field_label;
  Ast.field_type;
  Ast.field_options;
} as field_parsed) = 
  
  let field_type = field_type_of_string field_type in 
  let field_default = get_default field_options field_type in 
  {
    Astc.field_parsed;
    Astc.field_type;
    Astc.field_default;
  }

let compile_oneof_field_p1 ({
  Ast.oneof_field_name;
  Ast.oneof_field_number;
  Ast.oneof_field_type;
  Ast.oneof_field_options;
} as oneof_field_parsed) = 
  
  let oneof_field_type = field_type_of_string oneof_field_type in 
  let oneof_field_default = get_default oneof_field_options oneof_field_type in 
  {
    Astc.oneof_field_parsed;
    Astc.oneof_field_type;
    Astc.oneof_field_default;
  }

let compile_oneof_p1 ({
  Ast.oneof_name; 
  Ast.oneof_fields;
}) = {
  Astc.oneof_name; 
  Astc.oneof_fields = List.map compile_oneof_field_p1 oneof_fields; 
}

let rec compile_message_p1 message_scope ({
  Ast.message_name; 
  Ast.body_content; 
}) (all_messages:Astc.message list) = 
  
  let sub_scope = message_scope @ [ Astc.Message_name message_name] in 
  
  let body_content, all_sub = List.fold_left (fun (body_content, all_messages) -> function  
    | Ast.Message_field f -> 
        let sub = Astc.Message_field (compile_field_p1 f) in 
        (sub :: body_content, all_messages)
    | Ast.Message_oneof_field o -> 
        let sub = Astc.Message_oneof_field (compile_oneof_p1 o) in 
        (sub :: body_content, all_messages)
    | Ast.Message_sub m -> 
        let all_sub = compile_message_p1 sub_scope m [] in 
        (body_content,  all_messages @ all_sub)
  ) ([], []) body_content in

  let body_content = List.rev body_content in 

  all_messages @ ({
    Astc.message_scope;
    Astc.message_name; 
    Astc.body_content;
  } :: all_sub) 

let find_all_message_in_field_scope messages scope = 
  List.filter (fun { Astc.message_scope;_ } -> 
    let dec_scope = List.map (function 
      | Astc.Namespace x -> x
      | Astc.Message_name x -> x
    ) message_scope in 
    dec_scope = scope
  ) messages 

type error = 
  | Unresolved_type of {
    field_name: string; 
    type_:string; 
    message_name:string 
  }

exception Compilation_error of error  

let unresolved_type field_name type_ message_name = raise (Compilation_error (Unresolved_type {
  field_name; type_; message_name
}))

let compile_message_p2 messages {
  Astc.message_name; 
  Astc.message_scope;
  Astc.body_content} = 

  (* stringify the message scope so that it can 
     be used with the field scope. 
     
     see `Note on scoping` in the README.md file
   *)
  let message_scope = List.map (function 
    | Astc.Namespace    x -> x
    | Astc.Message_name x -> x) message_scope in

  let process_field_in_scope messages scope type_name = 
    let messages  = find_all_message_in_field_scope messages scope in 
    List.exists (fun {Astc.message_name; _ } -> 
      type_name = message_name 
    ) messages 
  in 

  (* this method returns all the scope to search for a type starting 
     by the most innner one first. 

     if [message_scope] = ['Msg1'; 'Msg2'] and [field_scope] = ['Msg3'] then 
     the following scopes will be returned:
     [
       ['Msg1'; 'Msg2'; 'Msg3'];  // This would be the scope of the current msg
       ['Msg2'; 'Msg3'; ];        // Outer message scope
       ['Msg3'; ]                 // Top level scope
     ]
  *)
  let search_scopes (field_scope:string list) from_root : (string list) list = 
    if from_root
    then [field_scope]
    else 
      List.fold_right (fun (new_scope:string) all_scope_list -> 
        let last_scope = List.hd all_scope_list in 
        (new_scope::last_scope)::all_scope_list
      ) message_scope [field_scope]
  in 
    
  let process_field_type field_name message_name  = function 
    | Astc.Field_type_unresolved {Astc.scope; Astc.type_name; Astc.from_root} -> ( 
      let exist = List.fold_left (fun exist scope -> 
        let exist' = process_field_in_scope messages scope type_name in 
        exist || exist'
      ) false (search_scopes scope from_root) in 
      
      if exist
      then () 
      else unresolved_type field_name type_name message_name 
    )
    | _ -> ()
  in

  List.iter (function 
    | Astc.Message_field {Astc.field_type; Astc.field_parsed } -> 
      process_field_type field_parsed.Ast.field_name message_name field_type  
    | Astc.Message_oneof_field {Astc.oneof_fields; _ } -> 
      List.iter (fun {Astc.oneof_field_type;Astc.oneof_field_parsed} -> 
        process_field_type oneof_field_parsed.Ast.oneof_field_name message_name oneof_field_type
      ) oneof_fields 
  ) body_content
