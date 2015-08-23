
let field ?options:(options =[]) ~label ~number ~type_ name = {
  Ast.field_name = name; 
  Ast.field_number = number;
  Ast.field_type = type_; 
  Ast.field_label = label;
  Ast.field_options = options;
} 

let oneof_field ?options:(options =[]) ~number ~type_ name = {
  Ast.field_name = name; 
  Ast.field_number = number;
  Ast.field_type = type_; 
  Ast.field_options = options;
  Ast.field_label = `Oneof
} 

let oneof ~fields name = {
  Ast.oneof_name = name;
  Ast.oneof_fields = fields;
}

let message_counter = ref 0

let enum_value ~int_value name = { 
  Ast.enum_value_name = name; 
  Ast.enum_value_int = int_value; 
}

let enum ?enum_values:(enum_values = []) enum_name =
  incr message_counter; 
  {
    Ast.enum_id  = !message_counter; 
    Ast.enum_name; 
    Ast.enum_values; 
  } 

let message_body_field field =  Ast.Message_field field  
let message_body_oneof_field field =  Ast.Message_oneof_field   field  
let message_body_sub message  =  Ast.Message_sub message 
let message_body_enum enum = Ast.Message_enum enum


let message ~content name = 
  incr message_counter;
  {
    Ast.id = !message_counter;
    Ast.message_name = name; 
    message_body = content;
  } 

let proto ?package messages = {
  Ast.package; 
  Ast.messages; 
}

let rec message_printer ?level:(level = 0) {
  Ast.message_name; 
  Ast.message_body; _ } = 

  let prefix () = 
    for _ =0 to level  - 1 do 
      Printf.printf " ";
    done; 
  in 
  prefix (); print_endline message_name; 
  List.iter (function 
    | Ast.Message_field {Ast.field_name; _ } -> 
        prefix (); Printf.printf "- field [%s]\n" field_name
    | Ast.Message_oneof_field {Ast.oneof_name ; _ } ->
        prefix (); Printf.printf "- one of field [%s]\n" oneof_name
    | Ast.Message_enum {Ast.enum_name; _ } ->
        prefix (); Printf.printf "- enum type [%s]\n" enum_name 
    | Ast.Message_sub m -> message_printer ~level:(level + 2) m
  ) message_body 
