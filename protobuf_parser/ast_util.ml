
let field ?options:(options =[]) ~label ~number ~type_ name = {
  Ast.field_name = name; 
  Ast.field_number = number;
  Ast.field_type = type_; 
  Ast.field_label = label;
  Ast.field_options = options;
} 

let oneof_field ?options:(options =[]) ~number ~type_ name = {
  Ast.oneof_field_name = name; 
  Ast.oneof_field_number = number;
  Ast.oneof_field_type = type_; 
  Ast.oneof_field_options = options;
} 

let oneof ~fields name = {
  Ast.oneof_name = name;
  Ast.oneof_fields = fields;
}

let message_body_field field =  Ast.Message_field field  
let message_body_oneof_field field =  Ast.Message_oneof_field   field  
let message_body_sub message  =  Ast.Message_sub message 

let message ~content name = {
  Ast.message_name = name; 
  body_content = content;
} 
