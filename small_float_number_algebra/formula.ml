

type expression = 
  | V_float of float 
  | V_add of expression * expression
  | V_minus of expression * expression


let string_of_expression e = 
  let format_op f op e1 e2 = 
    " (" ^ (f e1) ^ " " ^ op ^ " " ^ (f e2) ^ ")"
  in
  let rec f = function 
    | V_float v -> string_of_float v 
    | V_add (e1, e2) -> format_op f "+" e1 e2 
    | V_minus (e1, e2) -> format_op f "-" e1 e2 
  in
  f e;;

let eval e = 
  let rec f = function
    | V_float v -> v 
    | V_add (e1, e2) -> f e1 +. (f e2) 
    | V_minus (e1, e2) -> f e1 -. (f e2) in
  f e;;


let test1 = V_add (V_float 1.0, V_float 2.) in  
Printf.printf "%s : %f \n " 
              (string_of_expression test1) 
              (eval test1) ;;



