
type number = 
  | V_int of int 
  | V_float of float 

let string_of_number = function 
  | V_int i -> string_of_int i
  | V_float f -> string_of_float f;;


let eval_number op_int op_float = function 
  | (V_int i1, V_int i2) -> 
    V_int (op_int i1 i2)
  | (V_float f1, V_float f2) -> 
    V_float (op_float f1 f2)
  | (V_int i1, V_float f2) -> 
    V_float (op_float (float_of_int i1) f2)
  | (V_float f1, V_int i2) -> 
    V_float (op_float f1  (float_of_int i2)) 

type expression = 
  | V_number of number
  | V_add of expression * expression
  | V_minus of expression * expression

let number_of_int i =
  V_number (V_int i);;
let number_of_float f = 
  V_number (V_float f) ;;

let string_of_expression e = 
  let format_op f op e1 e2 = 
    " (" ^ (f e1) ^ " " ^ op ^ " " ^ (f e2) ^ ")"
  in
  let rec f = function 
    | V_number n -> string_of_number n
    | V_add (e1, e2) -> format_op f "+" e1 e2 
    | V_minus (e1, e2) -> format_op f "-" e1 e2 
  in
  f e;;

let eval e = 
  let p op1 op2 = function 
    | (V_number n1, V_number n2) -> 
      V_number (eval_number op1 op2 (n1, n2))
    | (_, _) -> 
      failwith "Programmatic error in simplication"
  in
  let rec f = function
    | V_number n -> V_number n
    | V_add (e1, e2) ->  p (+) (+.) (f e1, f e2) 
    | V_minus (e1, e2) -> p (-) (-.) (f e1, f e2) 
  in 
  f e;;


let test = V_add (number_of_float 1.0, number_of_float 2.) in  
Printf.printf "%s : %s \n " 
              (string_of_expression test) 
              (string_of_expression (eval test)) ;;

let test = V_add (number_of_int 1, number_of_int 2) in  
Printf.printf "%s : %s \n " 
              (string_of_expression test) 
              (string_of_expression (eval test)) ;;

let test = V_add (number_of_int 1, number_of_float 2.) in  
Printf.printf "%s : %s \n " 
              (string_of_expression test) 
              (string_of_expression (eval test)) ;;

let test = V_minus (number_of_int 1, number_of_float 2.) in  
Printf.printf "%s : %s \n " 
              (string_of_expression test) 
              (string_of_expression (eval test)) ;;
