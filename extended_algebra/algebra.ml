type env_value = string * float

type env       = env_value list 

let get_value env value = 
  try 
    Some (List.assoc value env) 
  with Not_found -> None 

type exp = 
  | Float of float 
  | Plus  of exp * exp 
  | Times of exp * exp 
  | Var   of string

let rec string_of_exp a =
  let f = string_of_exp in 
  match a with 
  | Var   s      -> s
  | Float a      -> string_of_float a
  | Plus  (l, r) -> "(" ^ f l ^ "+" ^ (f r) ^ ")" 
  | Times (l, r) -> "(" ^ f l ^ "x" ^ (f r) ^ ")" 

let rec simplify env a = 
  match a with 
  | Plus (l, r) -> 
    (match (simplify env l), (simplify env r) with 
     | Float 0. ,  r       -> r
     | l        , Float 0. -> l 
     | Float l  , Float r  -> Float (l +. r)
     | l        , r        -> Plus  (l , r ))
  | Times (l, r) -> 
    (match simplify env l, simplify env r with 
     | Float 0. , r        -> Float 0.
     |  l       , Float 0. -> Float 0.
     | Float 1. , r        -> r
     |  l       , Float 1. -> l 
     | Float l  , Float r  -> Float (l *. r)
     | l        , r        -> Times (l, r  ))
  | Var x -> 
    (match get_value env x  with 
     | Some v -> Float v 
     | None   -> a )
  | Float _ -> a 

let rec collect_vars a = 
  match a with 
  | Float _     -> []
  | Plus (a, b) -> (collect_vars a) @ (collect_vars b)
  | Times(a, b) -> (collect_vars a) @ (collect_vars b) 
  | Var x       -> [x]

let rec eval env a : float = 
  match simplify env a with 
  | Float a       -> a 
  | _             -> 
    let l = collect_vars a in 
    let s = List.fold_left (fun s v -> s ^ v ^ ", ") "" l in 
    failwith ("Variables (" ^ s ^ ")" ^ "not bound to the env")


(**********************
 * Main code
 *
 * Test the above alegra
 **********************)

let () = 
  let a = Plus ((Plus (Float 1., Float 2.)), Float 4.) in 
  let a = Times (a, Float 3.) in 
  let a = Plus ( Var "y", a) in 
  let env = [("y", 120.); ] in
  Printf.printf "Amount: %s = %f \n" (string_of_exp a) (eval env a);


  let print_simplification a = 
    Printf.printf 
      "Amount: %s simplifies to %s \n" 
      (string_of_exp a) 
      (string_of_exp (simplify [] a ))
  in

  let a = Times ((Times (Float 1., Float 0.)), Float 4.) in 
  print_simplification a;
  let a = Times ((Times (Float 1., Float 0.)), Var "Y") in 
  print_simplification a;
  let a = Times ((Plus (Float 1., Float 0.)), Var "Y") in 
  print_simplification a;
