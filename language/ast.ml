(* Ast type *)
type exp = 
  | Float of float 
  | Plus  of exp * exp 
  | Minus of exp * exp 
  | Times of exp * exp 
  | Div   of exp * exp 
  | Var   of string
  | Let   of string * exp * exp (*let s = exp in exp *)


(* pretty print of an expression *)
let rec string_of_exp a =
  let f = string_of_exp in 
  match a with 
  | Var   s      -> s
  | Float a      -> string_of_float a
  | Plus  (l, r) -> "(" ^ f l ^ " + " ^ (f r) ^ ")" 
  | Minus (l, r) -> "(" ^ f l ^ " - " ^ (f r) ^ ")" 
  | Times (l, r) -> "(" ^ f l ^ " * " ^ (f r) ^ ")" 
  | Div   (l, r) -> "(" ^ f l ^ " / " ^ (f r) ^ ")" 
  | Let   (s, eq_e1, in_e2) -> "(let " ^ s ^ " = " ^ (f eq_e1) ^ " in " ^ (f in_e2) ^ ")" 


(* environment used in the simplification process 
 * associate string with expression
 *)
type env_value = string * exp 
type env       = env_value list 

let get_value env value = 
  try 
    Some (List.assoc value env) 
  with Not_found -> None 

(* reduce the expression recursively 
 *
 * One interesting note about the simplification of var and let construct. 
 * Should one simplify the variable when the let expression is found and
 * inserted in the environment or should it be evaluated when being used? 
 *
 * Right now we are doing the simplification in the let construct
 * (see the JS var construct which is probably done the other way)
 *)
let rec simplify env a = 
  match a with 
  | Plus (l, r) -> 
    (match (simplify env l), (simplify env r) with 
     | Float 0. ,  r       -> r
     | l        , Float 0. -> l 
     | Float l  , Float r  -> Float (l +. r)
     | l        , r        -> Plus  (l , r ))
  | Minus (l, r) -> 
    (match (simplify env l), (simplify env r) with 
     | Float 0. ,  Float r -> Float (-. r)
     | l        , Float 0. -> l 
     | Float l  , Float r  -> Float (l -. r)
     | l        , r        -> Minus (l , r ))
  | Times (l, r) -> 
    (match simplify env l, simplify env r with 
     | Float 0. , r        -> Float 0.
     |  l       , Float 0. -> Float 0.
     | Float 1. , r        -> r
     |  l       , Float 1. -> l 
     | Float l  , Float r  -> Float (l *. r)
     | l        , r        -> Times (l, r  ))
  | Div (l, r) -> 
    (match simplify env l, simplify env r with 
     | Float 0. , r        -> Float 0.
     |  l       , Float 0. -> failwith "Error divided by 0."
     |  l       , Float 1. -> l 
     | Float l  , Float r  -> Float (l /. r)
     | l        , r        -> Minus (l, r  ))
  | Var x -> 
    (match get_value env x  with 
     | Some v -> v (* no need to simplify since this was done in the Let ctor *)
     | None   -> a )
  | Let (s, eq_e1, in_e2) -> 
    let env = (s, (simplify env eq_e1))::env in 
    simplify env in_e2
  | Float _ -> a 

let rec collect_vars a = 
  match a with 
  | Float _     -> []
  | Plus  (a, b) -> (collect_vars a) @ (collect_vars b)
  | Minus (a, b) -> (collect_vars a) @ (collect_vars b)
  | Times (a, b) -> (collect_vars a) @ (collect_vars b) 
  | Div   (a, b) -> (collect_vars a) @ (collect_vars b) 
  | Var x        -> [x]
  | Let (s, eq_e1, in_e2) -> (collect_vars eq_e1) @ (collect_vars in_e2)

let rec eval env a : float = 
  match simplify env a with 
  | Float a       -> a 
  | _             -> 
    let l = collect_vars a in 
    let s = List.fold_left (fun s v -> s ^ v ^ ", ") "" l in 
    failwith ("Variables (" ^ s ^ ")" ^ "not bound to the env")
