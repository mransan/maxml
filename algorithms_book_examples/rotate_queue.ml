

type 'a t = {
  f: 'a list; 
  r: 'a list; 
  s: 'a list; 
}


let create () = {
    f = []; r = []; s = [] 
} 

let empty = function 
  | {f=[]; r=[]; s=[]} -> true 
  | _ -> false 

let rec rotate f r a = match f, r with 
  | []   , hd::[] -> hd::a 
  | x::f', y::r' -> x::(rotate f' r' (y::a))
  | _ -> failwith "Invariant error"
  
let queue = function 
 | {f;r;s = hd::s} as q -> {q with s}
 | {f;r;s = []   } -> 
   let f' = rotate f r [] in 
   {f=f'; r=[]; s=f'} 

let push_back ({r;_} as q)   a =
  queue {q with r=a::r} 

let pop_front = function 
  | {f = []; _} as q -> None, q 
  | {f = hd::tl; _ } as q -> Some hd, (queue {q with f = tl})

let string_of {f;r;s} string_of_a = 
  "\n"   ^ 
  "{ \n" ^ 
  "  f = " ^ (Util.string_of_l f string_of_a) ^ " ; \n" ^  
  "  r = " ^ (Util.string_of_l r string_of_a) ^ " ; \n" ^  
  "  s = " ^ (Util.string_of_l s string_of_a) ^ " ; \n" ^  
  "} \n" 

