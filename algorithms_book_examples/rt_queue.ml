

type 'a t = {
  f: 'a Lazy_list.t; 
  r: 'a list; 
  s: 'a Lazy_list.t; 
}

let create () = {
    f = Lazy_list.create(); 
    r = []; 
    s = Lazy_list.create(); 
} 

let empty {f; r; s; } = 
  match Lazy.force f , r , Lazy.force s with 
  | Lazy_list.Nil, [], Lazy_list.Nil -> true 
  | _ -> false

let rec rotate f r a = 
  let open Lazy_list in 
  match Lazy.force f, r with 
  | Nil , hd::[] -> lazy (Cons (hd, a)) 
  | Cons(x, f') , y::r' -> lazy (Cons (x, (rotate f' r' (lazy (Cons (y,a)))))) 
  | _ -> failwith "Invariant error"
  
let queue ({f;r;s} as q) =  
  let open Lazy_list in 
  match Lazy.force s with 
  | Cons (hd, s) -> {q with s}
  | Nil -> 
    let f' = rotate f r (Lazy_list.create ()) in 
    {f=f'; r=[]; s=f'} 

let push_back ({r;_} as q) a =
  queue {q with r=a::r} 

let pop_front ({f;_ } as q) = 
  match Lazy.force f with 
  | Lazy_list.Nil -> None, q 
  | Lazy_list.Cons (hd, tl) -> Some hd, (queue {q with f = tl})

(*
let string_of {f;r;s} string_of_a = 
  "\n"   ^ 
  "{ \n" ^ 
  "  f = " ^ (Util.string_of_l f string_of_a) ^ " ; \n" ^  
  "  r = " ^ (Util.string_of_l r string_of_a) ^ " ; \n" ^  
  "  s = " ^ (Util.string_of_l s string_of_a) ^ " ; \n" ^  
  "} \n" 
*)
