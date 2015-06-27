
exception Programmatic_error 

type 'a t = { 
  a : 'a array ;  
  s : int ref;  
}

let init s root = {
  a = Array.make s root; 
  s = ref 1;
}

type 'a e = int * 'a  

let value_of_e ((_:int), v) = v  

let root {a; s=_ }  = (0, Array.get a 0) 

let parent_of_e {a; s} (i, _) = 
  if i >  !s || i = 0 
  then raise Programmatic_error
  else
    let pi = ((i+1)/2) - 1 in 
    (pi, Array.get a pi)

let left_child_of_e {a;s} (i, _) = 
  let lci = (i+1)*2 - 1 in 
  if lci > !s
  then None 
  else Some (lci, Array.get a lci)

let right_child_of_e {a;s} (i, _) = 
  let rci = (i+1)*2  in 
  if rci > !s
  then None 
  else Some (rci, Array.get a rci)

let insert ({a;s} as t)  v = 
  Array.set a !s v ; 
  let rec loop = function
    | (0, _) -> ()
    | (i, v) as e -> 
      let (i', v') as p = parent_of_e t e in 
      if v > v' 
      then begin Array.set a i' v; Array.set a i v'; loop (i', v) end 
      else () 
  in  
  loop (!s, v) ; 
  s := !s +1 

let length {a=_; s} = !s

let string_of_t {a; s=_} string_of_a = 
  let f s e = s ^ "; " ^ (string_of_a e) in 
  (Array.fold_left f "[" a ) ^ "]"
