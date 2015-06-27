
type 'a t'  = 
  | Nil 
  | Cons of 'a * 'a t

and 

'a t = 'a t' Lazy.t  

let create () = 
  lazy Nil

let empty l = 
  match Lazy.force l with 
  | Nil -> true
  | Cons _ -> false 

let push_front a l = 
  lazy (Cons (a, l))

let make i a = 
  let rec loop l = function 
    | 0 -> l 
    | i -> loop (push_front a l) (i - 1) 
  in 
  loop (create ()) i 

let rec append l1 l2 = 
  match Lazy.force l1 with 
  | Nil  -> l2 
  | Cons (a, l) -> lazy (Cons (a, (append l l2))) 

let fold_left f e0 l = 
  let rec loop acc e = 
    match Lazy.force e with 
    | Nil ->  acc 
    | Cons (a, l) ->  loop (f acc a) l 
  in 
  loop e0 l 
