

module Peano = struct 
  type z 
  type +'a s 
end 


module NList : sig 
  type (+'l,+'a) t 

  val nil  : (('m*'m), 'a) t 
  val cons : (('m*'n), 'a) t -> 'a -> (('m*'n Peano.s), 'a) t  

  val concat : 
    (('m*'n), 'a) t ->
    (('l*'m), 'a) t ->
    (('l*'n), 'a) t 

  val fold_left :
    ('b -> 'a -> 'b) -> 
    'b ->
    ('l, 'a) t ->
    'b 

end = struct 
  type (+'l, +'a) t = 'a list
  let nil = []
  let cons l x = x :: l
  let concat l r = l@r
  let fold_left f e0 l = List.fold_left f e0 l
end 

let l0 = NList.nil 
let l1 = NList.cons l0 0 
let l2 = NList.cons l1 1 
let l3 = NList.cons l2 2 

(*
let fold_i f e0 (b, e) = 
  let inc = if b>e then (-) else (+) in 
  let rec loop acc = function 
    | i when i = e -> f acc i 
    | i -> loop (f acc i) (inc i 1) 
  in 
  loop e0 b 

let l20 = fold_i (fun l i -> NList.cons l i) Nlist.nil (0, 19) 
*)

(*
let rec loop : (int -> ('l, 'a) NList.t) = function 
 | 0 -> NList.nil
 | i -> NList.cons (loop (i - 1)) i 
*)


let () = 
  Printf.printf "sum of l3 = %n \n" (NList.fold_left (fun x y -> x + y) 0 l3); 
  ()
