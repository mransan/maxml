

let at_least_once pa n = 
  
  let pac = 1. -. pa in 
  let rec f acc i = 
     match i with 
     |  (- 1) -> acc
     |  _     -> f (acc +. (pac ** (float_of_int i)*.pa)) (i - 1) 
  in

  f 0. (n - 1)

let () = 
  Printf.printf "P(A1) : %f\n" (at_least_once (1. /. 6. ) 4); 
  Printf.printf "P(A1) : %f\n" (at_least_once (1. /. 36.) 24); 

