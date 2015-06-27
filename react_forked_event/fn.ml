
let run (f1:(unit -> 'a)) (f2:('a -> 'b -> 'c)) : ('b -> 'c)  = 
  let rec f = ref (fun x -> 
      let s = f1 () in 
      f := (fun x -> 
          f2 s x); 
      !f x 
  ) in 
 
  fun x -> !f x 
