
  let module M = (struct 
      type 'a t = 'a ref 
      let create x = ref x 
      let apply x f = ignore @@ f !x 
  end: sig 
      type 'a t 
      val create : 'a -> 'a t
      val apply  : 'a t -> ('a -> 'b) -> unit   
  end ) in  

  let (i:[> `A of int] M.t )  = M.create (`A 1) in 
  let f j  = M.apply j (fun x  -> match x with 
    | `A _ -> () 
    | `B _ -> () ) in 
  f i ; 
  let g j = M.apply j (fun i -> match i with 
    | `A _ -> () 
    | `C _ -> ()) 
  in 
  g i; 
