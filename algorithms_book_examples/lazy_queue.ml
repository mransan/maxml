



type 'a t = { 
  fl : int ; 
  w  : 'a list; 
  f  : 'a list Lazy.t; 
  rl : int ; 
  r  : 'a list; 
}

let create () = 
  { fl = 0; w = []; f = lazy []; rl = 0; r = [] ; } 


let empty {fl; rl; _} = 
  fl = 0 && rl = 0 

let enforce q = 
  let check_w ({w; f; _ } as q)  = 
    match w with 
    | [] -> {q with w = Lazy.force f } 
    | _ -> q 
  in 

  let check_r ({fl; w; f; rl; r} as q) = 
    if fl >= rl 
    then q 
    else {
      fl = fl+rl; 
      w; 
      f = lazy (Lazy.force f @ (List.rev r)); 
      rl = 0;
      r = []; 
    }

  in
  check_w @@ check_r q 

let push_back ({fl; w; f; rl; r} as q)   a = 
  enforce {q with r = a::r ; rl = rl + 1} 

let pop_front = function 
  | {w = [] ; r = [] ; _ } as q -> None, q
  | {w = hd::tl; f; fl;   _ } as q -> Some hd, enforce {q with w=tl;fl=fl-1;f = lazy (List.tl @@ Lazy.force f); }  
  | _ -> failwith "Invariant error"
  



