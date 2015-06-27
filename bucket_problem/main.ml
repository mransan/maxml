

(*
let () = 
 let open V1 in 
 let rec f i = 
     if i <= (max left_size right_size)
     then 
       begin 
         let result = search [ ((0, 0), []) ; ]  [] i in 
         begin 
         match result with 
         | None -> 
            Printf.printf "[%d] No results found \n" i 
         | Some v -> 
            Printf.printf "[%d] Solution found: \n%s\n" i (string_of_decisions (List.rev v))
         end;
         f (i + 1)
       end
 in 
 f 0;;
*)



let () =  
  let open V2 in 
  let x = 9 in 


  let module Bucket_size  = struct 
    let left_size = x
    let right_size = 5 
  end in 
  let module Bucket_domain  = Bucket_domain(Bucket_size) in 
  let module Bucket_solver  = Domain_solver(Bucket_domain) in 
  let result = Bucket_solver.search (Bucket_domain.is_target 4)  in 
  match result with 
  | None -> 
     Printf.printf "[%d] No results found \n" 3
  | Some v -> 
     Printf.printf "[%d] Solution found: \n%s\n" 3 (Bucket_domain.string_of_decisions v);;
  
  
