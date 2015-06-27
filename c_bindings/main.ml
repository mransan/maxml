
let () = 
  let rec f = function 
    | 100000 -> ()
    | x      -> begin
      Printf.printf "3 + x = %d\n" (C_binding.add_int 3 x);
      f (x + 1); 
    end
  in  

  f 0;
