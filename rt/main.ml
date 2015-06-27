

let () = 

  let rec f = function 
    | 10 -> () 
    | i  -> 
      let v = Rt_ml_binding.get_t1_value i  in 
      let s = Rt_ml_binding.string_of_t1 v in 
      Printf.printf "T1 %s \n"s ;
      f (i + 1) 
  in
  f 0;;

let () = 

  let rec f = function 
    | 10 -> () 
    | i  -> 
      begin 
        let p = Rt_ml_binding.t2_create i  in 
        let s = Rt_ml_binding.string_of_t2 p in 
        Rt_ml_binding.t2_destroy p; 
        Printf.printf "T2 %s \n"s ;
        f (i + 1) 
      end
  in
  f 0;;


let () = 

  let rec f = function 
    | 10 -> () 
    | i  -> 
      begin 
        let p = Rt_ml_binding.t3_create i  in 
        let s = Rt_ml_binding.string_of_t3 p in 
        Rt_ml_binding.t3_destroy p; 
        Printf.printf "T3 %s \n"s ;
        f (i + 1) 
      end
  in
  f 0;;

let function_stack_limit = 1

let rec build_f l f i = 
  if i = l 
  then f 
  else build_f l (fun x -> f x) (i + 1) 

let () = 

  let rec f = function 
    | 10 -> () 
    | i  -> 
      begin 
        let p = Rt_ml_binding.t4_create i  in 
        let f' = build_f function_stack_limit (fun p -> 
          let _ = Rt_ml_binding.string_of_t4 p in 
          (*Printf.printf "T4 %s \n"s ;
           *)
          ()
        ) 0 in 
        f' p; 
        f (i + 1) 
      end
  in
  f 10;;
