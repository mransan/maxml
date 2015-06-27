
module Util = struct 
  
  let fuzzy_compare x y  = 
    y < x +. 100. *. epsilon_float && 
    y > x -. 100. *. epsilon_float

  let string_of_l l string_of_a = 
    (List.fold_left (fun s e -> s^";"^(string_of_a e)) "" l)^"]"

  let run_tests tests = 
    List.iter (fun (s, t) -> Printf.printf "[%s] : %b \n" s (t ())) tests
end


module Binomial = struct 
  type s = float * int (* value * counter *)
  
  let string_of_s (n, c) = 
    Printf.sprintf "(%f, %d)" n c

  let next_s s d = 
    let f (e,c) s' = 
      let el = e +. d in 
      let er = e -. d in 
      match s' with
      | [] -> (el, c)::(er, c)::[] 
      | (el', c')::tl -> 
        if Util.fuzzy_compare er el'
        then (el, c)::(er, c+c')::tl
        else (el, c)::(er, c)::s'
    in
    List.fold_right f s []
  (* 
   *
   * [                  [ 
   *   S0  = x0, 1;        S'0 = x0 + d, 1 ; 
   *   S1  = x1, 1;  ->    S'1 = x0 - d, 1 ; 
   *   S2  = x2, 1;        ...
   * ]                     ...
   *                    ]
   *)
  
  let apply_nth_next s0 d nth = 
    let rec loop acc = function 
      | 0   -> acc 
      | ith -> loop (next_s acc d) (ith - 1) 
    in 
    loop (s0::[]) nth  

  let price payoff_f s = 
    let sum = List.fold_left (fun sum (_, c) -> sum + c) 0 s in 
    let sum = float_of_int sum in 
    let f p (v, c) = 
      p +. (float_of_int c /. sum) *. (payoff_f v)
    in 
    List.fold_left f 0. s 

end


  
  
let next_tests = 
  let open Binomial in 
  let s0 = (100., 1)::[] in 
  let next_s_test_0 () = 
    (next_s s0 1.) = [(101., 1); (99., 1); ] 
  in
  let next_s_test_1 () = 
    let x = (next_s (next_s s0 1.) 1.) in 
    x = [(102., 1); (100., 2);(98., 1); ] 
  in
  let next_s_test_2 () = 
    let x = apply_nth_next (100., 1) 1. 3 in 
    x = [ (103., 1); 
          (101., 3);
          (99., 3); 
          (97., 1); ]  
  in
  let next_s_test_3 () = 
    let x = apply_nth_next (100., 1) 1. 4 in 
    x = [ (104., 1); 
          (102., 4);
          (100., 6);
          (98. , 4); 
          (96. , 1); ]  
  in

  [
    "next s0", next_s_test_0; 
    "next s1", next_s_test_1; 
    "next s2", next_s_test_2; 
    "next s3", next_s_test_3; 
  ]

let fuzzy_tests = 

  let open Util in 
  [
    "fz 0    0"    , (fun () -> fuzzy_compare 0. 0.) ; 
    "fz 1    1"    , (fun () -> fuzzy_compare 1. 1.) ; 
    "fz 0    1"    , (fun () -> not (fuzzy_compare 0. 1.)); 
    "fz 0    eps"  , (fun () -> fuzzy_compare 0. epsilon_float); 
    "fz 0    -eps" , (fun () -> fuzzy_compare 0. (-. epsilon_float));  
    "fz 100. 100." , (fun () -> fuzzy_compare 100. (100. +. 1. -. 1.));
  ]

let () = 
  Printf.printf "---- Fuzzy compare ---- \n";
  Util.run_tests fuzzy_tests;
  Printf.printf "---- Next state ---- \n";
  Util.run_tests next_tests;

  let call x = max (x -. 100.) 0. in 
  let p = Binomial.price call (Binomial.apply_nth_next (100., 1) 1. 20) in
  Printf.printf "price = %f \n" p;

