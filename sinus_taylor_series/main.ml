
(** Taylor serie approximation of the sine function. [sine x 11] will compute
    the sine(x) value with a polynomial of degree n 

    Note that this function will not work (infinite loop) when n is an even
    number

    The implemetation relies on the fact that both the nominator/denominator
    of each polynomial component can be computed in terms of the previous one.
    This enable faster computation than using factorial and power functions. 
 *)
let sine x n = 
  let rec loop (sum, sign, nom, den) i = 
    if i = n 
    then sum 
    else 
      let i' = float_of_int i in 
      let sign = (-. 1.) *. sign in 
      let nom  = nom *. x *. x in  
      let den  = den *. (i' +. 1.) *. (i' +. 2.) in 
      let sum  = sum +. (sign *. nom /. den) in 
      loop (sum, sign, nom, den) (i + 2) 
  in 
  loop (x, 1., x, 1.) 1 


(** [test x] test sine(x) function by comparing it to the native ocaml sin
    function in the Pervasives module. It displays the error in % for various
    polynomial degrees. 
 *) 
let test x = 
  let cases    = [3; 5; 11; 51; 111; ] in 
  let report   = 
    let f report n = 
      let t_value = sine x n in 
      let o_value  = sin x in 
      let error = 100. *. abs_float ((o_value -. t_value) /. o_value) in 
      (Printf.sprintf "%15.2f  " error)::report
    in 
    List.fold_left f [] cases in 
  Printf.printf "%5.2f = %s \n" x (String.concat "|" (List.rev report)) 


(** Main test function *)
let () = 
  let rec loop i = 
    match i with 
    | 100000 -> () 
    | _  -> test (float_of_int i *. 0.2) ; loop (i + 1)  
  in 
  loop 1
