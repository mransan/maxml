
let pi = acos (-. 1.) 
let m2 = -. 2. 

module NormalRandom = struct 
  (** Box Muller Transformation in basic form: 
    http://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform
   *)
  let box_muller_transform_basic_form u1 u2 = 
    let z0 = sqrt (m2 *. (log u1)) *. cos (2. *. pi *. u2) in 
    let z1 = sqrt (m2 *. (log u1)) *. sin (2. *. pi *. u2) in 
    (z0, z1) 
end 

module Statistic = struct 


 let inc_moving_average (length, l, average)  new_element = 
   match l with 
   | [] -> (length, [new_element], new_element)
   | hd::tl -> 
     let ll = List.length tl in 
     let l', p, n = 
       if ll = (length - 1) 
       then tl, ll   , length  
       else l , ll+1 , ll+2 in 
     let sum = average *. (float_of_int p) +. new_element in 
     (length, l@[new_element], sum /. (float_of_int n))

  let variance_calc sum_of_x sum_of_x2 n = 
    let n = float_of_int n in 
    (sum_of_x2 -. (sum_of_x *. sum_of_x /. n)) /. (n -. 1.)  
  
  let variance_of_l l = 
    let f (x, x2, n) x' = x +. x', (x2 +. x'*.x'), n + 1 in 
    let x, x2, n = List.fold_left f (0., 0., 0) l in 
    variance_calc x x2 n 
end

let random_number_e, main_loop = 
  let open React in 
  let (e:float E.t), send = E.create () in 
  let _   = Random.self_init () in 
  let main_loop () = 
    while true
    do 
      send (Random.float 1.);
    done
  in
  e, main_loop 


let box_muller_transform_e = 
  let open React in 
  let f (u1, u2, v) x = 
   match u1, u2, v with 
   | None   , None, None -> 
     Some x , None, None 
   | Some u1, None, _ -> 
     let z0, z1 = NormalRandom.box_muller_transform_basic_form x u1 in 
     None   , None, Some (z0, z1) 
   | None   , None, Some (z0, z1) -> 
     Some x , None, Some (z1, z0) 
   | _ -> failwith "Programatic error"
  in

  let e = E.fold f (None, None, None) random_number_e in 
  let f (_, _, v) = 
    match v with 
    | None -> None
    | Some (x,y) -> Some y 
  in
  E.fmap f e 
  
let moving_average_e = 
  let open React in 
  let size =  500000000 in 
  let internal_e = 
    E.fold Statistic.inc_moving_average (size, [], 0.) box_muller_transform_e
  in
  E.map (fun (_, _, a) -> a ) internal_e 

let variance_e = 
  let open React in 
  let sample_size = 5000 in 
  let e = 
    let f (l, sum_of_x, sum_of_x2, n, v) x = 
      match l with 
      | [] -> [x], x, x*.x, 1, None 
      | hd::tl -> 
         if n = sample_size 
         then 
           let l = tl @ [x] in 
           let sum_of_x  = sum_of_x -. hd +. x in 
           let sum_of_x2 = sum_of_x2 -. (hd*.hd) +. (x*.x) in 
           let v = Statistic.variance_calc sum_of_x sum_of_x2 n in 
           l, sum_of_x, sum_of_x2, n, Some v 
         else 
           let l = x::l in 
           let n = n + 1 in 
           let sum_of_x = sum_of_x +. x in 
           let sum_of_x2 = sum_of_x2 +. (x*.x) in 
           l, sum_of_x, sum_of_x2, n , None 
    in  
    E.fold f ([], 0., 0., 0, None) box_muller_transform_e  in 
  E.fmap (fun (_, _, _, _, x) -> x) e 

  (*
let printer = 
  let open React in 
  E.map (fun n -> Printf.printf "%f\n" n ; flush stdout;) moving_average_e
  *)

let both_printer = 
  let open React in 
  let f x y = Printf.printf "(%f, %f)\n" x y; flush stdout; in 
  E.l2 f moving_average_e variance_e 

let () = main_loop () 
