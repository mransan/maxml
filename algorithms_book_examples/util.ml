
let string_of_l l string_of_a = 
  (List.fold_left (fun s e -> s^";"^(string_of_a e)) "[" l)^"]"

let string_of_int_list (l:int list) = 
  string_of_l l (string_of_int)

let init_list (n:int) =
 let rec loop l = function 
   | 0 -> l
   | x -> 
     let x = x - 1 in 
     loop (x::l) x 
 in 
 loop [] n 
(** initialize a list of integer in descending order. 
    [init_list 10] will return [0; 1; 2; ... 9 ]
 *)

let init_random_list (n:int) =
 let rec loop l = function 
   | 0 -> l
   | x -> 
     let x = x - 1 in 
     loop ((Random.int 100)::l) x 
 in 
 loop [] n 
(** initialize a list of random integer between 0 and 100
 *)

let fold_i f e0 (b, e) = 
  let inc = if b>e then (-) else (+) in 
  let rec loop acc = function 
    | i when i = e -> f acc i 
    | i -> loop (f acc i) (inc i 1) 
  in 
  loop e0 b 
(** [fold_i (fun sum i -> sum + i) 0 (0 20)] iterate from 
    b to e while accumulating in a fold_left fashion
 *)

let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

let option_value = function | Some v -> v | None -> raise Not_found 
let is_none = function | Some _ -> false | None -> true
let is_some = function | Some _ -> true | None -> false

let print_test_banner s =  
  let l  = String.length s in 
  let s1 = String.make 5 '-' in 
  let s2 = String.make 5 ' ' in 
  let s3 = String.make (30 - l) ' ' in 
  print_string ( (String.make 45 '-') ^ "\n");
  print_string (s1 ^ s2 ^ s ^ s3 ^ s1 ^ "\n"); 
  print_string ( (String.make 45 '-') ^ "\n");;

let print_test t b = 
  let l  = String.length t in 
  let s1 = String.make (35 - l) ' ' in 
  Printf.printf "(%s) %s : %b\n" t s1 b;;
