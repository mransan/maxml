(**************************** 
 * Exercise for the Ocaml lab
 ****************************)


(* pretty print for float
 *)
let pp_float s f =
  print_string s;
  print_float f; 
  print_string "\n";;


exception Out_of_range;;


(* given a function f and a float a return the function f - a
 * f: float -> float
 * a: float value
 *)
let f_of f a =
  fun x -> f x -. a ;;

(* return the half value
 *)
let half a b = 
    (a +. b)/.2.0;;

let sign x =  
 if (x>0.0)
 then 1.0
 else (-.1.0);;

let abs x =
  (sign x) *. x;;


(* check withing a value is within tolerance of 0.0
 *)
let within_tolerance value tol =
  let half =  tol /. 2.0 in
  let abs_value = (abs value) in 
  (abs_value < half && abs_value > (-.half));; 


let f' f delta =
    fun x -> (f (x +. delta)  -. f x) /. delta;; 


let rec do_f_until f until x =
    let f_x = f x in 
    if(until f_x)
    then 
        x
    else
        do_f_until f until f_x;;

     
(* input type for the newton search
 * newton_value: current value for the search
 * newton_delta: delta value used in the f' computation
 * newton_tolerance: tolerance within which the search stop
 *)
type newton_input = {newton_value:float; 
                     newton_delta:float; 
                     newton_tolerance:float};;


(* perform a newton search for function f(x) = 0 
 * with newtonn input parameters
 *
 * f: function for which we want to solve for f = 0
 * binary_input: binary search parameters
 *)
let newton_search f i =
    let df = (f' f i.newton_delta) in 

    let until x  =
        (within_tolerance (f x) i.newton_tolerance) in
   
   (* TODO verify this *) 
    let rec impl x = 
        (x -. i.newton_tolerance *.  (f x) *.  (df x)) in 
    
    do_f_until impl until i.newton_value ;;


(* input type for the binary search
 * a: lower bound
 * b: upper bound
 * tol: tolerance
 *)
type binary_input = {binary_a: float; binary_b: float; binary_tol: float};;


(* perform a binary search for function f(x) = 0 
 * with binary_input parameters
 *
 * f: function for which we want to solve for f = 0
 * binary_inputg: binary search parameters
 *)
let rec binary_search f i = 
    let middle   = half i.binary_a i.binary_b in 
    let f_middle = f middle in
    
    if (within_tolerance f_middle i.binary_tol) 
    then
        middle
    else if(f_middle < 0.0) 
    then
        if(f i.binary_b < 0.0) 
        then 
            raise Out_of_range
        else
            binary_search f {binary_a=middle; 
                             binary_b=i.binary_b; 
                             binary_tol=i.binary_tol}
    else
        if(f i.binary_a > 0.0) 
        then 
            raise Out_of_range
        else
            binary_search f {binary_a=i.binary_a; 
                             binary_b=middle; 
                             binary_tol=i.binary_tol};;

type search_input = Binary of binary_input | Newton of newton_input;;

let search f input = 
  match input with 
  | Binary i -> binary_search f i
  | Newton i -> newton_search f i;;

(* binary_input constructor using default tolerance
 *)
let binary_input a b = 
    (Binary {binary_a=a; binary_b=b; binary_tol=0.00000001});; 

let newton_input value =
    (Newton {newton_value=value; 
             newton_delta=0.001; 
             newton_tolerance=0.00001});; 

(******************)
(****** Main ******)
(******************)

(* Sqaure function
 *)
let square x = 
    x *. x;;
(* Cube function
 *)
let cube x = 
    x *. x *. x;;


print_string("using binary search\n");;
let result = search (f_of square 4.0) (binary_input 0.0 10.0);;
pp_float "binary search of x^2 = 4 : " result;;
let result = search (f_of cube 27.0) (binary_input 0.0 10.0);;
pp_float "binary search of x^3 = 27 : " result;;

print_string("using newton search\n");;
let result = search (f_of square 4.0) (newton_input 1.0);;
pp_float "newton search of x^2 = 4 : " result;;
let result = search (f_of cube 27.0) (newton_input 1.0);;
pp_float "newton search of x^3 = 27 : " result;;
