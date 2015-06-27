

(* Example of a function which takes arguments
 * but return no values. 
 * 
 * let function_name arg1 arg2 arg3 = 
 *   do_something_1;
 *   do_something_2;; 
 *)

let printi n = 
    print_string("int: ");
    print_int(n);
    print_string("\n");;

let printf n = 
    print_string("float: ");
    print_float(n);
    print_string("\n");;

(* Example of arithmetic operation on int or float
 *)
let int_v1 = 1 * 4;;
printi(int_v1);;

let float_v1 = 1.0 *. sin(1.3);;
printf(float_v1);;

(* Example of arithmetic functions
 *)
let square x = x *. x ;;
print_string("square(2.0) = ");;
printf(square(2.0));;

(* Example of a function taking a function an argument
 *)
let apply_f_to_times2 f x = f(2.0 *. x);;

(* Example of a function returning a function
 *)
let get_times_a a = fun x -> x *. a;;
let times_3 = get_times_a 3.0;;
printf(times_3 4.0);;
printf(times_3 5.0);;

(* Example of a function taking functions as arguments 
 * and returning a function
 *)
let compose f g  = fun x -> f(g(x));;

(** Example of recursive functions
 *)
let rec time f n x =
    if(n == 1) then 
        f(x)
    else    
        f(x) * time(f)(n-1)(x);;
let square x = time(fun a -> a)(2)(x);;
let cube   x = time(fun a -> a)(3)(x);;
printi(square(5));;
printi(cube(3));;

(* Example - more complex of recursive function
 *)
let rec power f n =
    if(n <=0) then
        fun x -> x;
    else
        compose f (power(f)(n-1));;


(*
let mul x y = 
    x * y;;
let mul_by_x a = 
    mul(x)(a) in 
let cube x =
    power(mul_by_x)(3)(1);;

print_string("cube power: ");;
printi(cube(4));;
*)


(* Example of  the in construction where one can locally declare
 * functions or variables
 *)
let x1 = 1.0 in
let x2 = 3.4 +. x1 in 
    printf(x1 *. x2);;

(* Example of list usage
 *)
let rec sort lst = 
    match lst with
    [] -> []
    | head::tail -> insert head(sort tail)
and insert elt lst = 
    match lst with 
    [] -> [elt]
    | head::tail -> if elt <= head then elt::lst else head:: insert elt tail
;;

(* Example of a recursive function applied to a list.
 *)
let rec sum_f_of_list f lst = 
    match lst with
    [] -> 0.0
    |head::tail -> f(head) +. sum_f_of_list f tail
;; 
let the_times2_sum = sum_f_of_list(fun x -> 2.0 *. x)([1.0; 2.0; 3.0]);;
printf(the_times2_sum)


(* Example of record type 
 *)
type strike_abs = {abs_value: float };;
type strike_atm = {atm_value: float };;

(*
type strike = Abs of strike_abs| Atm of strike_atm;;

let strike_val s spot = 
    match s with 
    (Abs v) -> v.abs_value
   |(Atm v) -> v.atm_value *. spot;;

printf(strike_val (Abs {abs_value=1.25}) 1.35);;
printf(strike_val (Atm {atm_value=1.25}) 1.35);;
*)

(* Example of a variant type which uses basic types
 *)
type strike = Abs of float | Atm of float ;;

(* Example of a function which processes a variant type
 * using the match ... with keywords
 *)
let strike_val s spot = 
    match s with 
    (Abs v) -> v
   |(Atm v) -> v *. spot;;

printf(strike_val (Abs 1.25) 1.35);;
printf(strike_val (Atm 0.91) 1.35);;

(* Example of a mutable variable which can be modifiied
 *)
type the_result = {mutable value: float };;
let  sum_till_i i = 
    let res = {value=0.0} in 
    for iter=0 to i do
        res.value <- res.value +. float_of_int(iter)
    done;
    res;;

let sum_til_10 = sum_till_i 10;;
print_string("sum till 10: ");;
printf(sum_til_10.value);;

(* Extending on the previous example we are summing an 
 * array this time
 *)
let sum_array a = 
    let res = {value=0.0} in 
    let len = Array.length(a) in 
    for i=0 to len - 1 do 
        res.value <- res.value +. a.(i) 
    done;
    res;;
let sum  = sum_array([|1.0; 2.0; 3.0 |]);;
printf(sum.value);;
1
