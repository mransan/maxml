
type digit = | One | Zero 

type t = digit list 

let zero = []

let one  = [One] 

let string_of t = 
  List.fold_left (fun s -> function | One -> s ^ "1" | Zero -> s ^ "0") "" t 

let rec inc = function 
  | [] -> [One]
  | Zero::tl -> One::tl
  | One::tl -> Zero::(inc tl)

let rec dec = function 
  | [] -> failwith "Cannot go negative"
  | One::tl -> Zero::tl 
  | Zero::tl -> One::(dec tl)

let to_int t = 
  let d2i = function  | Zero -> 0 | One -> 1 in 
  fst @@ List.fold_left (fun (i, pow) d -> (i + (d2i d) * pow), pow * 2 ) (0, 1) t
  
