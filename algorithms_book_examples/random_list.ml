

type 'a tree  = Leaf of 'a | Node of int * 'a tree * 'a tree 
type 'a digit = Zero | One of 'a tree 
type 'a t = 'a digit list 

let rec string_of_tree f = function 
  | Leaf x -> f x 
  | Node (_, x, y) -> "(" ^ (string_of_tree f x) ^ "|" ^ (string_of_tree f y) ^ ")"

let rec leftmost_leaf = function 
  | Leaf x -> x 
  | Node (_, x, y) -> leftmost_leaf x 

let string_of_digit f = function 
  | Zero  -> "Zero"
  | One t -> "One (" ^ (string_of_tree f t) ^ ")" 

let string_of_list f l = 
  "[" ^ (List.fold_right (fun x s -> (string_of_digit f x) ^ ";" ^ s) l "") ^ "]" 

let tree_of x y = 
  match x, y with 
  | Leaf _, Leaf _ -> Node (2, x, y) 
  | Node (l, _, _), Node (r, _, _) -> Node ((l + r), x, y)
  | _ -> failwith "Programatic error"

let create () : 'a t = 
  []
  
let push_front a l = 
  let rec loop a = function 
    | []          -> [One a]
    | Zero::tl    -> (One a)::tl 
    | (One b)::tl -> Zero::(loop (tree_of a b) tl) 
  in
  let a = Leaf a in loop a l  

let rec hd = function 
  | [] -> failwith "Empty list"
  | Zero::tl -> hd tl 
  | (One t)::tl -> leftmost_leaf t      

let rec find_nth_in_tree n = function 
  | Leaf x when n = 0 -> Some x, 0 
  | Leaf _ -> None, 1 
  | Node (l, x, y) -> 
    if n >= l 
    then None, l 
    else
      let v = 
        let l' = l / 2 in 
        if n < l' 
        then find_nth_in_tree n x 
        else find_nth_in_tree (n - l') y
      in 
      match v with 
      | Some x, 0 -> v 
      | _ -> failwith "Programatic Error"

let rec nth n =  function 
    | []         -> failwith "Overflow"
    | Zero::tl   -> nth n tl 
    | (One d)::tl-> (
      match find_nth_in_tree n d with 
      | Some x, _ -> x 
      | None  , l -> nth (n - l) tl  
    )

let rec borrow = function 
  | []          -> failwith "Empty" 
  | (One t)::tl -> t, Zero::tl 
  | Zero::tl    -> (
    match borrow tl with
    | Leaf _ , tl -> failwith "Programatic Error"
    | Node (l, x, y), tl -> x, (One y)::tl 
  )

let pop_front l = 
  snd @@ borrow l  
