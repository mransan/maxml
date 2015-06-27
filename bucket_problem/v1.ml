(** Solver for the bucket problem : given 2 buckets of different size find the
 * sequence of action to measure a size =! than either bucket. 
 *
 * For instance:
 * Given a 9 and 5 liter buckets find how to measure 3 liter
 *)

(** Those are all the decision that one can do at any point in time *)
type decision = 
  | Pour_left   (** Poor the water from the right to the left bucket *) 
  | Pour_right  (** Poor the water from the left to the right bucket *)
  | Empty_left  (** Remove all water from the left bucket *)
  | Empty_right  (** Remove all water from thr right bucket *)
  | Fill_left   (** Fill entirely the left bucket *)
  | Fill_right  (** Fill entirely the right bucket *)

let string_of_decision = function 
  | Pour_left    -> "Pour_left"
  | Pour_right   -> "Pour_right"
  | Empty_left   -> "Empty_left"
  | Empty_right   -> "Empty_rigt"
  | Fill_left    -> "Fill_left"
  | Fill_right   -> "Fill_right"
    

(** all the possible decision that one can do at each state *)
let all_decisions =  [ 
  Pour_left ;
  Pour_right;
  Empty_left;
  Empty_right;
  Fill_left ;
  Fill_right ]

let string_of_decisions l = 
   let f s d = 
     s ^ ",\n" ^ (string_of_decision d) in 
   List.fold_left f "" l

(** Find wether a state has already been visited *)
let seen_before seach_space (bucket_state, _ ) = 
  List.exists (fun (state, _) -> state = bucket_state) seach_space

(** Check if the target size has been reached (ie if one of the 2 bucket
    contains the target size
 *)
let is_target ((left, right), _) target = 
  left = target || right = target 

(** Configuration of the buckets 
 *)
let left_size  = 60 
let right_size = 3

(** Given a decision and a state compute the next state
 *)
let apply_decision ((left, right), decisions) d =
  let left, right = match d with  
  | Pour_left -> 
    let max_fill = left_size - left in 
    let fill = min max_fill right in 
    (left + fill, right - fill)
  | Pour_right -> 
    let max_fill = right_size - right in 
    let fill = min max_fill left in 
    (left - fill, right + fill)
  | Empty_left -> (0, right)
  | Empty_right -> (left, 0)
  | Fill_left  -> (left_size, right)
  | Fill_right -> (left, right_size)
  in
  (left, right), d::decisions 


(** Breath first search algorithm to search for the target
 *)
let rec search search_space visited target : decision list option = 
  match search_space with  
  | []  -> None
  | ((_, decisions) as hd)::search_space -> 
    if is_target hd target 
    then Some decisions
    else
      let visited = hd::visited in 
      let f decisions decision = 
        match decisions with 
        | Some _ -> decisions
        | None   -> 
          let (_, decisions) as new_state = apply_decision hd decision in 
          if seen_before visited new_state 
          then search search_space visited target
          else search (search_space @ [new_state]) visited target 
      in
      let decisions = List.fold_left f None all_decisions in  
      decisions 
