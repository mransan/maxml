

(** Module signature for a domain to be solved by the Domain_solver.
 *
 * A Domain is essentially defined by all of its possible state and transition
 * between them that we will call decision. A domain must be able to have an
 * initial state the ability to apply a decision to go from one state to another
 * and finally to give all the possible decision for a given state. 
 *
 *)
module type Domain = 
  sig 
    type state
    type decision 
    
    val initial_state : unit -> state 
    val get_next_decisions : state -> decision list 
    val apply_decision : state -> decision -> state 
  end;;


(** Provide core functionality to solve any kind of domain. The current method
 * implemented is breadth first search. 
 *) 
module Domain_solver : 
  functor (D:Domain) -> sig 
    type decision = D.decision
    type state    = D.state 

    val search : (state -> bool) -> decision list option 
  end =  
  
  functor (D:Domain) -> struct
    
    type decision = D.decision
    type state    = D.state 
    type search_space = (D.state * (decision list)) list 

    let seen_before (search_space:search_space) ((state:D.state), _ ) = 
      List.exists (fun (seen_state, _) -> state = seen_state) search_space

    let rec search_impl 
      (search_space:search_space)
      (visited: search_space)  
      (is_target:(state -> bool)) : 
      decision list option = 

      match search_space with  
      | []  -> None
      | ((state, state_decisions) as hd)::search_space -> 
        if is_target state 
        then Some state_decisions
        else
          let visited = hd::visited in 
          let f decisions decision = 
            match decisions with 
            | Some _ -> decisions
            | None   -> 
              let new_state  = D.apply_decision state decision in 
              let new_state  = (new_state, decision::state_decisions) in 
              if seen_before visited new_state 
              then search_impl search_space visited is_target 
              else search_impl (search_space @ [new_state]) visited is_target 
          in
          let next_decisions = D.get_next_decisions state in 
          List.fold_left f None next_decisions 

    let search is_target  = 
      match search_impl [ (D.initial_state ()), []; ] [] is_target with 
      | None -> None 
      | Some l -> Some (List.rev l) 
  end;;

(** Module to parametrized the Bucket_domain module with abritrary size for 
 * the 2 buckets. 
 *)
module type Bucket_sizes  = 
  sig 
   val left_size  : int 
   val right_size : int
  end;;

(** The bucket domain implement the following problem : Given 2 buckets 
 * of different size find the sequence of action to measure a size different 
 * than either bucket. 
 *
 * For instance:
 * Given a 9 and 5 liter buckets find how to measure 3 liter
 *
 * In this domain the state are the level of water in each bucket and the
 * decision the ability to either fill, empty of transfer buckets. 
 *)
module Bucket_domain : 
  functor (Bs:Bucket_sizes) -> sig 
    type decision 
    val string_of_decision : decision -> string 
    val string_of_decisions : decision list -> string 
    
    type state

    val initial_state : unit -> state 
    val get_next_decisions : state -> decision list 
    val apply_decision : state -> decision -> state 
    val is_target : int -> state -> bool 

  end = 
  functor (Bs:Bucket_sizes) -> struct 

    type decision = 
      | Pour_left 
      | Pour_right
      | Empty_left
      | Empty_right
      | Fill_left 
      | Fill_right
    
    type state = int * int 

    let string_of_decision = function 
      | Pour_left    -> "Pour_left"
      | Pour_right   -> "Pour_right"
      | Empty_left   -> "Empty_left"
      | Empty_right   -> "Empty_rigt"
      | Fill_left    -> "Fill_left"
      | Fill_right   -> "Fill_right"
    
    let string_of_decisions l = 
       let f s d = 
         s ^ ",\n" ^ (string_of_decision d) in 
       List.fold_left f "" l


    let initial_state () = (0, 0) 

    let is_target target ((left, right) : state) =  
      left = target || right = target

    let get_next_decisions _ =  [ 
      Pour_left ;
      Pour_right;
      Empty_left;
      Empty_right;
      Fill_left ;
      Fill_right ]

    let apply_decision (left, right)  = function 
    | Pour_left -> 
      let max_fill = Bs.left_size - left in 
      let fill = min max_fill right in 
      (left + fill, right - fill)
    | Pour_right -> 
      let max_fill = Bs.right_size - right in 
      let fill = min max_fill left in 
      (left - fill, right + fill)
    | Empty_left -> (0, right)
    | Empty_right -> (left, 0)
    | Fill_left  -> (Bs.left_size, right)
    | Fill_right -> (left, Bs.right_size)
  end;;
