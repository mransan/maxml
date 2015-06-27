
let insertion_sort l = 

  let f final_l e = 
    let f' (intermediate_l, e_inserted) e' = 
      if e < e' && not e_inserted
      then e'::e::intermediate_l, true
      else e'::intermediate_l, e_inserted
    in
    let l, e_inserted = List.fold_left f' ([], false) final_l in 
    if not e_inserted
    then List.rev (e::l)
    else List.rev l
  in 
  List.fold_left f [] l 


module Merge_sort_detail = struct 

  let merge_sorted_list l r = 
    let rec loop l' = function 
      | [], [] -> l' 
      | (lhd::ltl as l, (rhd::rtl as r)) -> 
         (** Note of the pattern match for as... in this 
             case the second as will try to apply it to the tuple 
             itself rather than on the second tuple argument..
             
             As Alain mentioned 'as' has a little tricky priority
             *)
         if lhd < rhd 
         then loop (lhd::l') (ltl, r)
         else loop (rhd::l') (l  , rtl)
      | lhd::ltl, ([] as r) -> loop (lhd::l') (ltl,r) 
      | ([] as l) , rhd::rtl -> loop (rhd::l') (l, rtl)
    in
    List.rev (loop [] (l, r))
  
  let merge_init l = 
    let f l' e = 
      [e]::l'
    in 
    List.fold_left f [] l
  
  let merge_sort_step l = 
    let f acc e = 
      let s, l' = acc in 
      match s with 
      | None     -> Some e,  l' 
      | Some x   -> None, (merge_sorted_list x e)::l' 
    in
    let e, l' = (List.fold_left f (None, []) l) in 
    match e with 
    | None -> l'
    | Some x -> x::l' 
  
end (* Merge_sort_detail *)

let merge_sort l = 
  let rec loop = function
    | [] -> [] 
    | hd::[] -> hd
    | l' -> loop (Merge_sort_detail.merge_sort_step l')
  in
  loop (Merge_sort_detail.merge_init l)
