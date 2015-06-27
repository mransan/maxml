
type 'a l = { 
  next : 'a n ref; 
  prev : 'a n ref;  
  d    : 'a;  
}
(** link type *) 

and 
 
'a n =
 | Nil 
 | Link of 'a l   
(** node type *) 

and

'a t = {
  front : 'a n ref; 
  back  : 'a n ref; 
}
(** doubly linked list *)


let create () = 
  { front = ref Nil; back = ref Nil; }


let reset {front; back} = 
  front := Nil ; back :=Nil 

let empty {front ; back } =
  match !front, !back with 
  | Nil , Nil -> true
  | _, _ -> false

let create_nil_link d = 
 Link {next = ref Nil; prev = ref Nil; d } 

let create_front_link n d = 
 Link {next = ref n ; prev = ref Nil; d} 
  
let create_back_link n d = 
 Link {next = ref Nil ; prev = ref n ; d} 

let push_front {front; back}  d = 
  match !front with 
  | Nil  -> 
    let n = create_nil_link d in 
    front := n; back := n 
  | Link {next;prev; d = _}  -> 
    let new_front = create_front_link !front d in 
    prev := new_front ; 
    front := new_front

let pop_front ({front; back} as l) = 
  match !front with
  | Nil -> None
  | Link {next ; prev ; d} ->
    begin 
    match !next with 
    | Nil    ->  reset l; Some d 
    | Link l ->  l.prev := Nil ; front := !next ; Some d
    end
  
let push_back {front; back} d =  
  match !back with 
  | Nil  -> 
    let n = create_nil_link d in 
    front := n; back := n
  | Link {next;prev; d = _}  -> 
    let new_back = create_back_link !back d in 
    next := new_back; 
    back := new_back

let pop_back ({front; back} as l) = 
  match !back with
  | Nil -> None
  | Link {next ; prev ; d} ->
    begin 
      match !prev with 
      | Nil    ->  reset l; Some d 
      | Link l ->  l.next := Nil ; back := !prev; Some d
    end

let fold_left f e0 {front; back = _ } = 
  let rec loop acc = function 
    | Nil -> acc 
    | Link {next;prev=_;d} -> loop (f acc d) !next 
  in 
  loop e0 !front 

let fold_right f {front = _; back } e0 = 
  let rec loop acc = function 
    | Nil -> acc 
    | Link {next = _ ; prev ; d } -> loop (f d acc) !prev
  in 
  loop e0 !back
