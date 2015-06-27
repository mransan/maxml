
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


val create : unit -> 'a t 

val empty : 'a t -> bool 

val push_front : 'a t -> 'a ->  unit  
val pop_front  : 'a t -> 'a option 

val push_back : 'a t -> 'a ->  unit  
val pop_back  : 'a t -> 'a option 


val fold_left  : ('b -> 'a -> 'b) -> 'b   -> 'a t -> 'b 
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b   -> 'b 

