

exception Programmatic_error 

type 'a t 

val init : int -> 'a -> 'a t 

type 'a e 

val value_of_e : 'a e -> 'a  

val root : 'a t -> 'a e  

val parent_of_e      : 'a t -> 'a e -> 'a e 
val left_child_of_e  : 'a t -> 'a e -> 'a e option 
val right_child_of_e : 'a t -> 'a e -> 'a e option  

val insert : 'a t -> 'a -> unit 

val length : 'a t -> int 

val string_of_t : 'a t -> ('a -> string) -> string 



