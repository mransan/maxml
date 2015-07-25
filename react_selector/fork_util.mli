

type connection = {
  read_fd : Unix.file_descr;
  write_fd : Unix.file_descr; 
}

type fork = 
  | Child  of connection  
  | Parent of int * connection 

val fork : unit -> fork 

val fork_n : 
  parent:('a -> (int * connection) -> 'a) -> 
  child :(connection -> 'a) -> 
  accu:'a -> 
  int -> 
  'a 
