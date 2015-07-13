
type fork_connection = < 
  write_fd: Unix.file_descr;
  read_fd : Unix.file_descr; 
> 

type fork = 
    | Child  of fork_connection  
    | Parent of int * fork_connection 

val fork : unit -> fork 
