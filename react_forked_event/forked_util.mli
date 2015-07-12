
type fork_connection = < 
  write_fd: Unix.file_descr;
  read_fd : Unix.file_descr; 
> 

type fork = 
    | Child  of fork_connection  
    | Parent of int * fork_connection 

val fork : unit -> fork 

type selector 

val create : unit -> selector 

type read_value = 
    | String of string 
    | Closed 

val add_fork_connection : fork_connection -> selector -> ((string -> unit) * (read_value React.event))  

val select : float -> selector -> Select.select_status 

val nb_of_writes : selector -> int 
