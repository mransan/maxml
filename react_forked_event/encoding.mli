
val write_msg : ?buf:bytes -> Unix.file_descr  -> string -> unit 
(** [write_msg ~buf fd s] will write the string [s] on the 
    [fd]. The optional argument [buf] can be used by the client 
    to improve speed performance to avoid re-creating 
    temporary buffer. The buffer should be at least I.size in available
    length.
 *) 

val read_msg : ?buf:bytes -> Unix.file_descr -> string 
(** [read_msg ~buf fd] will read a string encoding using the [write_msg] 
    function. 
  *) 

type fork_connection = < 
  write_fd: Unix.file_descr;
  write   : string -> unit; 
  read_fd : Unix.file_descr; 
  read    : string ;
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
