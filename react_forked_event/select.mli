



type selector 

val create : unit -> selector 
(** [create ()] returns a new empty selector object *)

val add_in : Unix.file_descr -> selector -> Unix.file_descr React.event
(** [add fd selector] returns an [event] which occurence indicates the file
    descriptor has data to be read. 
 *) 

val remove_in : Unix.file_descr -> selector -> unit 

val add_out: Unix.file_descr -> selector -> Unix.file_descr React.event
(** [add fd selector] returns an [event] which occurence indicates the file
    descriptor can be written to. 
 *) 

val remove_out : Unix.file_descr -> selector -> unit 

type select_status = 
    | Timeout 
    | Event_happened 
    | No_fds

val select : float -> selector -> select_status 
(** [select timeout] will wait until either [timeout] seconds has happened or
    until an event happened on one of the [Unix.file_descr] added to the
    selector. 

    In case one or more event happens the corresponding [React.event] are
    triggered
  *)

val nb_of_writes : selector -> int 
