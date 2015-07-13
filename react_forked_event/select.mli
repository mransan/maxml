
type selector 

val create : unit -> selector 
(** [create ()] returns a new empty selector object *)

val add_in : Unix.file_descr -> selector -> Unix.file_descr React.event
(** [add fd selector] returns an [event] which occurence indicates the file
    descriptor has data to be read. 
 *) 

val remove_in : Unix.file_descr -> selector -> unit 
(** [remove_in fd selector] remove [fd] from the list of file descriptors which 
    are monitored for read events.
  *)

val add_out: Unix.file_descr -> selector -> Unix.file_descr React.event
(** [add fd selector] returns an [event] which occurence indicates the file
    descriptor can be written to. 
 *) 

val remove_out : Unix.file_descr -> selector -> unit 
(** [remove_out fd selector] remove [fd] from the list of file descriptors which 
    are monitored for write events.
  *)

val add_side_effect_event : Unix.file_descr -> unit React.event -> selector -> unit 
(** [add_side_effect_event fd event selector] keeps the [event] in memory with the
    [selector] until the [fd] is removed
 *) 

type select_status = 
    | Event_happened (** In this case at least one event was triggered *) 
    | Timeout        (** No event happened on the monitored file descriptors *) 
    | No_fds         (** There are no file desriptors to monitor any more *)

val select : float -> selector -> select_status 
(** [select timeout] will wait until either [timeout] seconds has happened or
    until an event happened on one of the [Unix.file_descr] added to the
    selector. 

    In case one or more event happens the corresponding [React.event] are
    triggered
  *)

val nb_of_writes : selector -> int 
