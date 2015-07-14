
type t 

val create : unit -> t 
(** [create ()] returns a new empty t object *)

val add_in : Unix.file_descr -> t -> Unix.file_descr React.event
(** [add fd t] returns an [event] which occurence indicates the file
    descriptor has data to be read. 
 *) 

val remove_in : Unix.file_descr -> t -> unit 
(** [remove_in fd t] remove [fd] from the list of file descriptors which 
    are monitored for read events.
  *)

val add_out: Unix.file_descr -> t -> Unix.file_descr React.event
(** [add fd t] returns an [event] which occurence indicates the file
    descriptor can be written to. 
 *) 

val remove_out : Unix.file_descr -> t -> unit 
(** [remove_out fd t] remove [fd] from the list of file descriptors which 
    are monitored for write events.
  *)

val add_side_effect_event : Unix.file_descr -> unit React.event -> t -> unit 
(** [add_side_effect_event fd event t] keeps the [event] in memory with the
    [t] until the [fd] is removed
 *) 

type select_status = 
    | Event_happened (** In this case at least one event was triggered *) 
    | Timeout        (** No event happened on the monitored file descriptors *) 
    | No_fds         (** There are no file desriptors to monitor any more *)

val select : float -> t -> select_status 
(** [select timeout] will wait until either [timeout] seconds has happened or
    until an event happened on one of the [Unix.file_descr] added to the
    t. 

    In case one or more event happens the corresponding [React.event] are
    triggered
  *)

val nb_of_writes : t -> int 
