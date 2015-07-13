
type read_value = 
    | String of string 
    | Closed 

val read_event : Unix.file_descr -> Selector.t -> read_value React.event 

val write_event : Unix.file_descr -> Selector.t -> (string -> unit) 
