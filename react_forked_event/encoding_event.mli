

type read_value = 
    | String of string 
    | Closed 

val read_event : Unix.file_descr -> Select.selector -> read_value React.event 

val write_event : Unix.file_descr -> Select.selector -> (string -> unit) 
