
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


module Read : sig 

    type status = 
        | Closed             (** [Closed] indicates the file descriptor has been closed
                                 and no data can be read anymore *)
        | Partial  of float  (** [Partial percentage] indicates a partial read 
                                of a message with [percentage] read so far. *)  
        | Complete of string (** [Complete s] indicates that the a full message
                                 [s] has been read. *) 
    
    type state
    (** abstract type which holds the data between intermediate and subsequent
        reads. In particular the value of this type will hold the information
        necessary to keep track of any partial reads.

        Note that a read state can only work with a single read file descriptor.
        In other word it won't keep track of the partial read on multiple file
        descriptors. 
     *)
    
    val create_state : unit -> state 
    (** [create_state ()] returns a new read state. 
     *)
    
    val read : state -> Unix.file_descr -> status 
    (** [read state fd] reads data from [fd]. If [Partial] is returned
        then all the information will be kept in the [state] (ie [state] will be
        modified).
      *)
end 


module Write : sig

    type status = 
        | Complete 
        | Partial 
    
    type state
    
    val create_state : unit -> state


    val write : string -> state -> Unix.file_descr -> status 
    
    
end


