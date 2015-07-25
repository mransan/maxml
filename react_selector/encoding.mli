
(** Module signature to encode the size of a message in bytes. 
    
    The protocol used to send message encodes first the size then 
    then the message string. 

    In order to accomodate different application with different message size
    this module signature allows an application to define their own size
    encoding. 

    For instance if your message size is less than 254 then you can 
    implement an in 1byte [Size_encoder_sig]. 

  *) 
module type Size_encoder_sig = sig
  val size : int 
  val encode : int -> bytes -> int -> unit 
  val decode : bytes -> int -> int  
end 

module Size_32_encoder : Size_encoder_sig
(** Off the shelf size encoder which will used 4bytes to encode the size, hence
    allowing msg size between [0; ~3.8Gb]
 *) 

module type S = sig
  
  type read_status = 
    | Read_closed              (** [Closed] indicates the file descriptor 
                                   has been closed and no data can be read 
                                   anymore *)
    | Read_partial of float    (** [Partial percentage] indicates a partial 
                                   read of a message with [percentage] read 
                                   so far. *)  
    | Read_complete of string  (** [Complete s] indicates that the a full 
                                   message [s] has been read. *) 
  
  type read_state
  (** abstract type which holds the data between intermediate and subsequent
      reads. In particular the value of this type will hold the information
      necessary to keep track of any partial reads.

      Note that a read state can only work with a single read file descriptor.
      In other word it won't keep track of the partial read on multiple file
      descriptors. 
   *)
  
  val create_read_state : unit -> read_state 
  (** [create_read_state ()] returns a new read state. 
   *)
  
  val read : read_state -> Unix.file_descr -> read_status 
  (** [read state fd] reads data from [fd]. If [Partial] is returned
      then all the information will be kept in the [state] (ie [state] will be
      modified).
    *)

  type write_status = 
    | Write_complete 
    | Write_partial 
  (** write status *)
  
  type write_state
  (** abstract type which encapsulate the data associated with writing to 
      a given file descriptor. A [state] can only be used with a single 
      file descriptor.
   *)
  
  val create_write_state : unit -> write_state
  (** [create_state ()] creates a new state *)

  val write : string -> write_state -> Unix.file_descr -> write_status 
  (** [write msg state fd] will write [msg] on [fd], encoding the 
      message size using the [S] module. If [Partial] is returned 
      then it is the responsability of the caller to call [write] 
      again with the same [state] value.
   *)
end

module Make(S:Size_encoder_sig) : S
