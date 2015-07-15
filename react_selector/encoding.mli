
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

module type Read_sig = sig
  
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

module Make_read (S:Size_encoder_sig) : Read_sig

module type Write_sig = sig

  type status = 
    | Complete 
    | Partial 
  (** write status *)
  
  type state
  (** abstract type which encapsulate the data associated with writing to 
      a given file descriptor. A [state] can only be used with a single 
      file descriptor.
   *)
  
  val create_state : unit -> state
  (** [create_state ()] creates a new state *)

  val write : string -> state -> Unix.file_descr -> status 
  (** [write msg state fd] will write [msg] on [fd], encoding the 
      message size using the [S] module. If [Partial] is returned 
      then it is the responsability of the caller to call [write] 
      again with the same [state] value.
   *)
end

module Make_write(S:Size_encoder_sig) : Write_sig 
