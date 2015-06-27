
module C_handle: sig
  type 'a t

  val noop : unit t 
  (** [noop] return a handle to a non existing child 
   *)

  val of_send : (?step:React.step -> 'a -> unit) -> 'a t 
  (** [of_send f] create a child handle for the current process. 
      
      This function is a conveniency to hold in a single 
      container the send function returned by [React.E.create] as well
      as the E.create 
   *) 
  
  val send : 'a t -> ?step:React.step -> 'a -> unit 
  (** [send handle ~step x] will send the value [x]. When handle
      was created with [of_send f] then it will simply execute the registered
      function [f]. When handle was created with [E.create], then it will be 
      a write to the pipe in the parent process and a read to the pipe 
      in the child process. 
    *)  
  
  val terminate : 'a t -> unit 
  (** [kill handle] when invoked in the parent process will close the pipe
      with the children process and wait for it to terminate.
   *) 
end

module P_handle : sig 
  type t  
  
  val create : unit -> t 
  val lock : t -> (unit -> 'a) -> 'a  
end 

module E : sig 

  val create : 
    P_handle.t -> 
    'a React.E.t -> 
    ('a -> string) -> 
    (string -> 'a) -> 
    ('a React.E.t * unit C_handle.t)

end 

