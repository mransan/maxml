
module type S = sig 
  type read_value = 
    | String of string 
    | Closed 
  
  val read_event : Unix.file_descr -> Selector.t -> read_value React.event 
  
  val write_event : Unix.file_descr -> Selector.t -> (string -> unit) 
end 

module Make(S:Encoding.Size_encoder_sig) = struct 

  module Encoding = Encoding.Make(S)

  type read_value = 
  | String of string 
  | Closed 

  let read_event read_fd selector = 
  
    let read_ready_e = Selector.add_in read_fd selector in 
  
    let on_close () = 
      Selector.remove_in read_fd selector; 
      Some Closed
    in
    
    let state = Encoding.create_read_state() in 
    
    let read_e  = React.E.map (fun read_fd -> 
      Encoding.read state read_fd
    ) read_ready_e in  
    
    let msg_e = React.E.fmap (function 
      | Encoding.Read_complete s -> Some (String s) 
      | Encoding.Read_closed     -> on_close ()
      | Encoding.Read_partial  _ -> None 
    ) read_e in 
  
    Selector.add_side_effect_event read_fd (React.E.fmap (fun _ -> None) msg_e) selector; 
    
    msg_e
    
  let write_event write_fd selector = 
    let msg_queue    = Queue.create () in 
    (* all the msg waiting to be sent *)
  
    let state = Encoding.create_write_state () in 
  
    let write () = 
      match Queue.is_empty msg_queue with 
      | true -> failwith "Programatic error [write_msg_queue is empty]"
      | false -> (
        match Encoding.write (Queue.peek msg_queue) state write_fd with
        | Encoding.Write_complete -> ignore @@ Queue.pop msg_queue
        | Encoding.Write_partial  -> () 
      )
    in
  
    let write_f msg = 
      Queue.add msg msg_queue;
      if Queue.length msg_queue = 1
      then ( 
        (* When a brand new msg is added this is when we need to start
           listening to the file descriptor availability
         *)
        let write_ready_e = Selector.add_out write_fd selector in  
        let write_e = React.E.map (fun write_fd -> 
          write ();
          if Queue.is_empty msg_queue 
          then Selector.remove_out write_fd selector
          else ()
        ) write_ready_e in 
        Selector.add_side_effect_event write_fd write_e selector;  
      )
      else () 
    in  
    write_f 
end 
