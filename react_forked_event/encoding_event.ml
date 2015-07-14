
type read_value = 
    | String of string 
    | Closed 

let read_event read_fd selector = 

    let read_ready_e = Selector.add_in read_fd selector in 

    let on_close () = 
        Selector.remove_in read_fd selector; 
        Some Closed
    in
    
    let state = Encoding.Read.create_state () in 
    
    let read_e  = React.E.map (fun read_fd -> 
        Encoding.Read.read state read_fd
    ) read_ready_e in  
    
    let msg_e = React.E.fmap (function 
        | Encoding.Read.Complete s -> Some (String s) 
        | Encoding.Read.Closed     -> on_close ()
        | Encoding.Read.Partial  _ -> None 
    ) read_e in 

    Selector.add_side_effect_event read_fd (React.E.fmap (fun _ -> None) msg_e) selector; 
    
    msg_e
    
let write_event write_fd selector = 
    let msg_queue      = Queue.create () in 
    (* all the msg waiting to be sent *)

    let state = Encoding.Write.create_state () in 

    let write () = 
        match Queue.is_empty msg_queue with 
        | true -> failwith "Programatic error [write_msg_queue is empty]"
        | false -> (
            match Encoding.Write.write (Queue.peek msg_queue) state write_fd with
            | Encoding.Write.Complete -> ignore @@ Queue.pop msg_queue
            | Encoding.Write.Partial  -> () 
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
