
type read_value = 
    | String of string 
    | Closed 

let read_event read_fd selector = 

    let read_ready_e = Select.add_in read_fd selector in 

    let on_close () = 
        Select.remove_in read_fd selector; 
        Some Closed
    in
    
    let state = Encoding.Read.create_state () in 
    
    let read_e  = React.E.map (fun read_fd -> 
        Encoding.Read.read state read_fd
    ) read_ready_e in  
    
    let msg_e = React.E.fmap (function 
        | Encoding.Read.Complete s -> Some (String s) 
        | Encoding.Read.Closed     -> on_close ()
        | Encoding.Read.Partial    -> None 
    ) read_e in 

    Select.add_side_effect_event read_fd (React.E.fmap (fun _ -> None) msg_e) selector; 
    
    msg_e


    
let write_event write_fd selector = 
    let msg_queue      = ref [] in 
    (* all the msg waiting to be sent *)

    let state = Encoding.Write.create_state () in 

    let write () = 
        match !msg_queue with 
        | []       -> failwith "Programatic error [write_msg_queue is empty]"
        | msg::tl  -> (
            match Encoding.Write.write msg state write_fd with
            | Encoding.Write.Complete -> (msg_queue := tl)
            | Encoding.Write.Partial  -> ()
        )
    in

    let write_f msg = 
        msg_queue := !msg_queue @ [msg];
        if List.length !msg_queue = 1
        then ( 
            (* When a brand new msg is added this is when we need to start
               listening to the file descriptor availability
             *)
            let write_ready_e = Select.add_out write_fd selector in  
            let write_e = React.E.map (fun write_fd -> 
                write ();
                if !msg_queue = [] 
                then Select.remove_out write_fd selector
                else ()
            ) write_ready_e in 
            Select.add_side_effect_event write_fd write_e selector;  
        )
        else () 
    in  
    write_f 
