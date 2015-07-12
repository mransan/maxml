type fork_connection = < 
  write_fd: Unix.file_descr;
  read_fd : Unix.file_descr; 
> 

type fork = 
    | Child  of fork_connection  
    | Parent of int * fork_connection 

let fork () = 
    let pc = Pipe_connection.create () in 
    match Unix.fork () with
    | 0 -> 
        Pipe_connection.set `P1 pc; 
        Child  (object(this) 
        method write_fd = Pipe_connection.write_fd `P1 pc; 
        method read_fd = Pipe_connection.read_fd `P1 pc; 
    end) 
    | childpid -> 
        Pipe_connection.set `P2 pc; 
        Parent (childpid, (object(this) 
        method write_fd = Pipe_connection.write_fd `P2 pc; 
        method read_fd = Pipe_connection.read_fd `P2 pc; 
    end)) 

type selector = {
    selector : Select.selector; 
    holder   : (unit React.event) list ref; 
} 

let create () = {
    selector = Select.create (); 
    holder   = ref [];
}

let add_write fork_connection {selector; holder} = 
    let msg_queue      = ref [] in 
    (* all the msg waiting to be sent *)

    let state = Encoding.Write.create_state () in 

    let write write_fd = 
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
            let write_ready_e = Select.add_out fork_connection#write_fd selector in  
            let write_e = React.E.map (fun write_fd -> 
                write write_fd;
                if !msg_queue = [] 
                then Select.remove_out write_fd selector
                else ()
            ) write_ready_e in 
            holder := write_e :: !holder; 
        )
        else () 
    in  
    write_f 

type read_value = 
    | String of string 
    | Closed 

let add_read fork_connection {selector; holder; } = 
    
    let read_ready_e = Select.add_in fork_connection#read_fd selector in 

    let on_close read_fd = 
        Select.remove_in read_fd selector; 
        Some Closed
    in
    
    let state = Encoding.Read.create_state  () in 

    let msg_e = React.E.fmap (fun read_fd -> 
        match Encoding.Read.read state read_fd with
        | Encoding.Read.Complete s -> Some (String s) 
        | Encoding.Read.Closed     -> on_close read_fd
        | Encoding.Read.Partial    -> None 
    ) read_ready_e in 

    holder:=(React.E.fmap (fun _ -> None) msg_e)::!holder;
    
    msg_e

let add_fork_connection fork_connection s = 
    let msg_e = add_read fork_connection s in  
    let write_f = add_write fork_connection s in 
    (write_f, msg_e) 
     
let select timeout {selector;} = 
    Select.select timeout selector

let nb_of_writes {selector; _ }  = 
    Select.nb_of_writes selector 
