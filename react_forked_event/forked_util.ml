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
