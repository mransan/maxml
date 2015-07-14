type connection = {
  read_fd : Unix.file_descr;
  write_fd : Unix.file_descr; 
}

type fork = 
  | Child  of connection  
  | Parent of int * connection 

let connection_of_pipe side pc = {
  write_fd = Pipe_connection.write_fd side pc; 
  read_fd = Pipe_connection.read_fd side pc; 
}

let fork () = 
  let pc = Pipe_connection.create () in 
  match Unix.fork () with
  | 0 -> 
    Pipe_connection.set `P1 pc; 
    Child  (connection_of_pipe `P1 pc) 
  | childpid -> 
    Pipe_connection.set `P2 pc; 
    Parent (childpid, connection_of_pipe `P2 pc) 
