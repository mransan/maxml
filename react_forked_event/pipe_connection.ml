
type t = {
  fd_server_read : Unix.file_descr; 
  fd_server_write : Unix.file_descr; 
  fd_client_read : Unix.file_descr; 
  fd_client_write : Unix.file_descr; 
}

let create () =
  let fd_server_read, fd_client_write = Unix.pipe () in 
  let fd_client_read, fd_server_write = Unix.pipe () in 
  {fd_server_write;fd_client_read; fd_client_write; fd_server_read } 

let set as_ pc = 
  match as_ with 
  | `P1 -> (Unix.close pc.fd_server_write; Unix.close pc.fd_server_read;)
  | `P2 -> (Unix.close pc.fd_client_write; Unix.close pc.fd_client_read;)

let read_fd as_ pc = 
  match as_ with
  | `P1 -> pc.fd_client_read 
  | `P2 -> pc.fd_server_read 

let write_fd as_ pc = 
  match as_ with
  | `P1 -> pc.fd_client_write 
  | `P2 -> pc.fd_server_write 
