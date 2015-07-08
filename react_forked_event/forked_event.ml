
module C_handle = struct 
  
  type child_process = {
    parent_pid : int;
    pid        : int; 
    fd         : Unix.file_descr; 
  }

  type 'a t = {
    mutable f : (?step:React.step -> 'a -> unit);
    child_process : child_process option;
  }
 
  let send {f;child_process = _ } ?step x = 
    f ?step x 

  let terminate {f=_; child_process} = 
    match child_process with 
    | Some {parent_pid; pid; fd} -> 
      if parent_pid = (Unix.getpid ())
      then begin  
        Printf.printf "parent [%10i] terminateing child [%10i]\n%!" (Unix.getpid()) pid; 
        Unix.close fd; 
        Printf.printf "parent [%10i] close done on [%10i] \n%!" (Unix.getpid()) pid; 
        let (_:int*Unix.process_status) =  Unix.waitpid [] pid in 
        () 
      end 
      else ()
    | None -> ()
  
  let of_send f = 
    let pid = Unix.getpid () in 
    let rec t  = { 
      child_process = None;
      f = (fun ?step (x:'a) -> 
        if Unix.getpid() = pid
        then (t.f <- f ; t.f ?step x) 
        else (t.f <- (fun ?step _ -> ())) 
    ) } in 
    t 
  
  let of_send_child f child_process =  
    {(of_send f) with child_process = Some child_process } 
 
  let child_pid {f=_ ; child_process} = match child_process with
    | None -> None 
    | Some {pid; fd=_} -> Some pid 

  let noop =  {f = (fun ?step _  -> ()) ; child_process = None } 

end

module P_handle = struct 
  type t = {
    mutable fds : (int * Unix.file_descr) list;
    pid : int; 
    mutable is_locked : bool; 
  }
  
  let create () = 
    {fds = [] ; pid = Unix.getpid(); is_locked = false } 
  
  let close_all ({fds; pid=_; is_locked = _ } as t) = 
    let (_:unit) = List.iter (fun (pid, fd) -> 
      if pid <> Unix.getpid() then Unix.close fd
    ) fds  in 
    t.fds <- []  
  
  let add_fd ({fds; pid=_; is_locked = _ } as t) fd = 
    t.fds <- ((Unix.getpid()),fd)::fds

  let lock ph f = 
    try 
      ph.is_locked <- true ; 
      let x = f () in 
      ph.is_locked <- false ; 
      x  
    with exn -> ph.is_locked <- false ; raise exn

  let is_locked {pid;is_locked; fds=_} = 
    is_locked && Unix.getpid() <> pid  
end 

module E = struct 
  let create parent_handle (e:'a React.event) serialize deserialize = 
    if e = React.E.never || P_handle.is_locked parent_handle
    then e, C_handle.noop
    else 
      let fd_read, fd_write = Unix.pipe () in 
      match Unix.fork () with 
      | 0 ->                                    (** child process *) 
        P_handle.close_all parent_handle;
        Unix.close fd_write; 
        (*
        Unix.set_nonblock fd_read;
        *)
        let read_a () = 
          deserialize @@ Encoding.read_msg fd_read 
        in 
        let e, send = React.E.create () in 
        e, C_handle.of_send (fun ?step () -> 
          (*Printf.printf "[%10i] : waiting on read \n%!" (Unix.getpid());
           *)
          send ?step @@ read_a ()   
        )
  
      | pid ->                                  (** parent process *) 
        Printf.printf "[%10i] child %i \n%!" (Unix.getpid()) pid; 
        Unix.close fd_read; 
        P_handle.add_fd parent_handle fd_write;
        let write_a x = 
          Encoding.write_msg fd_write @@ serialize x 
        in
        let write_e =  React.E.fmap (fun x -> 
          write_a x; 
          None
        ) e in 
        let pidd =  C_handle.of_send_child 
          (fun ?step () -> ignore write_e)
          {C_handle.parent_pid = Unix.getpid(); pid; fd = fd_write} in  
        React.E.never, pidd
end 
