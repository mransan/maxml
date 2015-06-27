(* Compile (native):
   $ ocamlopt -thread unix.cmxa threads.cmxa async_interface.ml -o
   async_interface
 *)

module Mailbox = struct

  type 'a t = {
      lock: Mutex.t;
      notempty_condition: Condition.t;
      queue: 'a Queue.t;
  }

  let create () = {
    lock = Mutex.create ();
    notempty_condition = Condition.create ();
    queue = Queue.create ();
  }

  let add mb v =
    Mutex.lock mb.lock;
    Queue.add v mb.queue;
    Condition.signal mb.notempty_condition;
    Mutex.unlock mb.lock

  let take mb =
    Mutex.lock mb.lock;
    while Queue.is_empty mb.queue do
      Printf.printf "(waiting)\n%!";
      Condition.wait mb.notempty_condition mb.lock
    done;
    let v = Queue.take mb.queue in
    Mutex.unlock mb.lock;
    v
end

type 'a orders =
  | Process of 'a
  | Terminate

  let permute_string s buf =
    let len = String.length s in
    let sep = ref "" in
    let rec aux i =
      if i = 0 then begin
        Buffer.add_string buf !sep;
        Buffer.add_char buf '"';
        Buffer.add_string buf s;
        Buffer.add_char buf '"';
        sep := ","
      end
      else
        let c = s.[i] in
        for j = 0 to i - 1 do
          s.[i] <- s.[j];
          s.[j] <- c;
          aux (i - 1);
          s.[j] <- s.[i]
        done;
        s.[i] <- c;
        aux (i - 1)
    in
    if len > 0 
    then
      aux (len - 1) 
  
  
  let rec slave_loop mailbox =
    match Mailbox.take mailbox with
    | Process s ->
      Printf.printf "Working on %s...%!" s;
      let len = String.length s in
      let fact n =
        let rec aux i acc =
          if i < 2 
          then acc
          else 
            aux (i - 1) (acc * i) 
        in aux n 1 
      in
      (* Buffers reallocate as needed, but since we know the size
         beforehand... 
       *)
      let expected_output_size = (len + 3) * (fact len) + 2 in
      let buf = Buffer.create expected_output_size in
      Buffer.add_char buf '[';
      permute_string s buf;
      Buffer.add_string buf "]\n";
      Printf.printf " Done Work On %s!\n" s;
      Buffer.output_buffer stdout buf;
      flush stdout;
      slave_loop mailbox
    | Terminate ->
      Printf.printf "%s\n%!" "We're quitting! Alright!"

let rec master_loop mailbox article =
  Printf.printf "Please input %s string to permute: %!" article;
  let exit_string = "EXIT" in
  let s = 
    try read_line () 
    with End_of_file -> exit_string 
  in
  if s = exit_string then begin
  Printf.printf "%s\n%!" "Quitting, I'll let my worker thread know";
  Mailbox.add mailbox Terminate
  end
  else begin
    Printf.printf "Passing on %s...\n%!" s;
    Mailbox.add mailbox (Process s);
    master_loop mailbox "another"
  end

let () =
  let mailbox = Mailbox.create () in
  let slave_thread_id = Thread.create slave_loop mailbox in
  print_string "Hello user! ";
  master_loop mailbox "a";
  Thread.join slave_thread_id
