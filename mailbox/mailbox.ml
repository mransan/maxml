

module Util = struct 

let fold_i f e0 (b, e) = 
  let inc = if b>e then (-) else (+) in 
  let rec loop acc = function 
    | i when i = e -> f acc i 
    | i -> loop (f acc i) (inc i 1) 
  in 
  loop e0 b 
(** [fold_i (fun sum i -> sum + i) 0 (0 20)] iterate from 
    b to e while accumulating in a fold_left fashion
 *)

let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

end 


module C_binding = struct 
  external c_sleep : int -> unit  = "ml_c_sleep"
end

module Bsl_mutex = struct 
  let on_lock m f = 
    Mutex.lock m; 
    let x = f () in 
    Mutex.unlock m; 
    x 
end

module Bsl_thread = struct
  let create_n n funct arg = 
    Util.fold_i (fun l _ -> (Thread.create funct arg)::l) [] (1,n) 

  let join_all l = 
    List.fold_left (fun () x -> Thread.join x ) () l
end 

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
    Bsl_mutex.on_lock mb.lock (fun () -> 
      Queue.add v mb.queue; 
      Condition.signal mb.notempty_condition; 
    ) 

  let take mb =
    Bsl_mutex.on_lock mb.lock (fun () -> 
      while Queue.is_empty mb.queue do
        Condition.wait mb.notempty_condition mb.lock;
      done;
      Queue.take mb.queue 
    )
end

type 'a orders =
  | Process of (unit -> unit)
  | Terminate

(** main loop for the worker thread. 
   Each worker thread will execute the Process command until the 
   Terminate signal is sent
 *)
let rec worker_loop mailbox =
  match Mailbox.take mailbox with
  | Process f ->
    f (); worker_loop mailbox;  
  | Terminate ->
    ()

let create_worker_job iter i print_mailbox = fun () ->
  try 
    let do_sum acc i = 
      if i mod 10_000 = 0 then Thread.yield () ; 
      acc+i
    in
    if iter = 1 || iter = 2
    then 
      C_binding.c_sleep 17
    else
      let t, sum = Util.time_f (fun () ->  Util.fold_i do_sum 0 (1, i*1000)) in  
      let thread_id = Thread.id @@ Thread.self () in 
      Mailbox.add print_mailbox (Process (fun () -> 
          Printf.printf "[%5d] : sum (1 -> %7dk) = %10d [%3.3f s]\n%!" thread_id i sum t;
      )) 
  with _ -> 
    Printf.printf "Error for i = %d \n%!" i

let rec master_loop calc_mailbox print_mailbox =
  
  Util.fold_i (fun (nb_jobs, sum) i -> 
    if sum > 1_500_000 
    then nb_jobs, sum 
    else 
      let x = Random.int 10_000 in 
      Mailbox.add calc_mailbox (Process (create_worker_job i x print_mailbox)); 
      nb_jobs + 1, sum + x 
  ) (0,0) (1, 10_000) 
    

let () =

  let t, (nb_jobs, sum) = Util.time_f (fun () -> 
  let number_of_thread = 10 in 
  let calc_mailbox = Mailbox.create () in
  let calc_workers = Bsl_thread.create_n number_of_thread worker_loop calc_mailbox in
  
  (** Centralized printing thread so that all printing operations are
      synchronous
   *)
  let print_mailbox = Mailbox.create () in 
  let print_worker  = Thread.create worker_loop print_mailbox in   
  
  let nb_jobs, sum = master_loop calc_mailbox print_mailbox in 
  print_string "Done creating jobs \n%!";
  
  List.iter (fun _ -> Mailbox.add calc_mailbox Terminate) calc_workers;
  Bsl_thread.join_all calc_workers;

  Mailbox.add print_mailbox Terminate; 
  Thread.join print_worker;
  nb_jobs, sum
  ) in 

  Printf.printf ">> Nb of jobs: %5d, Total sum: %d , Total time: %f \n%!" nb_jobs sum t;
