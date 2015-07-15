
module Encoding_event = Encoding_event.Make(Encoding.Size_32_encoder) 

let (holder:unit React.event list ref) = ref []


(** [time_f f] returns [(time_in_second, f ())] *) 
let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

let nb_of_child = 10
let nb_of_msg   = 5000
let msg_size    = 50_000

let total       = nb_of_msg * nb_of_child * msg_size 

let run_as_child {Fork_util.write_fd; } = 

  Random.self_init ();
  let selector   = Selector.create () in 
  let write_msg  = Encoding_event.write_event write_fd selector in 

  let new_msg () = 
    let c   = Char.chr @@ Random.int 20 + 65 in 
    String.make msg_size c 
  in 
    
  let rec loop nb_of_msg =  
    match Selector.select 0.1 selector with 
    | Selector.Timeout | Selector.No_fds -> (
      if Selector.nb_of_writes selector = 0 
      then (
        if nb_of_msg = 0
        then (
          Printf.printf "Child [%10i] exiting\n%!" (Unix.getpid()) ; 
          exit 0 
        )
        else (
          write_msg @@ new_msg (); 
          loop @@ nb_of_msg - 1 
        )
      )
      else loop nb_of_msg 
    )
    | Selector.Event_happened -> loop nb_of_msg
  in 
  loop nb_of_msg 

let run () =

  let selector = Selector.create () in

  let events =
    let rec loop events = function
    | 0 -> events
    | i -> (
      match Fork_util.fork () with
      | Fork_util.Child  connection -> run_as_child connection 
      | Fork_util.Parent (childpid , {Fork_util.read_fd;write_fd} ) -> (
        (*Unix.close write_fd;
         *)
        let event = Encoding_event.read_event read_fd selector in
        let event = React.E.map (fun read_value -> 
          (*Printf.printf "read_event from [%10i]\n%!" childpid;
           *)
          (childpid, read_value)) event in  
        loop ((event)::events) (i - 1)
        )
      )
    in
    loop [] nb_of_child
  in

  let merger_e = React.E.merge (fun (l:string list) (childpid, read_value) ->  
    match read_value with 
    | Encoding_event.String s -> 
      let len = String.length s in 
      (Printf.sprintf "child [%6i] received : [%10i]" childpid len)::l  
    | Encoding_event.Closed   -> l
  ) [] events in

  let counter_s =
    let e = React.E.merge (fun i (_, read_value) -> match read_value with 
      | Encoding_event.Closed   -> i+1
      | Encoding_event.String _ -> i ) 0 events in
    let e = React.E.fold (fun n i -> n  - i) nb_of_child e in
    React.S.hold nb_of_child e in

  let merger_e = React.E.fmap (function | [] -> None | l -> Some l) merger_e in

  let counter = ref 0 in 
  let (printer:unit React.event) = React.E.map (fun l ->
    counter := !counter + List.length l ; 
    print_endline @@ String.concat "," l;
    print_endline "-----------------";
    flush stdout;
    ()
  ) merger_e in

  holder := printer::!holder; 

  while React.S.value counter_s <> 0 do
    let (_:Selector.select_status) = Selector.select 1. selector in 
    ()
  done;
  Printf.printf "nb of msg: %i\n" !counter ;
  ()

let () =
  let t, _ = time_f run in 
  Printf.printf "data rate : %f  Mb/s \n" (float_of_int total /.  (t *.  float_of_int 1024**2.));
  Printf.printf "msg  rate : %f msg/s \n" (float_of_int (nb_of_msg*nb_of_child) /.  t)
