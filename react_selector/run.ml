
module Encoding_event = Encoding_event.Make(Encoding.Size_32_encoder) 

let (holder:unit React.event list ref) = ref []


(** [time_f f] returns [(time_in_second, f ())] *) 
let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

let nb_of_child = 10
let nb_of_msg   = 10000
let msg_size    = 1_000

let total       = nb_of_msg * nb_of_child * msg_size 

let run_as_child {Fork_util.write_fd; } = 

  Random.self_init ();
  let selector   = Selector.create () in 
  let write_msg  = Encoding_event.write_event write_fd selector in 

  let new_msg () = 
    let c   = Char.chr @@ Random.int 20 + 65 in 
    let s   = String.make msg_size c in 
    (*
    String.iter(fun c -> 
      if c ='7'
      then ()
      else ()
    ) s ;
    *)
    s 
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

  let events, selector =
    let parent = fun (events, selector) (childpid, {Fork_util.read_fd;write_fd}) ->
      Unix.close write_fd; 
      let event = 
        Encoding_event.read_event read_fd selector
        |> React.E.map (fun read_value -> (childpid, read_value))
      in 
      event::events, selector 
    in 
    let child  = run_as_child  in 
    Fork_util.fork_n ~parent ~child ~accu:([], Selector.create()) nb_of_child
  in 

  let merger_e = React.E.merge (fun (l:string list) (childpid, read_value) ->  
    match read_value with 
    | Encoding_event.String s -> 
        (*
      String.iter(fun c -> 
        if c ='7'
        then ()
        else ()
      ) s ;
      *)
      (*
      let len = String.length s in 
      (Printf.sprintf "child [%6i] received : [%10i]" childpid len)::l  
      *)
      s::l
    | Encoding_event.Closed   -> l
  ) [] events in

  let counter_s =
    let e = React.E.merge (fun i (_, read_value) -> match read_value with 
      | Encoding_event.Closed   -> i+1
      | Encoding_event.String _ -> i ) 0 events in
    let e = React.E.fold (fun n i -> n  - i) nb_of_child e in
    React.S.hold nb_of_child e in

  let merger_e = React.E.fmap (function | [] -> None | l -> Some l) merger_e in

  let file_fd = Unix.openfile 
    "tmp.tx" [
      Unix.O_RDWR;
      Unix.O_TRUNC; 
      Unix.O_CREAT;
      Unix.O_NONBLOCK; 
      Unix.O_CLOEXEC] 
    0o640 in   
  let write_file = Encoding_event.write_event file_fd selector in 
  let file_e     = React.E.map (List.iter write_file) merger_e in 

  holder := file_e::!holder; 

  let counter = ref 0 in 
  let (printer:unit React.event) = React.E.map (fun l ->
    counter := !counter + List.length l ; 
    (*
    print_endline @@ String.concat "," l;
    print_endline "-----------------";
    flush stdout;
    *)
    ()
  ) merger_e in

  holder := printer::!holder; 

  while React.S.value counter_s <> 0 || Selector.nb_of_writes selector <> 0  do
    let (_:Selector.select_status) = Selector.select 1. selector in 
    ()
  done;
  Printf.printf "nb of msg: %i\n" !counter ;
  
  assert (0 = (Unix.lseek file_fd 0 Unix.SEEK_SET));

  let t, _ = time_f (fun () -> 
    let b = Bytes.create Encoding.Size_32_encoder.size in 
    while !counter <> 0 do 
      ignore @@ Unix.read file_fd b 0 4; 
      let i = Encoding.Size_32_encoder.decode b 0 in 
      (* Printf.printf "size: %i\n%!" i;
       *)
      ignore @@ Unix.lseek file_fd i Unix.SEEK_CUR;
      counter := !counter - 1 
    done
    ) in 
  Printf.printf "iteration time: %f\n%!" t;
  ()

let () =
  let t, _ = time_f run in 
  Printf.printf "data rate : %f  Mb/s \n" (float_of_int total /.  (t *.  float_of_int 1024**2.));
  Printf.printf "msg  rate : %f msg/s \n" (float_of_int (nb_of_msg*nb_of_child) /.  t)
