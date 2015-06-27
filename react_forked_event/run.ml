
let debug = true 

module Prime = struct  

  let not_divisible n l = 
    for i=0 to 100 do
        ignore @@ not @@ List.exists (fun m -> n mod m = 0) l 
    done;
    not @@ List.exists (fun m -> n mod m = 0) l 

  let print_prime n = 
      if debug 
      then Printf.printf "[%6i] prime found: %5i \n%!" (Unix.getpid()) n  
      else () 
      
  let print_to_file_prime = Fn.run 
    (fun () -> 
      let name = Printf.sprintf "./logs/prime%i.txt" (Unix.getpid()) in 
      open_out name)
    (fun oc n -> Printf.fprintf oc "%5i\n%!" n; flush oc) 

  exception Sequence_done 

  let create_counter_event k = 
    let e, send = React.E.create () in 
    let counter = ref 2 in  
    let counter_send ?step () = 
      if !counter = k
      then 
        raise Sequence_done 
      else begin
        if debug 
        then (
          Unix.sleep 1; 
          Printf.printf "[%6i] sending %5i \n%!" (Unix.getpid ()) !counter
        )
        else ();
        
        send ?step !counter; 
        counter:=!counter+1; 
      end
    in
    e, counter_send
  
  let create_read_prime ?nth when_prime_f int_event = 
    let first_primes = ref [] in  
    let len          = ref 0 in 
    React.E.fmap (fun n -> 
      if not_divisible n !first_primes 
      then 
        let handle_prime () = 
          when_prime_f n; 
          first_primes := n:: !first_primes; 
          len := !len + 1;  
          None
        in 
        match nth with 
        | None -> handle_prime () 
        | Some nth -> ( 
          if !len >= nth 
          then Some n 
          else handle_prime () 
        )
      else None
    ) int_event
end 

let print_banner s=
  let open Printf in 
  printf "----------------------\n";  
  printf "----%5s----\n" s;
  printf "----------------------\n";  
  ()

let run_01 ()  = 
   
   print_banner "test 1";
   flush stdout;
  
  let counter_size = 12 in 

  let ph = Forked_event.P_handle.create () in 

  let fork_event (e, step_functions) =
    let e, send = Forked_event.E.create ph e string_of_int int_of_string in 
    e, send::step_functions
  in 

  let create_counter_event sf = 
      let e, send = Prime.create_counter_event counter_size  in
      e, Forked_event.C_handle.of_send send :: sf
  in 

  let read_prime ?nth (int_event, sf) = 
      Prime.create_read_prime ?nth Prime.print_prime int_event, sf
  in 

  let (_, sf) = (
      create_counter_event []
    |> read_prime ~nth:1
    |> fork_event 
    |> read_prime ~nth:1
    |> fork_event 
    |> read_prime ~nth:1
    |> fork_event 
    |> read_prime 
  )
  in
  
  try
    while true do 
      let step = React.Step.create () in 
      List.iter (fun pidd -> Forked_event.C_handle.send pidd ~step ()) sf; 
      React.Step.execute step
    done 
  with exn -> ( 
      Printf.eprintf "[%6i] : %s \n%!" (Unix.getpid ()) (Printexc.to_string exn); 
      List.iter (fun pidd -> Forked_event.C_handle.terminate pidd) sf 
  )

let run_02 () = 
  
  print_banner "test 2";
  flush stdout;

  let counter_event, counter_send = Prime.create_counter_event 12 in 
  (*
  let small_printer = React.E.map (fun x -> Printf.printf "d:%i\n" x) counter_event in  
  *)
  let counter_send      = Forked_event.C_handle.of_send counter_send in 
  let dispatched_events = Dispatcher.dispatch counter_event 4 in  
  let dispatched_events = Array.map (fun e -> 
    (e, Forked_event.C_handle.noop)
    ) dispatched_events in  
  
  let ph                = Forked_event.P_handle.create () in 
  let forked_events     = Forked_event.P_handle.lock ph (fun () -> 
    Array.map (fun (e,_)  -> 
      Forked_event.E.create ph e string_of_int int_of_string
    ) dispatched_events)  in 

  let printed_events = Array.map (fun (e,ppid) -> 
    React.E.map (fun x -> Printf.printf "[%6i] received %4i\n%!" (Unix.getpid ()) x) e, ppid
    ) forked_events in 
   
  let all_send = counter_send::(Array.to_list @@ Array.map snd printed_events) in 
  
  try
    while true do 
      let step = React.Step.create () in 
      List.iter (fun pidd -> Forked_event.C_handle.send pidd ~step ()) all_send; 
      React.Step.execute step
    done 
  with exn -> ( 
      Printf.eprintf "[%6i] : %s \n%!" (Unix.getpid ()) (Printexc.to_string exn); 
      List.iter (fun pidd -> 
        Forked_event.C_handle.terminate pidd) all_send
  )

let () = 
  run_02 () 
