
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

let holder = ref []

let run_03 () =

    let nb_of_child = 100 in

    let selector = Forked_util.create () in

    let events =
        let rec loop events = function
        | 0 -> events
        | i -> (
            match Forked_util.fork () with
            | Forked_util.Child connection -> (
                Random.self_init ();
                (*Unix.sleep (Random.int 3 + 1); 
                 *)
                let selector     = Forked_util.create () in 
                let write_msg, _ = Forked_util.add_fork_connection connection selector in 
                for i=1 to 100  do
                    let len = Random.int 100_000 + 10 in 
                    let len = 100_000 in 
                    let c   = Char.chr @@ Random.int 20 + 65 in 
                    let msg = String.make len c in 
                    write_msg msg 
                done;
                let rec loop () = 
                    match Forked_util.select 0.1 selector with 
                    | Select.Timeout | Select.No_fds -> (
                        if Forked_util.nb_of_writes selector = 0 
                        then (
                            Printf.printf "Child [%10i] exiting\n%!" (Unix.getpid()) ; 
                            exit 0 
                        )
                        else loop ()
                    )
                    | Select.Event_happened -> loop ()  
                in 
                loop () 
            )
            | Forked_util.Parent (childpid , connection) -> (
                Unix.close connection#write_fd;
                let _, event = Forked_util.add_fork_connection connection selector in
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
        | Forked_util.String s -> 
            let len = String.length s in 
            (Printf.sprintf "child [%6i] received : [%10i]" childpid len)::l  
        | Forked_util.Closed   -> l
    ) [] events in

    let counter_s =
        let e = React.E.merge (fun i (_, read_value) -> match read_value with 
            | Forked_util.Closed   -> i+1
            | Forked_util.String _ -> i ) 0 events in
        let e = React.E.fold (fun n i -> n  - i) nb_of_child e in
        React.S.hold nb_of_child e in

    let merger_e = React.E.fmap (function | [] -> None | l -> Some l) merger_e in

    let (printer:unit React.event) = React.E.map (fun l ->
        print_endline @@ String.concat "," l;
        print_endline "-----------------";
        flush stdout
    ) merger_e in

    holder := printer::!holder; 

    while React.S.value counter_s <> 0 do
        let (_:Select.select_status) = Forked_util.select 1. selector in 
        ()
    done;
    ()

let () =
  run_03 ()
