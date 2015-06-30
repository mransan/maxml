open Printf 

(** global side effect event holder so that they don't get 
    garbage collected and continue running
 *)
let hold = ref [] 

let add_to_hold (e:unit React.event) = 
  hold := e::!hold 

type request  = string 

type response = string

(** Define a server state and the various handler for 
   different event. Note that the implementation of the Server
   is done independently of the event system. 
 *)  
module Server = struct 

  type request_handler = {
    request: string; 
    response_time_i : int; 
  }
  
  (** Simplified server state. 
      
      [current_time_i] is keeping track of the current time. Note that this
      could be replaced by a signal to the time event (or better a function 
      passed as an argument to get the current time.) 

      [request_handler] is the data structure which keep tracks of the request 
      (if any) being handled by the service. The current behavior of the server
      is to send the response at a random time after receiving the request. This
      random time is \[min_request_time; max_request_time\]. 
   *)
  type t = {
    current_time_i : int; 
    request_handler : request_handler option
  }

  let min_request_time = 2 

  let max_request_time = 12
  
  let handle_time_e ({request_handler; _} as t) (i:int) (send_response:(response -> unit))  =
    match request_handler with 
    |  None ->  {t with current_time_i = i }
    |  Some {request; response_time_i} -> 
      if i = response_time_i 
      then (
        printf "\n+++++ [server] response sent%!";
        let (_:unit) = send_response (sprintf "Response of [%s]" request) in 
        {current_time_i = i; request_handler = None; }
      )
      else {t with current_time_i = i} 

  let handle_request_e r ({current_time_i; request_handler; } as t) send_response  =  
    match request_handler with 
    | Some _ -> (
      eprintf "\n+++++ [server] Cannot handle multiple request%!"; 
      t )
    | None -> (
      let response_time_i = min_request_time + current_time_i + (Random.int (max_request_time - min_request_time)) in 
      let request_handler = Some {
        request = r; 
        response_time_i;
      } in 
      printf "\n+++++ [server] request is received, response time => [%3i] %!" response_time_i;
      {t with request_handler } 
    )

  (** Initial server state
   *)
  let create () = {current_time_i = -1 ; request_handler = None } 
   
end 


(** Simplified implementation of a client. 
    
    The client behavior is simply to send request at a random 
    time intervals, regardeless if it has received the response
    from a previous request. 

    The response handler is just a logging of the response.
  *)
module Client = struct  

  type t = int 

  let handle_time_e (t:t) (i:int) send_request = 
    match t with 
    | (-1) -> i + (Random.int 20) + 2 
    | t    -> 
      if t = i 
      then (
        printf "\n+++++ [client] request sent%!";
        let () = send_request (sprintf "Request[%3i]" i)  in 
        t + Random.int 20 + 2
      )
      else t 

  (** much simplified response handling ... no
      state update
   *)
  let handle_response_e (r:response) = 
    printf "\n+++++ [client] response received: {%s}" r 

  let create () = - 1
end

(** Logic to connect the various component of this 
    event system
  *)
module E = struct 

  let setup_server te request_e send_response = 
    let te = React.E.map (fun i -> `Time i) te in  
    let request_e = React.E.map (fun x -> `Request x) request_e in 
    (add_to_hold @@ 
      let s = ref @@ Server.create () in 
      React.E.merge (fun () x -> match x with 
        | `Time i -> 
          s := Server.handle_time_e !s i send_response 
        | `Request r ->
          s := Server.handle_request_e r !s send_response   
      ) () [te;request_e]) 
  
  let setup_client te response_e send_request = 
    (add_to_hold @@ 
      let s = ref @@ Client.create () in 
      React.E.map (fun i -> 
        s := Client.handle_time_e !s i send_request 
    ) te); 
    (add_to_hold @@ React.E.map (fun response -> 
      Client.handle_response_e response
    ) response_e) 
end 

type app = {
  request: request option; 
  response: response option; 
}

let () = 

  let next_step = ref @@ React.Step.create () in   

  (** Create 3 independent event. 
      All this events will be synchronize with the next_step 

      * `Time Event` is the application clock which will drive the various 
         random behavior of the different component (Client/Server)
      * `Request/Response Event` essentially defines a `connection` between 2
         components using the event semantic. Using the next_step mechanism
         each connection event will take exactly 1 `Time Event` occurence. 
   *)
  let te, send_t = React.E.create () in 
  let request_e, send_request = React.E.create () in 
  let response_e, send_response = React.E.create () in  

  E.setup_client te response_e (fun x -> send_request ~step:!next_step x); 
  E.setup_server te request_e (fun x -> send_response ~step:!next_step x);
  
  (** Main application loop
   *) 
  for i=0 to 32 do 
      let step = !next_step in 
      next_step := React.Step.create ();
      printf "\n@t=%i%!" i; 
      send_t ~step:step i;
      React.Step.execute step;
  done 

