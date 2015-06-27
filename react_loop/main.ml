open Printf 

let hold = ref [] 

let add_to_hold (e:unit React.event) = 
  hold := e::!hold 

type request  = string 

type response = string

module Server = struct 

  type request_handler = {
    request: string; 
    response_time_i : int; 
  }
  
  type t = {
    current_time_i : int; 
    request_handler : request_handler option
  }
  
  let handle_time_e i ({request_handler; _} as t)  send_response =
    match request_handler with 
    |  None ->  {t with current_time_i = i }
    |  Some {request; response_time_i} -> 
      if i = response_time_i 
      then (
        printf "\n+++++response sent%!";
        let (_:unit) = send_response (sprintf "Response of [%s]" request) in 
        {current_time_i = i; request_handler = None; }
      )
      else {t with current_time_i = i} 

  let handle_request r ({current_time_i; request_handler; } as t) send_response  =  
    match request_handler with 
    | Some _ -> (
      eprintf "\n+++++Cannot handle multiple request%!"; 
      t )
    | None -> (
      let response_time_i = 2 + current_time_i + (Random.int 10) in 
      let request_handler = Some {
        request = r; 
        response_time_i;
      } in 
      {t with request_handler } 
    )

  let create () = {current_time_i = -1 ; request_handler = None } 
   
end 


module E = struct 

  let setup_server te request_e send_response = 
    let te = React.E.map (fun i -> `Time i) te in  
    let request_e = React.E.map (fun x -> `Request x) request_e in 
    (add_to_hold @@ 
      let s = ref @@ Server.create () in 
      React.E.merge (fun () x -> match x with 
        | `Time i -> 
          s := Server.handle_time_e i !s send_response 
        | `Request r ->
          s := Server.handle_request r !s send_response   
      ) () [te;request_e]) 
  
  
  let setup_client te send_request = 
    (add_to_hold @@ 
      let next = ref (-1) in 
      React.E.map (fun i -> 
        match !next with 
        | (-1) -> next := i + (Random.int 20) + 2 
        | v    -> ( 
          if v = i 
          then (
              printf "\n+++++request sent%!";
              let () = send_request (sprintf "Request[%3i]" i)  in 
               next := !next + Random.int 20 + 2 ; 
              ()
          )
          else () 
    )) te)
end 

type app = {
  request: request option; 
  response: response option; 
}

let () = 

  let global_step = ref @@ React.Step.create () in   

  let te, send_t = React.E.create () in 
  let request_e, send_request = React.E.create () in 
  let response_e, send_response = React.E.create () in  

  E.setup_client te (fun x -> send_request ~step:!global_step x); 
  E.setup_server te request_e (fun x -> send_response ~step:!global_step x);

  let app_e = React.E.merge (fun app x -> 
    match x with 
    | `Request r  -> {app with request  = Some r } 
    | `Response r -> {app with response = Some r} 
  ) {request=None;response=None}  [
    React.E.map(fun x -> `Request x) request_e;  
    React.E.map(fun x -> `Response x) response_e; 
  ] in    

  add_to_hold @@ (React.E.map (fun {request; response } -> 
    match request with 
    | Some r -> printf "\n  -> Request was received: {%s}" r 
    | None -> (); 
    match response with 
    | Some r -> printf "\n  -> Response was received: {%s}" r 
    | None -> (); 
    printf "%!"
  ) app_e);
    
  for i=0 to 32 do 
      let step = !global_step in 
      global_step := React.Step.create ();
      printf "\n@t=%i%!" i; 
      send_t ~step:step i;
      React.Step.execute step;
  done 

