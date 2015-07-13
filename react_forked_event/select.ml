
type event_handler = (?step:React.step -> Unix.file_descr -> unit) 

type selector_i  = {
    in_fds : (Unix.file_descr * event_handler) list ;
    out_fds : (Unix.file_descr * event_handler) list ;
    holders_fds : (Unix.file_descr * unit React.event) list;
} 

type selector = selector_i ref 

let create () = ref {
    in_fds = []; 
    out_fds = []; 
    holders_fds = [];
} 

let filter_out l fd = 
    List.filter (fun x -> fst x <> fd) l 

let add_in fd selector = 
    let {in_fds; _ } as selector_i = !selector in  
    let e, event_handler = React.E.create () in 
    selector := {selector_i with in_fds = (fd, event_handler)::in_fds}; 
    e 

let remove_in fd selector = 
    let {in_fds; holders_fds; _ } as selector_i = !selector in  
    selector := {selector_i with 
        in_fds = filter_out in_fds fd; 
        holders_fds = filter_out holders_fds fd
    } 

let add_out fd selector = 
    let {out_fds; _ } as selector_i = !selector in  
    let e, event_handler = React.E.create () in 
    selector := {selector_i with out_fds = (fd, event_handler)::out_fds}; 
    e 

let remove_out fd selector = 
    let {out_fds;holders_fds;  _ } as selector_i = !selector in  
    selector := {selector_i with 
        out_fds = filter_out out_fds fd; 
        holders_fds = filter_out holders_fds fd;
    } 

let add_side_effect_event fd event selector = 
    let ({holders_fds; _ } as selector_i) = !selector in 
    selector := {selector_i with holders_fds = (fd, event)::holders_fds }

type select_status = 
    | Event_happened 
    | Timeout 
    | No_fds 

let select timeout selector  = 
    let {in_fds; out_fds; } = !selector in 
    
    if List.length in_fds = 0 && List.length out_fds = 0 
    then No_fds
    else 
        let get_fds x = List.map fst x in 
    
        let in_fds', out_fds', _ = 
            Unix.select (get_fds in_fds) (get_fds out_fds) [] timeout in   
        
        let step = React.Step.create () in 
        let trigger handlers = List.fold_left (fun () fd -> 
            try 
                let (handler:event_handler) = List.assoc fd handlers in  
                let (_:unit) = handler ~step fd in 
                ()  
            with Not_found -> failwith "Programmatic error"
        ) () 
        in 

        let (_:unit) = trigger in_fds in_fds' in 
        let (_:unit) = trigger out_fds out_fds' in
        let (_:unit) = React.Step.execute step in 
        
        if List.length in_fds' = 0  && List.length out_fds' = 0 
        then Timeout 
        else Event_happened  

let nb_of_writes selector = 
    let {out_fds; _ } = !selector in 
    List.length out_fds
