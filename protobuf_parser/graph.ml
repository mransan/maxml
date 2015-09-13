module Int_map = Map.Make(struct 
  type t = int 
  let  compare = Pervasives.compare
end)

type node = {
  id  : int ;
  sub : int list; 
  mutable index : int option; 
  mutable lowlink : int option; 
  mutable on_stack: bool
} 

let create id sub = {
  id; sub; index = None; lowlink = None; on_stack = false 
} 

type graph = node Int_map.t

let reset g = 
  Int_map.iter (fun _ n ->
    n.index <- None; 
    n.lowlink <- None; 
    n.on_stack <- false
  ) g 

module Option = struct 
  
  let some x = Some x 
  
  let value = function | Some x -> x | None -> failwith "Error accessing value from None"

  let min_value = function 
    | None   , None 
    | Some _ , None 
    | None   , Some _ -> failwith "min_value error"
    | Some x , Some y -> some @@ min x y  

  let eq_value = function 
    | None   , None 
    | Some _ , None 
    | None   , Some _ -> failwith "eq_value error"
    | Some x , Some y -> x = y

  let string_of_option f = function 
    | None -> "None"
    | Some x -> Printf.sprintf "Some(%s)" (f x)
end

let rec strong_connect g sccs stack index v = 

  Logger.log "[Graph] processing v [%i], index: %i\n" v.id index; 
  
  v.index   <- Some index; 
  v.lowlink <- Some index; 
  let stack = v::stack in 
  v.on_stack <- true; 

  let sccs, stack, index = List.fold_left (fun (sccs, stack, index) id -> 
    let (w:node)  = Int_map.find id g in 

    Logger.log "[Graph] sub w [%i], w.index: %s\n" 
      w.id (Option.string_of_option string_of_int w.index);
    match w.index with 
    | Some _ -> ( 
      (if w.on_stack 
      then v.lowlink <- Option.min_value (v.lowlink, w.index) 
      else ()
      );
      (sccs, stack, index) 
    )
    | None -> ( 
        let sccs, stack, index = strong_connect g sccs stack (index + 1) w in  
        v.lowlink <- Option.min_value (v.lowlink, w.lowlink);
        (sccs, stack, index) 
    )
  ) (sccs, stack, index) v.sub 
  in 

  Logger.log "[Graph] after sub for v [%i], lowlink: %s, index: %s\n" 
    v.id 
    (Option.string_of_option string_of_int v.lowlink)
    (Option.string_of_option string_of_int v.index);

  Logger.log "[Graph]   -> stack : %s\n" 
    ("[" ^ (String.concat ";" (List.map (fun {id; _ } -> string_of_int id) stack)) ^ "]");
  if Option.eq_value (v.lowlink, v.index) 
  then (
    let scc, stack, _ = List.fold_left (fun (scc, stack, splitted) n -> 
      if splitted 
      then scc, n::stack, splitted
      else (
        n.on_stack <- false; 
        if n.id = v.id 
        then n.id::scc, stack, true
        else n.id::scc, stack, false 
      )
    ) ([], [], false) stack in 
    (scc::sccs, (List.rev stack), index) 
  )
  else (sccs, stack, index) 

let tarjan g = 
  let sccs, _, _ = Int_map.fold (fun _ n (sccs, stack, index) -> 
    match n.index with 
    | Some _ -> (sccs, stack, index) 
    | None   -> strong_connect g sccs stack index n
  ) g ([], [], 0) in  
  sccs


