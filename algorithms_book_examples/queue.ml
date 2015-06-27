




type 'a t = {
  front : 'a list; 
  back  : 'a list;
}

let create () = 
  { front = [] ; back = [] ; }

let empty {front ; back } = 
  match front, back with 
  | [], [] -> true
  | _ , _  -> false

let push_back {front;  back } e = 
  {front;back = e::back }

let pop_front ({front; back} as q)  = 
  match front with 
  | hd::front -> Some hd, {front; back} 
  | []        -> 
    match List.rev back with 
    | hd::front -> Some hd , {front; back = [] } 
    | [] ->  None, q  


