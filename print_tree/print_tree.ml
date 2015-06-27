
type tree = 
  | Empty
  | Node of (int * tree list ) ;;

let rec string_of_tree t = 
  let string_of_children = function 
    | [] -> ""
    | children -> 
      let rec impl = function  
        | []       -> ""
        | hd::tail -> (string_of_tree hd) ^ (impl tail) 
      in 
      " of (" ^ (impl children) ^ " )"
  in 
  
  match t with 
  | Empty -> ""
  | Node (v, children) -> 
    let s = " " ^ (string_of_int v) in 
    s ^ (string_of_children children) ;;



let rec format_tree t ll  = 
  let format_children children tail =
    List.fold_left (fun acc e -> format_tree e acc) tail children 
  in
  let format_node n ll = 
    let v, children = n in 
    let hd, tail = match ll with 
    | [] -> [v],  [] 
    | hd::tail -> (hd @ [v]) , tail in 
    [hd] @ (format_children children tail) 
  in 
  match t with 
  | Empty   -> []
  | Node  n -> (format_node n ll) ;;

let string_of_list_of_list ll = 
  let fold1 acc e =
    let fold2 acc e = 
      acc ^ " " ^ (string_of_int e) in 
    List.fold_left fold2 (acc ^ "\n") e in 
  List.fold_left fold1 "" ll ;;
        
let leaf_of_int i = Node (i,  []) in 
let level_1_tree i l = 
    Node (i, List.map (fun j -> leaf_of_int j ) l) in  
let children = leaf_of_int 1 :: [] in 
let children = level_1_tree 2 [3;4;5] :: children in 
let children = level_1_tree 6 [7;8;9] :: children in
let t = Node (10, children) in 

print_string ("tree: " ^ (string_of_tree t ));
print_string "\n";
print_string ("tree: " ^ (string_of_list_of_list (format_tree t [])));;

