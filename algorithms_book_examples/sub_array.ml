
module List_helper = struct 

  let list_max compare = function 
    | [] -> raise Not_found 
    | hd::tl -> 
      let rec loop acc = function
        | [] -> acc 
        | hd::tl -> 
            if compare hd acc = 1 
            then loop hd  tl 
            else loop acc tl 
      in
      loop hd tl 

end 


module Array_helper = struct 

  exception Invalid_indices
  
  let sub_fold_left f e0 a i j = 
    let rec f' acc i = 
      let acc = f acc (Array.get a i) i in 
      if i = j 
      then acc
      else f' acc (i + 1)
    in
    if i > j 
    then raise Invalid_indices 
    else f' e0 i 
  
  let sub_fold_right f a j i e0 =   
    let rec f' acc j = 
      let acc = f (Array.get a j) acc j in 
      if i = j 
      then acc
      else f' acc (j - 1)
    in
    if i > j 
    then raise Invalid_indices
    else f' e0 j 
end
    

module Details = struct 
  let max_running_sum e (max, max_i, sum) i = 
    let sum = sum +. e in 
    if sum > max
    then sum, i, sum 
    else max, max_i, sum 

  let max_left a i j = 
    let f = max_running_sum in  
    let x, y, _  = Array_helper.sub_fold_right f a j i  (-. infinity, 0, 0.) in 
    x, y  
  
  let max_right a i j = 
    let f acc e i = max_running_sum e acc i in 
    let x, y, _ = Array_helper.sub_fold_left f  (-. infinity, 0, 0.) a i j in 
    x, y   
end 

let find_max_crossing_array a low mid high = 
  let left_sum, left_i   = Details.max_left a low mid in 
  let right_sum, right_i = Details.max_right a (mid + 1) high in 
  (left_i, right_i, left_sum +. right_sum)

let find_max a = 
  let rec loop a low high = 
    if low = high
    then (low, high, Array.get a low)
    else 
      let mid = (low + high) / 2 in 
      let l = loop a low mid in 
      let r = loop a (mid + 1) high in 
      let m = find_max_crossing_array a  low mid high in 
      let compare (_, _, x) (_,_,y) = Pervasives.compare x y in 
      List_helper.list_max compare  [l;r;m] 
  in
  loop a 0 (Array.length a - 1)
