
module M : sig 
    type t 
    val to_string : t -> string
    val of_string : string -> t 
end = struct 
    type t = { s : string ; i : int } 
    let to_string {s; i } =  s ^ "(" ^ (string_of_int i) ^ ")" 
    let of_string s = {s; i = 0} 
end

module T = M 

module M_alt = struct 
    type t = {s : string  }  
    let to_string {s} = s ^ "(Alt)"
    let of_string s  = {s}
    let of_i _ = {s=""}
end 

module A : (module type of M) = M_alt 

module type A_alt_sig = sig 
    include (module type of M) 
    val of_i : 'a -> t 
end 

module A_alt : A_alt_sig = M_alt 

let () =  

    let t = M.of_string "My test" in 
    Printf.printf "s = %s \n" (M.to_string t); 
    let t = T.of_string "My test (T)" in 
    Printf.printf "s = %s \n" (M.to_string t); 
    let t = A.of_string "My test (A)" in 
    Printf.printf "s = %s \n" (A.to_string t); 
    
    (** cannot compile 
    let t = A.of_i "My test (A)" in 
    *)

    let t = A_alt.of_i 2.3 in 
    Printf.printf "s = %s \n" (A_alt.to_string t); 


