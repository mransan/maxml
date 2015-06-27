module T : sig
    class c : object 
        method int : int 
    end 
end = struct 
    class c = object 
        method int = 2
    end 
end 

let () = 
  let o = new T.c in 
  Printf.printf "int from object: %i \n" (o#int)


module U : sig
    type o = <
      int : int 
    >
    val get_o: unit -> o 

end = struct 
    type o = <
      int : int 
    >
    let get_o () = object 
        val x  = 2
        method int = x 
    end 

end 

let () = 
  Printf.printf "int from object: %i \n" @@ (U.get_o ()) # int


module V : sig
 type 'a o = < next : 'a ; is_last : bool >   
 val get_o : 'a list -> 'a o

end = struct 
 type 'a o = < next : 'a ; is_last : bool >   

 let get_o l  = (object 
   val l = ref l  

   method next = match !l with 
     | [] -> raise Not_found 
     | hd::tl -> l := tl ; hd 

   method is_last = match !l with 
     | [] -> true
     | _  -> false
   method x  =  2 end :> 'a o)
end  

let () = 
  let open V in 
  let o = get_o [1;] in 
  Printf.printf "is_last: %b, next: %i, is_last: %b\n"  o#is_last o#next o#is_last;
  let o = get_o [1;2;] in 
  Printf.printf "next: %i, is_last: %b \n" o#next o#is_last ;
    
