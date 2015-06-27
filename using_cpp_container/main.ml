
(** {1 C++ std::map<int, value*> bindings } *) 

type 'a map
external int_value_map_empty : unit -> 'a map = "int_value_map_empty"
external int_value_map_is_empty : 'a map -> bool = "int_value_map_is_empty"
external int_value_map_add : 'a map -> int -> 'a -> unit = "int_value_map_add"
external int_value_map_size : 'a map -> int = "int_value_map_size"
external int_value_map_get : 'a map -> int -> 'a option = "int_value_map_get"
external int_value_map_exist : 'a map -> int -> bool = "int_value_map_exist"

(** {1 Utilities } *)  

module Std_option = struct 
  let exec f  = function | None -> () |  Some x -> f x 
end 

let time_f f = 
  let t1 = Unix.gettimeofday () in 
  let x  = f () in 
  let t2 = Unix.gettimeofday () in 
  (t2 -. t1), x 

let fold_i f e0 (b, e) = 
  let inc = if b>e then (-) else (+) in 
  let rec loop acc = function 
    | i when i = e -> f acc i 
    | i -> loop (f acc i) (inc i 1) 
  in 
  loop e0 b 

(** {1 Testing framework *} *) 

module type Map_i = sig 
  type 'a t 

  val empty : unit -> 'a t 
  val add : 'a t -> int -> 'a -> 'a t
  val get : 'a t -> int -> 'a option 
  val exist : 'a t -> int -> bool
end 
(** Map interface required to have unified testing *) 

module Map_tester = functor(M:Map_i) -> struct 

  let create_test_map size = 
    let f m i = 
      M.add m i (Printf.sprintf ">>%d<<" i)
    in 
    fold_i f (M.empty ())  (1,size)  

  let test_add () = 
    let t, _ = time_f (fun () -> create_test_map 1_000_000)  in 
    t

  let test_get () = 
    let m = create_test_map 1_000_000 in 
    let f () i = 
      let _ = M.get m i in 
      ()
    in 
    let t, _ = time_f (fun () -> fold_i f () (1, 1_000_000)) in 
    t
  
  let test_exist () = 
    let m = create_test_map 1_000_000 in 
    let f () i = 
      let _ = M.exist m i in 
      ()
    in 
    let t, _ = time_f (fun () -> fold_i f () (1, 1_000_000)) in 
    t

end  
(** Actual suite of performance tests *)

let () = 

  let module Stl_int_map = Map.Make(struct 
    type t = int 
    let compare = Pervasives.compare 
  end) in 

  let module Stl_i = struct 
    type 'a t = 'a Stl_int_map.t 

    let empty ()   = Stl_int_map.empty
    let add m i v  = Stl_int_map.add i v m  
    let get m i = 
      try Some (Stl_int_map.find i m) 
      with _ -> None  
    let exist m i = Stl_int_map.mem i m  
  end in 

  let module Cpp_i = struct 
    type 'a t = 'a map 
    let empty = int_value_map_empty
    let add m i v = 
      int_value_map_add m i v; 
      m 
    let get   = int_value_map_get
    let exist = int_value_map_exist 
  end in 

  let module Stl_tester = Map_tester(Stl_i) in 
  let module Cpp_tester = Map_tester(Cpp_i) in 
  Printf.printf "(add) Stl = %3.5f \n" (Stl_tester.test_add ());
  Printf.printf "(add) Cpp = %3.5f \n" (Cpp_tester.test_add ());
  Printf.printf "(get) Stl = %3.5f \n" (Stl_tester.test_get ());
  Printf.printf "(get) Cpp = %3.5f \n" (Cpp_tester.test_get ());
  Printf.printf "(exist) Stl = %3.5f \n" (Stl_tester.test_exist ());
  Printf.printf "(exist) Cpp = %3.5f \n" (Cpp_tester.test_exist ())
