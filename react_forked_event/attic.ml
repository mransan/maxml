
module Marshal_int32 = struct 

  let size = 32

  let int_to_bytes_unsafe x buf start = 
     let x = Int32.of_int x in 
     let nb_bytes  = Marshal.to_buffer buf start size x [] in 
     if nb_bytes > size 
     then (Printf.eprintf ">> Encoding n:{%i}, required {%i} bytes" (Int32.to_int x) nb_bytes); 
     
     for i=nb_bytes to (size - 1) do
         Bytes.unsafe_set buf i (Char.unsafe_chr 0)
     done;
     size

  let int_of_bytes buf start = 
     Int32.to_int @@ (Marshal.from_bytes buf start:int32) , size ;

end 

module Test(I:Int_encoding_sig) = struct 
  let time_f f = 
    let t1 = Unix.gettimeofday () in 
    let x  = f () in 
    let t2 = Unix.gettimeofday () in 
    (t2 -. t1), x 

 (* ---  Unit Test --- *) 
  let () = 
    let open Int32 in 
    let b = Bytes.create I.size in 
    let t, (_:unit) = time_f (fun () -> 
      for i=0 to to_int (shift_right max_int 5)  do 
        let (_:int)  = I.int_to_bytes_unsafe i b 0 in 
        assert (i=fst (I.int_of_bytes b 0))
      done
    ) in 
    Printf.printf "t: %f\n%!" t
end


module Step_function : sig 
  type t 
  (** Functional data structure to hold all the step functions created with 
      various events.
   *)
  
  val empty : unit -> t 
  (** [empty ()] will create an empty container. Running [run_all] on a empty
      container is a no op
   *)
  
  val add : t -> (?step:React.step -> unit -> unit) -> t
  (** [add step_functions f] returns a new [t] container with the closure [f] 
      added
    *)
  
  val run_all : t -> unit 
  (** [run_all step_functions will start an infinite loop invoking all the step
      functions associated with the current process at each iteration. 
  
      The loop is interupted if one of the step functions raises an exception. 
    *)
end = struct 
  type t = (int * (?step:React.step -> unit -> unit)) list 
  
  let empty () = [] 
  
  let add step_functions f = 
    (Unix.getpid (), f) :: step_functions 
  
  let run_all (t:t)  = 
    let pid = Unix.getpid () in 
    while true do 
      let step = React.Step.create () in 
      List.iter (fun (x, (f:(?step:React.step -> unit -> unit))) -> if x = pid then f ~step ()) t; 
      React.Step.execute step
    done 
end 
