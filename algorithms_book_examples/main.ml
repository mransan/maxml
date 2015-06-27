open Sort
open Util

let run_sorting_test i f l = 
  let prof_time, sorted_input = time_f (fun () -> f l) in  
  let is_sorted = 
    match sorted_input with 
    | [] -> true 
    | hd::tl ->
      let f (is_sorted, previous) e  = 
        if e >= previous 
        then (is_sorted, e) 
        else (false, e)  
      in
      fst (List.fold_left f (true, hd) tl) 
  in  
  if is_sorted 
  then
    Printf.printf "[%i] (%f): success \n" i (prof_time *. 1000.)
  else 
    begin 
    Printf.printf "[%i] : error \n" i ;
    Printf.printf "[%i] : input %s\n" i (string_of_int_list l);  
    Printf.printf "[%i] : sorted %s\n" i (string_of_int_list sorted_input);
    end;;


let () = 
  let tests = [
    []; 
    [1]; 
    [1;2]; 
    [2;1];
    [1;3;2]; 
    [3;2;1]; 
    [2;1;4;3;6;5;8;7;0];
    (init_list 2000); 
    (init_random_list 2000); 
    (init_random_list 2000); 
  ] in 

  print_test_banner "Insertion Sort"; 
  List.iteri (fun i test -> run_sorting_test i insertion_sort test) tests;
  print_test_banner "Merge Sort"; 
  List.iteri (fun i test -> run_sorting_test i merge_sort test) tests;
  print_test_banner "Merge Sort OCaml"; 
  List.iteri (fun i test -> run_sorting_test i (List.sort Pervasives.compare) test) tests;;



let () = 
  print_test_banner "Sub Array";
  let open Sub_array in 
  let test_fold_right a j i = 
    let f e (e_sum, i_sum) i = 
      (e_sum +. e, i_sum + i)
    in  
    Array_helper.sub_fold_right f a j i (0., 0)
  in

  let a = [| 1.; 2.; 3.; 4.; |] in 

  let e_sum, i_sum = test_fold_right a 2 0 in  
  print_test  "(sub_fold_right 0) e_sum" (e_sum = 6.);
  print_test  "(sub_fold_right 0) i_sum" (i_sum = 3 ); 

  let e_sum, i_sum = test_fold_right a 0 0 in  
  print_test "(sub_fold_right 1) e_sum" (e_sum = 1.);
  print_test "(sub_fold_right 1) i_sum" (i_sum = 0 ); 
  
  let e_sum, i_sum = test_fold_right a 3 1 in  
  print_test "(sub_fold_right 2) e_sum" (e_sum = 9.);
  print_test "(sub_fold_right 2) i_sum" (i_sum = 6 ); 
  
  let test_fold_left a i j = 
    let f (e_sum, i_sum) e i = 
      (e_sum +. e, i_sum + i)
    in  
    Array_helper.sub_fold_left f (0., 0 ) a i j  
  in

  let e_sum, i_sum = test_fold_left a 0 2 in  
  print_test "(sub_fold_left 0) e_sum" (e_sum = 6.);
  print_test "(sub_fold_left 0) i_sum" (i_sum = 3 ); 

  let e_sum, i_sum = test_fold_left a 0 0 in  
  print_test "(sub_fold_right 1) e_sum" (e_sum = 1.);
  print_test "(sub_fold_right 1) i_sum" (i_sum = 0 ); 
  
  let e_sum, i_sum = test_fold_left a 1 3 in  
  print_test "(sub_fold_right 2) e_sum" (e_sum = 9.);
  print_test "(sub_fold_right 2) i_sum" (i_sum = 6 ); 

  let max_left, max_i = Details.max_left a 0 2 in 
  print_test "(max_left 0) max_left" (max_left = 6.);  
  print_test "(max_left 0) max_i"    (max_i    = 0);  
  
  let max_right, max_i = Details.max_right a 1 3 in 
  print_test "(max_right 0) max_right " (max_right = 9.);  
  print_test "(max_right 0) max_i     " (max_i     = 3);  
  
  let a = [| -. 1.; 2.; 3.; -4.; |] in 
  let max_left, max_i = Details.max_left a 0 2 in 
  print_test "(max_left 1) max_left" (max_left = 5.);  
  print_test "(max_left 1) max_i   " (max_i    = 1);  
  
  let max_right, max_i = Details.max_right a 0 3 in 
  print_test "(max_right 1) max_right" (max_right = 4.);  
  print_test "(max_right 1) max_i    " (max_i     = 2);  

  let low, high, sum = find_max a in 
  print_test "(find_max 0) low " (low  = 1); 
  print_test "(find_max 0) high" (high = 2); 
  print_test "(find_max 0) sum " (sum  = 5.); 

  let a = [| 5. ; 5.; -1.; -1.; |] in 
  let low, high, sum = find_max a in
  print_test "(find_max 1) low  " (low  = 0); 
  print_test "(find_max 1) high " (high = 1); 
  print_test "(find_max 1) sum  " (sum  = 10.); 
  
  let a = [| 5. ; -1.; -1.; -1.; |] in 
  let low, high, sum = find_max a in
  print_test "(find_max 2) low " (low  = 0); 
  print_test "(find_max 2) high" (high = 0); 
  print_test "(find_max 2) sum " (sum  = 5.); 
  
  let a = [| -1. ; -1.; 1.; 1.; |] in 
  let low, high, sum = find_max a in 
  print_test  "(find_max 3) low  " (low  = 2); 
  print_test  "(find_max 3) high " (high = 3); 
  print_test  "(find_max 3) sum  " (sum  = 2.); 

  let a = Array.make  1_000_000 1. in 
  Array.iteri (fun i _ -> Array.set a i (float_of_int (i+1))) a ; 
  let t, (low, high, _ ) = time_f (fun () -> find_max a) in 
  print_test "(find_max big) low   " (low  = 0); 
  print_test "(find_max big) high  " (high = (Array.length a -1)); 

  let get_performance n = 
    let a = Array.make  n 1. in 
    Array.iteri (fun i _ -> Array.set a i (float_of_int (i+1))) a ; 
    fst @@ time_f (fun () -> find_max a)  
  in 

  let limit = 10_000 in 
  let rec loop i = 
    if i = limit 
    then ()
    else begin
      Printf.printf "%d: %f seconds \n" i (get_performance i);
      loop (i * 10)
    end 
  in
  loop 1;; 


let () = 
  let open Heap in 

  print_test_banner "Heap Data Structure";

  let h = init 1_000 10 in 
  print_test "(init) root" ((value_of_e (root h)) = 10); 
  insert h 11;
  let r = root h in 
  print_test "(insert 11) root" ((value_of_e r) = 11); 
  let left = left_child_of_e h r in 
  print_test "(insert 11) left has value " (is_some left);
  print_test "(insert 11) left " (value_of_e @@ option_value left = 10);
  
  let rec is_valid e h = 
    let l = left_child_of_e h e in 
    let r = right_child_of_e h e in 
    let v = value_of_e e in 
    if (is_some l && value_of_e @@ option_value l > v) 
    then begin 
       Printf.printf "error left child: %d > %d \n" 
         (value_of_e @@ option_value l) v ; 
       false
    end
    else if  (is_some r && value_of_e @@ option_value r > v) 
    then begin 
      Printf.printf "error right child %d > %d \n"
      (value_of_e @@ option_value r) v;  
         false
    end 
    else (is_none l || is_valid (option_value l) h) && 
         (is_none r || is_valid (option_value r) h) 
  in 
  print_test "(insert 11) is_valid" (is_valid r h); 
  Random.self_init () ; 

  let rec f = function 
    | 0 -> ()
    | i   -> begin insert h @@ Random.int 1000 ; f (i - 1) end  
  in 
  f 800; 
  let ok = is_valid (root h) h in
  if not ok 
  then Printf.printf "heap %s \n" (string_of_t h string_of_int)
  else ();
  print_test "(random) is_valid" ok ;
  print_test "(random) size    " (length h = 802);; 


(** Doubly Linked List *)


let () = 
  print_test_banner "Doubly Linked List";

  let open Dl_list in 
  let l = create () in 
  
  let test_l = {front = ref Nil; back = ref Nil; } in 
  print_test "(Dl_list init) Nil/Nil" (l = test_l) ;  

  print_test "(Dl_list empty) " (empty test_l) ; 
  
  let () = push_front l 3 in 
  let b0, b1  = match !(l.front) with
    | Nil -> false, false  
    | Link {next; prev; d } -> 
      match !next, !prev, d with 
      | Nil, Nil, 3 ->  true, true
      | _, _, _     -> true, false 
  in
  let {front;back} = l in 
  print_test "(Dl_list push_front 3) Not Nil" b0; 
  print_test "(Dl_list push_front 3) Link   " b1; 
  print_test "(Dl_list push_front 3) front = back" (!front = !back); 

  let () = push_front l 4 in 
  let {front; back} = l in 
  let b0, b1, b2= match !front with  
    | Nil -> false, false, false 
    | Link {next; prev; d} ->
      match !next, !prev, d with 
      | Link {next;prev;d }, Nil, 4 -> 
        begin 
        match !next, !prev,  d with 
        | Nil, Link {d=4; _ }  , 3 ->  true, true, true
        | _  , _     , _ ->  true, true, false
        end
      | _      , _ , _ -> true, false, false
  in
  print_test "(Dl_list push_front 4) Not Nil" b0; 
  print_test "(Dl_list push_front 4) Link  0" b1; 
  print_test "(Dl_list push_front 4) Link  1" b2; 

  let rec setup_push limit push l = function  
   | i when i = limit  -> ()
   | i    -> push l i; setup_push limit push l  (i+1) 
  in 

  let rec verify_pop pop ((success, l) as acc) = function 
   | 0  -> acc
   | i -> 
     begin 
       match pop  l with
       | Some v  -> 
         if v = (i - 1)  
         then verify_pop pop  (true, l) (i - 1) 
         else begin 
           Printf.printf "error i = %d != %d \n" i v;
           verify_pop pop (false, l) (i - 1)
         end
       | None -> begin 
          Printf.printf "!! List underflow \n"; false, l;
       end
     end
  in 

  let k = 1000 in 

  let l  = create () in 
  let () = setup_push k push_front l 0 in 
  let b, _ = verify_pop pop_front (true, l) k in 
  print_test "(Dl_list pop_front) " b;

  let b = match pop_front l with
    | Some x -> Printf.printf "Error >> %d \n" x ; false
    | None   -> true in 
  print_test "(Dl_list pop_front) Null" b;
  
  let l = create () in 
  let () = setup_push k push_back l 0 in 
  let b, l = verify_pop pop_back (true, l) k in 
  print_test "(Dl_list pop_back) " b;

  let l  = create ()  in 
  let () = setup_push 10 push_front l 0 in  
  let v  = fold_left (fun acc x -> acc + x) 0 l in 
  let v' = 9+8+7+6+5+4+3+2+1 in 
  print_test "(Dl_list fold_left)" (v = v');
  let v = fold_right (fun x acc -> acc + x) l 0 in 
  print_test "(Dl_list fold_right)" (v = v');; 


let () = 
  print_test_banner "Lazy List"; 

  let open Lazy_list in 
  
  let l = create () in 
  print_test "(Lazy_list empty 1)" (empty l);

  let l = push_front 1 l in 
  print_test "(Lazy_list empty 2)" (not (empty l));

  let l1 = create ()  |> push_front 4 |> push_front 3 in 
  let l2 = create ()  |> push_front 2 |> push_front 1 in 
  let l  = append l2 l1 in 
  
  let f (prev, ordered, sum) e = 
    match prev with 
    | None   -> (Some e, true, sum + e) 
    | Some p -> (Some e, ordered && (p < e), sum +e) 
  in 
  let _, ordered, sum = fold_left f (None, true, 0) l in 
  print_test "(Lazy_list fold_left) ordered" ordered; 
  print_test "(Lazy_list fold_left) sum" (sum = 10); 


  let measure i = 
    let t1, l1 = time_f @@ (fun () -> make i 1) in 
    let t2, l2 = time_f @@ (fun () -> make i 2) in 
    let t3, l3 = time_f @@ (fun () -> append l1 l2) in 
    
    let f sum e = sum + e in 
    let t4, sum = time_f @@ (fun () -> fold_left f 0 l3) in 
    (*
    print_test "(Lazy_list performance) sum" (sum = i * 3) ; 
     *)
    (i, t1, t2, t3, t4)
  in 

  let print_performance (i, t1, t2, t3, t4) = 
    Printf.printf "[%10d] %f3.3 %f3.3 %f3.3 %f3.3 \n" i t1 t2 t3 t4; 
  in 

  print_performance (measure 10_000); 
  print_performance (measure 100_000); 
  print_performance (measure 1_000_000);; 

module type Queue_sig = sig
  type 'a t 
  val create    : unit -> 'a t
  val empty     : 'a t -> bool 
  val push_back : 'a t -> 'a -> 'a t
  val pop_front : 'a t -> 'a option * 'a t
end

module Q_tester = functor (Q:Queue_sig) -> struct 

  let test () = 
    let q = Q.create () in
    print_test "(Queue empty) empty " (Q.empty q); 
    let q = Q.push_back q 3 in 
    print_test "(Queue empty) not empty " (not @@ Q.empty q); 

    let v, q = Q.pop_front q in 
    
    print_test "(Queue pop_front of 1) has_value " (is_some v); 
    print_test "(Queue pop_front of 1) value" (option_value v = 3);  
    print_test "(Queue pop_front of 1) empty after " (Q.empty q);  

    let rec setup_queue acc limit = function 
      | i when i = limit -> acc 
      | i -> setup_queue (Q.push_back acc i) limit (i + 1) 
    in 

    let rec assert_ordered = function 
      | (None  , q) , i, limit ->  assert_ordered ((Q.pop_front q), i+1, limit)  
      | (Some x, q) , i, limit -> 
        if i = limit 
        then true 
        else let y, q = Q.pop_front q in is_some y && option_value y > x && assert_ordered ((y, q), i+1, limit)  
    in 
    
    let k = 1000 in 
    let q = setup_queue (Q.create ()) k 0 in 
    print_test "(Queue pop_front 1000) not empty " (not @@ Q.empty q);  
    let b = assert_ordered ((None, q), 0, k) in 
    print_test "(Queue pop_front 1000) ordered " b; 
    let b = assert_ordered ((None, Q.push_back q 1), 0, k + 1) in 
    print_test "(Queue pop_front 1001) not ordered " (not b);;

  type action =
    | Push_back of int 
    | Pop_front 

  let generate_random_actions n = 
    fold_i (fun actions i -> 
      if Random.int 2 = 0 
      then (Push_back i)::actions
      else Pop_front::actions  
    ) [] (0, n) 

  let generate_push_back_actions n = 
    fold_i (fun actions i -> (Push_back i)::actions)  [] (0, n) 


  let run_actions actions = 
    let apply_action q = function 
      | Push_back i -> Q.push_back q i 
      | Pop_front -> let _, q = Q.pop_front q in q 
    in
    let x, _ = time_f (fun () -> List.fold_left apply_action (Q.create ()) actions) in 
    x 
     
  let performance_report which_report = 
    let f n = 
      let actions = match which_report with 
        | `Push_back -> generate_push_back_actions n 
        | `Random    -> generate_random_actions n in 
      run_actions actions 
    in 

    let report = fold_i 
      (fun (x, r) i -> let x = x * 10 in x, (x,f x)::r)  
      (1, [])
      (1, 6) in 
    List.rev (snd report)

end

(** Queue *)
let () = 
  
  print_test_banner "Simple Queue";
  let module Simple_queue_test = Q_tester(Queue) in 
  Simple_queue_test.test ();
  print_test_banner "Lazy Queue";
  let module Lazy_queue_test = Q_tester(Lazy_queue) in 
  Lazy_queue_test.test ();
  print_test_banner "Rotate Queue";
  let module Rotate_queue_test = Q_tester(Rotate_queue) in 
  Rotate_queue_test.test ();
  print_test_banner "Real Time  Queue";
  let module Rt_queue_test = Q_tester(Rt_queue) in 
  Rt_queue_test.test ();

  let do_report which_report = 
    let sq_report = Simple_queue_test.performance_report which_report in 
    let lq_report = Lazy_queue_test.performance_report which_report in 
    let rq_report = Rotate_queue_test.performance_report which_report in 
    let rt_report = Rt_queue_test.performance_report which_report in 
    
    let p = Printf.printf in 

    p "           %s | %s | %s | %s\n" "Simple Queue" "Lazy Queue" "Rotate Queue" "Real Time Queue" ;
    let rec loop = function 
      | (x,a)::al, (_,b)::bl, (_,c)::cl, (_,d)::dl -> 
        let percent x = 100. *. (x -. a) /. a in
        p "%10d %2.7f   | %2.7f (%-3.0f) | %2.7f (%-3.0f) | %2.7f (%-3.0f)\n" x
        a b (percent b) c (percent c)  d (percent d); 
        loop (al, bl, cl, dl) 
      | [],_,_,_ -> () 
      | _ -> failwith "error"
    in
    loop (sq_report, lq_report, rq_report, rt_report) 
  in 

  (*
  do_report `Push_back; 
  do_report `Random;;  
  *)
  ();;


  
let p = Printf.printf;;

let () = 

  print_test_banner "Rotate Queue";
  let open Rotate_queue in 

  let q = create () in 
  print_test "(Rotate_queue) empty " (empty q);
  
  let q = push_back q 1 in 
  print_test "(Rotate_queue) push_back -> empty" (not (empty q)); 
  ();;


(** Random List *)

let () = 
  print_test_banner "Random List"; 

  let open Random_list in 
  let string_of_list = string_of_list string_of_int in  

  let n = 10 in 

  let l = fold_i (fun acc x -> 
    let l = push_front x acc in 
    Printf.printf "List [%2d..0] = %65s | hd = %2d \n" 
      x (string_of_list l) (hd l);
    let s = (fold_i (fun s i -> 
        s ^ (string_of_int (nth i l)) ^ "; "
    ) "[" (x, 0)) ^ "]" in 
    Printf.printf ">> %s \n" s;  
    l) (create ()) (0, n) in
  let _ = fold_i (fun l x -> 
    Printf.printf "List [%2d..0] = %65s | hd = %2d \n" 
      x (string_of_list l) (hd l);
    pop_front l
  ) l (n, 0) in 
  ();;  
     

(** Binary Number *) 

let () = 
  print_test_banner "Binary Number"; 

  let open Binary_number in 

  let n = 10 in  
  print_endline "+++ Increment";
  let x = fold_i (fun x i ->
    let x = inc x  in 
    Printf.printf "%2d = %20s \n" i (string_of x); 
    x 
  ) zero (1, n) in 
  print_endline "--- Decrement";
  let _ = fold_i (fun x i ->
    Printf.printf "%2d = %8s (%2d) \n" i (string_of x) (to_int x); 
    let x = dec x  in 
    x 
  ) x (n, 1) in 
  ();;
