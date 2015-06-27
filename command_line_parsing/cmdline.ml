(**
 * arg_id is an argument identifier which is can either be defined by 
 * either a short id ("-m") or a long id ("--mode") or both
 *)

type arg_short_id  = string;;
type arg_long_id   = string;;
type arg_full_id   = 
    {
        short_id : arg_short_id;
        long_id  : arg_long_id;
    };;

type arg_id = 
    | Long_id of arg_long_id
    | Short_id of arg_short_id
    | Full_id of arg_full_id;;

let str_match_arg_id str id =
    match id with
    | Long_id s -> str = s
    | Short_id s -> str = s
    | Full_id s -> 
        (str = s.short_id ) || (str = s.long_id);;

type arg_value_type =
    | Switch_value
    | Switch_single
    | Switch_multiple;;

type arg_def = 
    {
        arg_id : arg_id;
        arg_value_type : arg_value_type;
    };;

type arg_value = 
    | Switch of bool
    | Single of string
    | Multiple of string list ;; 

let rec str_matches_one_of_arg_id s arg_ids = 
    let fold c arg_id = 
        c || (str_match_arg_id s arg_id) in 
    List.fold_left fold false arg_ids ;; 

exception MissingValue

let split_list_until l condition = 
    let rec impl l1 l2  = 
        match l1 with 
        | [] -> [], l2
        | hd::tail -> 
            if (condition hd)
            then 
                l1, l2
            else
                impl tail (hd :: l2) in
    let l1, l2 = impl l [] in 
    List.rev l2, l1 ;;

let switch_value_from_strings l arg_value_type f = 
    match arg_value_type with
    | Switch_value -> Switch true, l
    | Switch_single -> 
        let process_single l = 
            match l with 
            | [] -> raise MissingValue
            | hd::tail -> Single hd, tail in 
        process_single l 
    | Switch_multiple -> 
        let l1, l2 = split_list_until l f in 
        Multiple l1, l2;;  

let rec process_strings_with_arg l_i l_o a f = 
    (* let {arg_id; arg_value_type} = a in
     *)
    match l_i with 
    | [] -> (match a.arg_value_type with  
        | Switch_value -> (Some (Switch false) , l_o)
        | Switch_single -> (None, l_o)
        | Switch_multiple -> (Some (Multiple []), l_o))
    | hd::tail -> 
       let does_match = str_match_arg_id hd a.arg_id in 
       if does_match
       then 
           let (v, l) = (switch_value_from_strings 
                                            tail a.arg_value_type f) in 
           (Some v, (List.append l_o l)) 
       else
           let l_o = List.append l_o [hd] in 
           process_strings_with_arg tail l_o a f ;;


let rec string_is_one_of_args arg_l s = 
    match arg_l with
    | [] -> false
    | hd::tail -> 
        if (str_match_arg_id s hd.arg_id)
        then
            true
        else
            string_is_one_of_args tail s ;;

let arg_values_from_strings arg_list strings = 
    let f = string_is_one_of_args arg_list in 
    
    let rec impl arg_list strings out = 
    match arg_list with
    | [] -> out
    | hd::tail -> 
        let (v, strings) = process_strings_with_arg 
                                        strings [] hd f in 
        impl tail strings (List.append out [v]) in 
    impl arg_list strings [];; 

exception ProgramaticError

type arg_callback = arg_value -> unit ;; 

type arg_input = 
    {
        arg_def : arg_def; 
        arg_callback : arg_callback; 
    }

let process_arg_inputs arg_inputs strings = 
    
    let args = List.map (fun i -> i.arg_def) arg_inputs in 
    let values = arg_values_from_strings args strings in 
    
    let rec call arg_inputs = function 
        | [] -> ()
        | hd::tail -> match arg_inputs with 
            | [] -> raise ProgramaticError 
            | hda::taila -> match hd with
                | None -> raise MissingValue 
                | Some v -> hda.arg_callback v;
            (call taila tail) in 
    call arg_inputs values;;

(**
 * Public interface to append arg_inputs
 *)

let append l arg_def arg_callback = 
    List.append l [{arg_def = arg_def; arg_callback = arg_callback}]

let add_single_arg l id c = 
    let arg_def = { arg_id  = id; arg_value_type = Switch_single;} in 
    let arg_callback  = function 
        | Switch _ -> raise ProgramaticError
        | Multiple _ -> raise ProgramaticError 
        | Single  v -> c v  in 
    append l arg_def arg_callback;;

let add_multiple_arg l id c = 
    let arg_def = { arg_id  = id; arg_value_type = Switch_multiple;} in 
    let arg_callback  = function 
        | Switch _ -> raise ProgramaticError
        | Multiple v  -> c v 
        | Single  _ -> raise ProgramaticError in
    append l arg_def arg_callback;;

let add_switch_arg l id c = 
    let arg_def = { arg_id  = id; arg_value_type = Switch_value;} in 
    let arg_callback  = function 
        | Switch v -> c v
        | Multiple _ -> raise ProgramaticError
        | Single  _ -> raise ProgramaticError in
    append l arg_def arg_callback;;



(*****************************************************)

let print_bool_result str expected result = 
    print_string str;
    print_string ": ";
    let b_as_string = match (expected == result) with 
        | true -> "success"
        | false -> "failed" in 
    print_string b_as_string; 
    print_string "\n";;

print_string "********************* cmd line \n";

(**
 * is_string id 
 *)

print_bool_result "test1" 
                  true 
                  (str_match_arg_id "--mode" (Long_id "--mode"));

print_bool_result "test2" 
                  true 
                  (str_match_arg_id "-m" (Short_id "-m"));

print_bool_result "test3" 
                  false
                  (str_match_arg_id "-m" (Short_id "-l"));


let id = {short_id = "-m"; long_id = "--mode"} in 
print_bool_result "test4" 
                  true
                  (str_match_arg_id "-m" (Full_id id));
print_bool_result "test5"
                  false
                  (str_match_arg_id "-l" (Full_id id));
print_bool_result "test6"
                  true
                  (str_match_arg_id "--mode" (Full_id id));;

(** 
 * str_matches_one_of_arg_id
 *)

let ids = [Short_id "-m"] in 
let ids = (Long_id "--list") ::ids in 
print_bool_result "test7"   
                  false
                  (str_matches_one_of_arg_id "--mode" ids); 
print_bool_result "test8"   
                  true
                  (str_matches_one_of_arg_id "--list" ids); 
print_bool_result "test9"   
                  true
                  (str_matches_one_of_arg_id "-m" ids);;

(**
 * switch_value_from_strings
 *)

let f s = false in 
let (v, l) = switch_value_from_strings [] Switch_value f in 
print_bool_result "test10"
                   true
                   (v = (Switch true));
print_bool_result "test11"
                  true
                  (l == []);;

let f s = false in
let (v, l) = switch_value_from_strings ["max"] Switch_single f in 
print_bool_result "test12"
                  true
                  (v = Single "max");
print_bool_result "test13"
                  true
                  (l == []);;

let f s = (s = "max") in 
let l = ["sonia"; "jaja"; "max"; "jp"] in 
let (v, l) = switch_value_from_strings l Switch_multiple f in 
print_bool_result "test14"
                  true
                  (v = Multiple ["sonia"; "jaja"]); 
print_bool_result "test15"
                  true
                  (l = ["max"; "jp"]);;
let f s = (s = "max") in 
let (v, l) = switch_value_from_strings [] Switch_multiple f in 
print_bool_result "test16"
                  true
                  (v = Multiple []); 
print_bool_result "test17"
                  true
                  (l = []);;

let arg = {arg_id = Short_id "-m"; arg_value_type = Switch_single } in 
let l   = ["-m"; "list"] in 
let f   = fun str -> false in 
let (v, l)  =  process_strings_with_arg l [] arg f in 
print_bool_result "test18"
                  true
                  (v = Some (Single "list")); 
print_bool_result "test19"
                  true
                  (l = []);;

let arg = {arg_id = Short_id "-m"; arg_value_type = Switch_single } in 
let l   = ["random"; "-m"; "list"; "after"] in 
let f   = fun str -> false in 
let (v, l)  =  process_strings_with_arg l [] arg f in 
print_bool_result "test20"
                  true
                  (v = Some (Single "list")); 
print_bool_result "test21"
                  true
                  (l = ["random"; "after"]);;
(** test21 is quite a funny cases since it makes a multiple switch 
 *  values span after a single switch is inserted in between 
 *  multiple values
 *  We might want to change that
 *)

let append_short_single l id = 
    List.append l [{arg_id = Short_id id; 
                    arg_value_type = Switch_single}] in 
let append_short_mul l id =  
    List.append l [{arg_id = Short_id id; 
                    arg_value_type = Switch_multiple}] in 
let append_long_single l id  = 
    List.append l [{arg_id = Long_id id; 
                    arg_value_type = Switch_single}] in 
let args = [] in 
let args = append_short_single args "-m" in 
let args = append_short_single args "-l" in 
let args = append_short_single args "-k" in 
let args = append_short_mul    args "-i" in 
let args = append_long_single  args "--long" in 
let l   = ["-i"; "i1"; "i2"; "i3"; 
           "-m"; "list"; 
           "--long"; "the_long";
           "-l"; "element"] in 
let values = arg_values_from_strings args l in 
print_bool_result "test22"
                  true
                  (values = [ 
                      Some (Single "list"); 
                      Some (Single "element"); 
                      None;
                      Some (Multiple ["i1"; "i2"; "i3"]);
                      Some (Single "the_long")]); 



let args = [] in 

let args = add_single_arg args (Short_id "-m") (
    fun str -> print_string "-m "; 
               print_string str ; 
               print_string "\n") in 

let args = add_switch_arg args (Long_id "--enabled") (
   fun b -> 
       if b 
       then print_string "Enabled"
       else print_string "Disabled";
       print_string "\n";) in 

let args = add_multiple_arg args (Short_id "-l") (
    fun l -> 
        print_string "List of size ";
        print_int (List.length l); 
        print_string "\n";) in 

let argv = ["-l"; "l1"; "l2"; "l3"; "--enabled"; "-m"; "list"] in 

process_arg_inputs args argv; 

print_string "******************\n"
