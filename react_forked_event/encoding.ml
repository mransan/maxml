
module Int_encoding : sig 
  val size : int 
  (** Fixed size of the encoding of an int in bytes *)

  val int_to_bytes_unsafe : int -> bytes -> int -> unit 
  (** [int_to_bytes_unsafe x buf offset] will encode the [x] in the 
      [buf] starting at position [offset]. 
   *)

  val int_of_bytes : bytes -> int -> int 
  (** [int_of_bytes buf offset] decodes an int value from [buf] starting 
      at position [offset]. i

      The function returns the int.
    *)

end = struct 

  let size = 4
    
  let int_to_bytes_unsafe (x:int) (buffer:bytes) (start:int) = 

    let f mask shift_size = 
       Char.unsafe_chr @@  ((x land mask) lsr shift_size) 
    in 
    Bytes.set buffer (start + 0) @@ f 0xFF000000 24;  
    Bytes.set buffer (start + 1) @@ f 0x00FF0000 16;  
    Bytes.set buffer (start + 2) @@ f 0x0000FF00 8;  
    Bytes.set buffer (start + 3) @@ f 0x000000FF 0;
    ()

  let int_of_bytes buffer start = 
    let f i shift_size = 
      (Bytes.get buffer i |> Char.code )lsl shift_size
    in 
    f 0 24 
    lor f 1 16 
    lor f 2 8 
    lor f 3 0

end 

module Fd_util : sig 
  
  val read : Unix.file_descr -> bytes -> int -> int -> unit 
  (** [read fd buf offset length] will read [length] bytes from 
      [fd] and set them in [buf] starting at [offset]. 

      Function raises End_of_file if no bytes can be read from [fd].
   *)

end = struct 

  let rec read fd buff start length =
    if length = 0 
    then () 
    else 
      if length <0 
      then failwith "Programatic error" 
      else
        match Unix.read fd buff start length with
        | 0 -> raise End_of_file
        | n -> read fd buff (start+n) (length-n);;
end 

let write_msg ?buf fd  s = 
  let buf = match buf with 
    | Some b -> b 
    | None   -> Bytes.create Int_encoding.size in 
  let l = String.length s in 
  (* send the message size *)
  let (_:unit) = Int_encoding.int_to_bytes_unsafe l buf 0 in 
  let (actuall_written:int) = Unix.single_write fd buf 0 Int_encoding.size in
  assert (actuall_written = Int_encoding.size);
  (* send the message *) 
  let (actuall_written:int)  = Unix.single_write_substring fd s 0 l in 
  assert (actuall_written = l);
  ()

let read_msg ?buf fd = 
  
  let buf1 =  match buf with 
    | Some b -> b
    | None   -> Bytes.create Int_encoding.size in 
  
  (* read the message size *)
  Fd_util.read fd buf1 0 Int_encoding.size; 
  let string_size = Int_encoding.int_of_bytes buf1 0 in 

  (* read the message *) 
  let buf2, offset2 = match buf with 
    | Some b -> b, Int_encoding.size 
    | None -> Bytes.create string_size, 0  in 
  Fd_util.read fd buf2 offset2 string_size; 
  Bytes.sub_string buf2 offset2 string_size

  
type fork_connection = < 
  write_fd: Unix.file_descr;
  write   : string -> unit; 
  read_fd : Unix.file_descr; 
  read    : string; 
> 

type fork = 
    | Child  of fork_connection  
    | Parent of int * fork_connection 

let fork () = 
    let pc = Pipe_connection.create () in 
    match Unix.fork () with
    | 0 -> 
        Pipe_connection.set `P1 pc; 
        Child  (object(this) 
        method write_fd = Pipe_connection.write_fd `P1 pc; 
        method write s = write_msg this#write_fd s  
        method read_fd = Pipe_connection.read_fd `P1 pc; 
        method read = read_msg this#read_fd; 
    end) 
    | childpid -> 
        Pipe_connection.set `P2 pc; 
        Parent (childpid, (object(this) 
        method write_fd = Pipe_connection.write_fd `P2 pc; 
        method write s = write_msg this#write_fd s  
        method read_fd = Pipe_connection.read_fd `P2 pc; 
        method read = read_msg this#read_fd; 
    end)) 


type selector = {
    selector : Select.selector; 
    holder   : (unit React.event) list ref; 
} 

let create () = {
    selector = Select.create (); 
    holder   = ref [];
}

type read_value = 
    | String of string 
    | Closed


let add_read fork_connection {selector; holder; } = 
    
    let read_ready_e = Select.add_in fork_connection#read_fd selector in 

    let on_close read_fd = 
        Select.remove_in read_fd selector; 
        Some Closed
    in

    let len = ref 4 in  
    let buf = ref @@ Bytes.create !len in  

    let check_buf string_size = 
        let overflow = !len - Int_encoding.size - string_size in 
        if overflow < 0 
        then ( 
            buf := Bytes.extend !buf 0 (- overflow);
            len := Bytes.length !buf 
        )
    in 

    let remaining = ref None in 

    let read_new_string read_fd = 
        match Unix.read read_fd !buf 0 Int_encoding.size with
        | 0 -> on_close read_fd
        | i when i = Int_encoding.size -> (
            let string_size = Int_encoding.int_of_bytes !buf 0 in 
            check_buf string_size; 
            match Unix.read read_fd !buf Int_encoding.size (string_size) with 
            | i when i = string_size -> ( 
                let s = Bytes.sub_string !buf Int_encoding.size string_size in 
                Some (String s)
            )
            | 0 -> on_close read_fd
            | x -> (
                remaining := Some (string_size - x, x); 
                None
            ) 
        )
        | x -> failwith @@ Printf.sprintf "Failed to read msg size (%i)" x
    in 
    
    let read_remaining read_fd (remaining_n, read_n) = 
        match Unix.read read_fd !buf read_n remaining_n with
        | 0 -> on_close read_fd
        | i when i = remaining_n -> (
            remaining := None; 
            let s = Bytes.sub_string !buf Int_encoding.size (read_n + remaining_n) in  
            Some (String s) 
        )
        | n -> (
            remaining := Some ((remaining_n - n), (read_n + n)); 
            None 
        )
    in

    let msg_e = React.E.fmap (fun read_fd -> 
        match !remaining with 
        | None   -> read_new_string read_fd 
        | Some r -> read_remaining  read_fd r 
    ) read_ready_e in 

    holder:=(React.E.fmap (fun _ -> None) msg_e)::!holder;
    
    msg_e

let add_write fork_connection {selector; holder} = 
    let buf = Bytes.create 4 in 

    let msg_queue      = ref [] in 
    (* all the msg waiting to be sent *)

    let remaining_write = ref None in  
    (* the information to keep track of a partial read. When the 
       file descriptor becomes readily available the 
       remaining data is then rather than the next msg in the queue. 
     *)

    let write_new_msg write_fd = 
        match !msg_queue with 
        | []       -> failwith "Programatic error [write_msg_queue is empty]"
        | msg::tl  -> (
            let l = String.length msg in 
            Int_encoding.int_to_bytes_unsafe l buf 0;
            let written = Unix.single_write write_fd buf 0 Int_encoding.size in
            assert (written = Int_encoding.size);
            match Unix.single_write_substring write_fd msg  0 l with 
            | x when x = l -> ( msg_queue :=tl )
            | n            -> ( remaining_write := Some (msg, tl, n, l - n) 
            )
        );
    in

    let write_remaining write_fd (msg, tl, encoded_n, len) = 
        match Unix.single_write_substring write_fd msg encoded_n len with 
        | x when x = len -> (
            remaining_write := None ; 
            msg_queue := tl
        ) 
        | n -> remaining_write := Some (msg, tl, encoded_n + n, len - n)
    in 
    
    let write_f msg = 
        msg_queue := !msg_queue @ [msg];
        if List.length !msg_queue = 1
        then ( 
            (* When a brand new msg is added this is when we need to start
              listening to the file descriptor availability
             *)
            let write_ready_e = Select.add_out fork_connection#write_fd selector in  
            let write_e = React.E.map (fun write_fd -> 
                (match !remaining_write with 
                | Some r -> write_remaining write_fd r 
                | None   -> write_new_msg write_fd);
                if !msg_queue = [] 
                then Select.remove_out write_fd selector
                else ()
            ) write_ready_e in 
            holder := write_e :: !holder; 
        )
        else () 
    in  
    write_f 

let add_fork_connection fork_connection s = 
    let msg_e = add_read fork_connection s in  
    let write_f = add_write fork_connection s in 
    (write_f, msg_e) 
     
let select timeout {selector;} = 
    Select.select timeout selector

let nb_of_writes {selector; _ }  = 
    Select.nb_of_writes selector 
