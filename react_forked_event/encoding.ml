
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
    (* single buffer object to handle all the read from that connection 
     *)

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



module Read = struct 

    type value = 
        | String of string 
        | Closed
    
    type state = {
        mutable len : int;
        mutable buf : bytes; 
        mutable remaining : (int * int) option; 
    }
    
    let create_state () = {
        len = 4;
        buf = Bytes.create 4; 
        remaining = None; 
    }
    
    let state_check_size ({len; buf; remaining=_} as state) s = 
        let overflow = len - s in 
        if overflow < 0 
        then ( 
            state.buf <- Bytes.extend buf 0 (- overflow);
            state.len <- Bytes.length buf 
        )
    
    type status = 
        | Closed
        | Partial 
        | Complete of string  
    
    let read_new state fd = 
        match Unix.read fd state.buf 0 Int_encoding.size with
        | 0 -> Closed
        | i when i = Int_encoding.size -> (
            let string_size = Int_encoding.int_of_bytes state.buf 0 in 
            state_check_size state @@ string_size + Int_encoding.size; 
            match Unix.read fd state.buf Int_encoding.size string_size with 
            | i when i = string_size -> ( 
                let s = Bytes.sub_string state.buf Int_encoding.size string_size in 
                Complete s
            )
            | 0 -> Closed
            | x -> (
                state.remaining <- Some (string_size - x, x); 
                Partial
            ) 
        )
        | x -> failwith @@ Printf.sprintf "Failed to read msg size (%i)" x
    
    let read_remaining state (remaining_n, n) fd = 
        match Unix.read fd state.buf n remaining_n with
        | 0 -> Closed
        | i when i = remaining_n -> (
            state.remaining <- None; 
            let s = Bytes.sub_string state.buf Int_encoding.size (n + remaining_n) in  
            Complete s 
        )
        | n -> (
            state.remaining <- Some ((remaining_n - n), (n + n)); 
            Partial 
        )
    
    let read ({remaining; _ } as state) read_fd = 
        match remaining with 
        | None   -> read_new state read_fd 
        | Some r -> read_remaining state r read_fd
end 

module Write = struct 

    type state = {
        mutable remaining_write : (int * int) option;
        buf : bytes; 
    }
    
    let create_state () = {
        remaining_write = None; 
        buf = Bytes.create 4; 
    }
    
    type status = 
        | Complete 
        | Partial 
    
    let write_new msg state fd : status = 
        let l = String.length msg in 
        Int_encoding.int_to_bytes_unsafe l state.buf 0;
        let written = Unix.single_write fd state.buf 0 Int_encoding.size in
        assert (written = Int_encoding.size);
        match Unix.single_write_substring fd msg  0 l with 
        | x when x = l -> Complete
        | n            -> ( 
            state.remaining_write <- Some (n, l - n); 
            (*state.remaining_write <- Some (msg, tl, n, l - n) 
        *)
            Partial
        )
    
    let write_remaining msg state (encoded_n, len) fd : status = 
        match Unix.single_write_substring fd msg encoded_n len with 
        | x when x = len -> (
            state.remaining_write <- None ; 
            Complete 
        ) 
        | n -> (
            state.remaining_write <- Some (encoded_n + n, len - n);
            Partial
        )
    
    let write msg ({remaining_write; _ } as state) fd = 
        match remaining_write with 
        | Some r -> write_remaining msg state r fd 
        | None   -> write_new msg state fd 


end


