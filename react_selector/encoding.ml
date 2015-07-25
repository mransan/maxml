
module type Size_encoder_sig = sig
  val size : int 
  val encode : int -> bytes -> int -> unit 
  val decode : bytes -> int -> int  
end 

module Size_32_encoder = struct 

  let size = 4
  
  let encode (x:int) (buffer:bytes) (start:int) = 

    let f mask shift_size = 
       Char.unsafe_chr @@  ((x land mask) lsr shift_size) 
    in 
    Bytes.set buffer (start + 0) @@ f 0xFF000000 24;  
    Bytes.set buffer (start + 1) @@ f 0x00FF0000 16;  
    Bytes.set buffer (start + 2) @@ f 0x0000FF00 8;  
    Bytes.set buffer (start + 3) @@ f 0x000000FF 0;
    ()

  let decode buffer start = 
    let f i shift_size = 
      (Bytes.get buffer i |> Char.code )lsl shift_size
    in 
    f 0 24 
    lor f 1 16 
    lor f 2 8 
    lor f 3 0

end 

module type S = sig 
  type read_status = 
    | Read_closed       
    | Read_partial  of float 
    | Read_complete of string
  type read_state
  val create_read_state : unit -> read_state 
  val read : read_state -> Unix.file_descr -> read_status 

  type write_status = 
    | Write_complete 
    | Write_partial 

  type write_state
  val create_write_state : unit -> write_state
  val write : string -> write_state -> Unix.file_descr -> write_status 
end 

module Make (S:Size_encoder_sig) = struct 
  
  type read_state = {
    mutable len : int;
    mutable buf : bytes; 
    mutable remaining : (int * int) option; 
  }
  
  let create_read_state () = {
    len = S.size;
    buf = Bytes.create S.size; 
    remaining = None; 
  }
  
  let state_check_size ({len; buf; remaining=_} as state) s = 
    let overflow = len - s in 
    if overflow < 0 
    then ( 
      state.buf <- Bytes.extend buf 0 (- overflow);
      state.len <- Bytes.length buf 
    )
  
  type read_status = 
    | Read_closed
    | Read_partial  of float  
    | Read_complete of string  
  
  let read_new state fd = 
    match Unix.read fd state.buf 0 Size_32_encoder.size with
    | 0 -> Read_closed
    | i when i = S.size -> (
      let string_size = S.decode state.buf 0 in 
      state_check_size state @@ string_size + S.size; 
      match Unix.read fd state.buf S.size string_size with 
      | i when i = string_size -> ( 
        let s = Bytes.sub_string state.buf S.size string_size in 
        Read_complete s
      )
      | 0 -> Read_closed
      | x -> (
        state.remaining <- Some (string_size - x, x); 
        Read_partial (100. *. float_of_int x /. (float_of_int string_size)) 
      ) 
    )
    | x -> failwith @@ Printf.sprintf "Failed to read msg size (%i)" x
  
  let read_remaining state (remaining_n, n) fd = 
    match Unix.read fd state.buf n remaining_n with
    | 0 -> Read_closed
    | i when i = remaining_n -> (
      state.remaining <- None; 
      let s = Bytes.sub_string state.buf S.size (n + remaining_n) in  
      Read_complete s 
    )
    | n' -> (
      let read = n+n' in 
      let remaining_n = remaining_n - n' in 
      let total = remaining_n + read in 
      state.remaining <- Some (remaining_n, read); 
      Read_partial (100. *. float_of_int read /. (float_of_int total))  
    )
  
  let read ({remaining; _ } as state) read_fd = 
    match remaining with 
    | None   -> read_new state read_fd 
    | Some r -> read_remaining state r read_fd
  
  type write_state = {
    mutable remaining_write : (int * int) option;
    buf : bytes; 
  }
  
  let create_write_state () = {
    remaining_write = None; 
    buf = Bytes.create S.size; 
  }
  
  type write_status = 
    | Write_complete 
    | Write_partial 
  
  let write_new msg state fd : write_status = 
    let l = String.length msg in 
    S.encode l state.buf 0;
    let written = Unix.single_write fd state.buf 0 S.size in
    assert (written = S.size);
    match Unix.single_write_substring fd msg  0 l with 
    | x when x = l -> Write_complete
    | n      -> ( 
      state.remaining_write <- Some (n, l - n); 
      (*state.remaining_write <- Some (msg, tl, n, l - n) 
      *)
      Write_partial
    )
  
  let write_remaining msg state (encoded_n, len) fd : write_status = 
    match Unix.single_write_substring fd msg encoded_n len with 
    | x when x = len -> (
      state.remaining_write <- None ; 
      Write_complete 
    ) 
    | n -> (
      state.remaining_write <- Some (encoded_n + n, len - n);
      Write_partial
    )
  
  let write msg ({remaining_write; _ } as state) fd = 
    match remaining_write with 
    | Some r -> write_remaining msg state r fd 
    | None   -> write_new msg state fd 
end
