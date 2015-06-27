
module type Int_encoding_sig = sig 

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
end 

module Int : Int_encoding_sig = struct 

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


module Make (I:Int_encoding_sig) : sig

  val write_msg : ?buf:bytes -> Unix.file_descr  -> string -> unit 
  (** [write_msg ~buf fd s] will write the string [s] on the 
      [fd]. The optional argument [buf] can be used by the client 
      to improve speed performance to avoid re-creating 
      temporary buffer. The buffer should be at least I.size in available
      length.
    *) 

  val read_msg : ?buf:bytes -> Unix.file_descr -> string 
  (** [read_msg ~buf fd] will read a string encoding using the [write_msg] 
      function. 
      
    *) 

end = struct 

  let write_msg ?buf fd  s = 
    let buf = match buf with 
      | Some b -> b 
      | None   -> Bytes.create I.size in 
    let l = String.length s in 

    (* send the message size *)
    let (_:unit) = I.int_to_bytes_unsafe l buf 0 in 
    let (actuall_written:int) = Unix.single_write fd buf 0 I.size in
    assert (actuall_written = I.size);
    (* send the message *) 
    let (actuall_written:int)  = Unix.single_write_substring fd s 0 l in 
    assert (actuall_written = l);
    ()

  let read_msg ?buf fd = 
    
    let buf1 =  match buf with 
      | Some b -> b
      | None   -> Bytes.create I.size in 
    
    (* read the message size *)
    Fd_util.read fd buf1 0 I.size; 
    let string_size = I.int_of_bytes buf1 0 in 

    (* read the message *) 
    let buf2, offset2 = match buf with 
      | Some b -> b, I.size 
      | None -> Bytes.create string_size, 0  in 
    Fd_util.read fd buf2 offset2 string_size; 
    Bytes.sub_string buf2 offset2 string_size
end 
