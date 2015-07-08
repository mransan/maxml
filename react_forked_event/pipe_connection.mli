
type t 
(** double pipe connection allowing 2 sided read/writes *)

val create : unit -> t 
(** [create ()] @returns a new pipe connection *)

val set : [`P1 | `P2 ]  -> t -> unit 
(** [set process t] closes the unused file descriptors from each side 
    of the pipe. This function must be called after the fork in both 
    parent and child process but with different [process] values.
 *)

val read_fd : [`P1 | `P2 ] -> t -> Unix.file_descr 
(** [read_fd process t] returns the read file descriptor *)

val write_fd : [`P1 | `P2] -> t -> Unix.file_descr 
(** [write_fd process t] returns the write file descriptor *)
