


type t1
let t1: t1 Ctypes.structure Ctypes.typ = Ctypes.structure "t1"
let m_i = Ctypes.field t1 "m_i" Ctypes.int 
let m_d = Ctypes.field t1 "m_d" Ctypes.double
let () = Ctypes.seal t1

let get_t1_value  = 
  let open Ctypes in 
  Foreign.foreign "get_t1_value" (int @-> returning t1) 

let string_of_t1' = 
  let open Ctypes in 
  Foreign.foreign "string_of_t1" (ptr t1 @-> returning string) 

let string_of_t1 v = 
  string_of_t1' (Ctypes.allocate t1 v) 


type t2 
let t2  : t2 Ctypes.structure Ctypes.typ = Ctypes.structure "t2" 

let t2_create = 
  let open Ctypes in 
  Foreign.foreign "t1_create" (int @-> returning (ptr t2))

let t2_destroy  = 
  let open Ctypes in 
  Foreign.foreign "t1_destroy" (ptr t2 @-> returning void)

let string_of_t2 = 
  let open Ctypes in 
  Foreign.foreign "string_of_t1" (ptr t2 @-> returning string) 


type t3 
external t3_create : int -> t3  = "ml_t3_create" 
external t3_destroy : t3 -> unit = "ml_t3_destroy" 
external string_of_t3 : t3 -> string = "ml_string_of_t3" 

type t4 
external t4_create : int -> t4  = "ml_t4_create" 
external string_of_t4 : t4 -> string = "ml_string_of_t4" 

