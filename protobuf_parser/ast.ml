

type constant = 
  | Constant_string of string 
  | Constant_bool   of bool 
  | Constant_int    of int 
  | Constant_float  of float 

type field_option  = string * constant 
type field_options = field_option list 
