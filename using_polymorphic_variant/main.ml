


type label = {
  text : string ; 
  fg_color : int ; 
  bg_color : int ;
}

type number_entry = {
  fg_color : int; 
  value    : float; 
}

let set_label (label:label) = function 
  | `Fg_color fg_color -> {label with fg_color}
  | `Bg_color bg_color -> {label with bg_color}
  | `Text text  -> {label with text}

let set_number_entry (number_entry:number_entry) = function 
  | `Fg_color fg_color  -> {number_entry with fg_color}
  | `Number_value value -> {number_entry with value;} 



