



type b1 = | B1 of int
type b2 = string


let get_b1 () = B1 2
let get_b2 () = "bbbbbb"

let get_s = get_b2

let get_a1 () = A.get_a1 ()
