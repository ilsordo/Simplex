type t = float

let of_string s = try Some (float_of_string s) with Failure _ -> None
let of_int = float_of_int
let print chan x = Printf.fprintf chan "%f" x

let zero = 0.
let one = 1.

let ( + )  = ( +. )
let ( - )  = ( -. )
let ( * )  = ( *. )
let ( / )  = ( /. )


let compare = compare
