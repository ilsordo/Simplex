type t = float

let of_string s =
  try Some (float_of_string s)
  with Failure _ ->
    try Scanf.sscanf s "%d/%d"
          (fun x1 x2 -> Some ((float_of_int x1) /. (float_of_int x2)))
    with _ -> None
let of_int = float_of_int
let to_string = string_of_float
let print chan x = Printf.fprintf chan "%f" x

let zero = 0.
let one = 1.

let ( + )  = ( +. )
let ( - )  = ( -. )
let ( * )  = ( *. )
let ( / )  = ( /. )

let neg x = -. x
let inv x = 1. /. x

let compare = compare
