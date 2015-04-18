open Num
type t = num

let of_string s = try Some (num_of_string s) with Failure _ -> None
let of_int = num_of_int
let to_string = string_of_num
let print chan x = Printf.fprintf chan "%s" (string_of_num x)

let zero = Int 0
let one = Int 1

let ( + )  = add_num
let ( - )  = sub_num
let ( * )  = mult_num
let ( / )  = div_num

let neg = minus_num
let inv x = one // x

let compare = compare_num
