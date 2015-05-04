open Num
type t = num

let rec exprap x acc = function
  | 0 -> acc
  | n when n mod 2 = 1 ->
    exprap x (acc*/acc*/x) (n/2)
  | n ->
    exprap x (acc*/acc) (n/2)

let of_string s =
  try Some (num_of_string s)
  with Failure _ ->
    try Scanf.sscanf s "%d.%d"
          (fun x1 x2 ->
             Some ((num_of_int x1) +/ (num_of_int x2) // (exprap (num_of_int 10) (Int 1) (String.length (string_of_int x1)))))
    with _ -> None
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
