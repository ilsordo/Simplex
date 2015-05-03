type t = Q.t

let rec exprap x acc = function
  | 0 -> acc
  | n when n mod 2 = 1 ->
    exprap x Q.(acc*acc*x) (n/2)
  | n ->
    exprap x Q.(acc*acc) (n/2)

let of_string s =
  try Some (Q.of_string s)
  with Invalid_argument _ ->
    try Scanf.sscanf s "%d.%d"
          (fun x1 x2 ->
             Some Q.((of_int x1) + (of_int x2) / (exprap (of_int 10) one (String.length (string_of_int x1)))))
    with _ -> None

let of_int = Q.of_int
let to_string = Q.to_string
let print = Q.output

let zero = Q.zero
let one = Q.one

let ( + )  = Q.add
let ( - )  = Q.sub
let ( * )  = Q.mul
let ( / )  = Q.div

let neg = Q.neg
let inv = Q.inv

let compare = Q.compare
