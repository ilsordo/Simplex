type t = Q.t

let of_string s = try Some (Q.of_string s) with Invalid_argument _ -> None
let of_int = Q.of_int
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
