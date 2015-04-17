type kind = Max | Min
type 'a bound = Inf of 'a | Sup of 'a | Both of 'a * 'a

type 'a varmap = (int, 'a) Hashtbl

type 'a t = { names : string list
            ; kind : kind
            ; z : 'a list * 'a (* Objective function with constant term *)
            ; constraints : ('a varlist * 'a) (* The sum over the list + constant term must be positive *)
            ; bounds : 'a bound varlist
            }

type 'a normalized_t = { map : Varmap.t}

let validate names kind z constraints bounds =
