(* A dictionary is rep*)

type 'a row =
  { mutable head : int
  ; body : 'a array
  ; mutable const : 'a
  }

type 'a t =
  { vars : int array (* vars.(i) is the id of the variable in column i*)
  ; rows : 'a row array
  ; coeffs : 'a array
  ; mutable value : int
  }

let make
