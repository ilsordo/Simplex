type var = int

type 'a row =
  { mutable head : var
  ; body : 'a array
  ; mutable const : 'a
  }

type 'a t =
  { vars : var array (* vars.(i) is the id of the variable in column i*)
  ; rows : 'a row array
  ; coeffs : 'a array
  ; mutable value : int
  }
