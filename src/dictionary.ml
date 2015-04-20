open Field
open Lp

type var = int

module Make(F:FIELD) = struct
  type row =
    { body : F.t array
    ; mutable const : F.t
    }

  type t =
    { vars : var array (* vars.(i) is the id of the variable in column i *)
    ; heads : var array (* heads.(i) is the variable of row i *)
    ; coeffs : row (* coefficients of the weight function *)
    ; rows : row array
    }

  let make_row n_vars n =
    { head = n_vars+n
    ; body = Array.make n_vars F.zero
    ; const = F.zero
    }

  type 'a var = Unbounded of int*int (* id+, id- *)
              | Shift of int*'a (* id, shift *)
              | Swap_and_shift of int*'a (* id, shift *)
              | Constant of 'a

  let numvars {vars; _} = Array.length vars

  exception Impossible of string*F.t*F.t

  let make {objective; constraints; bounds} =
    let conversion = Hashtbl.create (Hashtbl.length bounds) in
    let process_bounds var bound (n,consts) = match bound with
      | Unconstrained ->
        Hashtbl.add conversion var (Unbounded (n, n+1)); (n+2, consts)
      | Inf x ->
        Hashtbl.add conversion var (Shift (n, x)); (n+1, consts)
      | Sup x ->
        Hashtbl.add conversion var (Swap_and_shift (n, F.neg x)); (n+1, consts)
      | Both (x,y) ->
        match F.compare x y with
        | 0 -> Hashtbl.add conversion var (Constant x); (n, consts)
        | k when k>0 -> raise (Impossible (var, x, y))
        | _ ->
          let const = F.([var,neg one],y) in (* var <= y *)
          Hashtbl.add conversion var (Shift (n, x)); (n, const::consts)
    in
    let (n, consts) = Hashtbl.fold process_bounds bounds (0, constraints) in
    let dic = { vars = Array.init n (fun x -> x)
              ; coeffs = Array.make n F.zero
              ; rows = Array.init (List.length consts) (make_row n)
              } in
    (conversion, dic)

  let print chan ({vars; coeffs; rows} as dic) =
    let open Printf in
    let numvars = numvars dic in
    let varname i =
      if i > numvars then
        sprintf "x_%d" i
      else
        sprintf "y_%d" (i-numvars) in
    let mx = " Maximize" in
    let st = "Such that" in
    let toto = printf "%a" F.print F.zero in
    let hd = String.length mx in
    let varnames = Array.map varname vars in
    let rownames = Array.map (fun {head; _} -> varname head) rows in
    let lwidth = Array.fold_right (fun n w -> max w (String.length n)) rownames hd in
    let coeffs = Array.map (fun {body;_} -> Array.map (fun x -> sprintf "%a" F.print x) coeffs) rows in
    ()


end
