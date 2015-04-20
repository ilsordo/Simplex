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

  let make_row n_vars _ =
    { body = Array.make n_vars F.zero
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
    let (n_vars, consts) = Hashtbl.fold process_bounds bounds (0, constraints) in
    let n_consts = List.length constraints in
    let dic = { vars = Array.init n_vars (fun x -> x)
              ; heads = Array.init n_consts (fun n -> n + n_vars)
              ; coeffs = make_row n_vars ()
              ; rows = Array.init (List.length consts) (make_row n_vars)
              } in
    (conversion, dic)

  let print_conv chan conv =
    let open Printf in
    let print_var v = function
      | Unbounded (n1, n2) -> fprintf chan "%s => x_%d- x_%d\n" v n1 n2
      | Shift (n, x) -> fprintf chan "%s => x_%d- %a\n" v n F.print x
      | Swap_and_shift (n, x) -> fprintf chan "%s => -x_%d+ %a\n" v n F.print x
      | Constant x -> fprintf chan "%s => %a\n" v F.print x in
    Hashtbl.iter print_var conv

  let print chan ({vars; heads; coeffs; rows} as dic) =
    let open Printf in
    let numvars = numvars dic in
    let varname i =
      if i < numvars then
        sprintf "x_%d" i
      else
        sprintf "y_%d" (i-numvars) in
    let pad_to n s = String.make (n-(String.length s)) ' ' ^ s in
    let max_length n w = max w (String.length n) in
    let mx = " Maximize" in
    let st = "Such that" in
    let cst = "Constant" in
    let hd = max (String.length mx) (String.length st) in
    let rownames = Array.map varname heads in
    let lwidth = 2 + Array.fold_right max_length rownames hd in
    let const_name = F.to_string coeffs.const in
    let consts_names = Array.map (fun {const; _} -> F.to_string const) rows in
    let rwidth = Array.fold_right max_length consts_names (max (String.length cst) (String.length const_name)) in
    let varnames = Array.map varname vars in
    let coeff_names = Array.map F.to_string coeffs.body in
    let body_names = Array.map (fun {body;_} -> Array.map (fun x -> sprintf "%a" (fun () -> F.to_string) x) body) rows in
    let colwidth_aux = Array.mapi (fun i n -> max (String.length n) (String.length coeff_names.(i))) varnames in
    let colwidth = Array.mapi (fun i w -> Array.fold_right (fun a w -> max_length a.(i) w) body_names w) colwidth_aux in
    let print_lc h body const =
      fprintf chan "%s" (pad_to lwidth h);
      Array.iteri (fun i w -> fprintf chan "%s |" (pad_to w body.(i))) colwidth;
      fprintf chan "%s\n" (pad_to rwidth const) in
    print_lc (mx^"  ") coeff_names const_name;
    print_lc (st^"  ") varnames cst;
    Array.iteri (fun i n -> print_lc (n^" |") body_names.(i) consts_names.(i)) rownames


end
