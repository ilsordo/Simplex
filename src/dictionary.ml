open Field
open Lp

type var_id = int

type 'a row =
    { body : 'a array
    ; mutable const : 'a
    }

type 'a t =
  { nonbasics : var_id array (* nonbasics.(i) is the id of the variable in column i *)
  ; basics : var_id array (* basics.(i) is the variable of row i *)
  ; coeffs : 'a row (* coefficients of the weight function *)
  ; rows : 'a row array
  }

type 'a var = Unbounded of int*int (* id+, id- *)
            | Shift of int*'a (* id, shift *)
            | Swap_and_shift of int*'a (* id, shift *)
            | Constant of 'a

type 'a result = Invalid_constraint of string*'a*'a
               | Conversion of (string,'a var) Hashtbl.t*'a t

module Make(F:FIELD) = struct

  exception Impossible of string*F.t*F.t

  let make {objective; constraints; bounds} =
    try
      let make_row n_vars _ =
        { body = Array.make n_vars F.zero
        ; const = F.zero
        } in
      let conversion = Hashtbl.create (Hashtbl.length bounds) in
      let process_bounds var bound (n,consts) = match bound with
        | Unconstrained ->
          Hashtbl.add conversion var (Unbounded (n, n+1)); (n+2, consts)
        | Inf x ->
          Hashtbl.add conversion var (Shift (n, x)); (n+1, consts)
        | Sup x ->
          Hashtbl.add conversion var (Swap_and_shift (n, x)); (n+1, consts)
        | Both (x,y) ->
          match F.compare x y with
          | 0 -> Hashtbl.add conversion var (Constant x); (n, consts)
          | k when k>0 -> raise (Impossible (var, x, y))
          | _ ->
            let const = F.([var,neg one],y) in (* var <= y *)
            Hashtbl.add conversion var (Shift (n, x)); (n+1, const::consts)
      in
      let (n_vars, consts) = Hashtbl.fold process_bounds bounds (0, constraints) in
      let n_consts = List.length constraints in
      let dic = { nonbasics = Array.init n_vars (fun x -> x)
                ; basics = Array.init n_consts (fun n -> n + n_vars)
                ; coeffs = make_row n_vars ()
                ; rows = Array.init (List.length consts) (make_row n_vars)
                } in
      let apply_constr (lc, c) row =
        row.const <- c;
        List.iter
          (fun (var, coeff) ->
             match Hashtbl.find conversion var with
             | Unbounded (n1, n2) ->
               row.body.(n1) <- coeff;
               row.body.(n2) <- F.neg coeff
             | Shift (n, x) ->
               row.body.(n) <- coeff;
               row.const <- F.(row.const - coeff * x)
             | Swap_and_shift (n, x) ->
               row.body.(n) <- F.neg coeff;
               row.const <- F.(row.const + coeff * x) (* A vérifier *)
             | Constant x ->
               row.const <- F.(row.const + x)
             | exception Not_found -> raise (Failure ("Unknown variable : "^var)))
          lc
      in
      apply_constr objective dic.coeffs;
      List.iteri (fun i constr -> apply_constr constr dic.rows.(i)) constraints;
      Conversion (conversion, dic)
    with Impossible (v, x1, x2) -> Invalid_constraint (v, x1, x2)

  let dual {nonbasics; basics; coeffs; rows} =
    let new_coeffs = Array.map (fun {const; _} -> F.neg const) rows in
    let row_size = Array.length rows in
    let new_rows = Array.mapi
        (fun i const ->
           let body = Array.init row_size (fun j -> F.neg rows.(j).body.(i)) in
           {const = F.neg const; body})
        coeffs.body in
    { nonbasics = Array.copy basics
    ; basics = Array.copy nonbasics
    ; coeffs = {coeffs with body = new_coeffs}
    ; rows = new_rows
    }

  let solution conv dic =
    let numvars = Array.length dic.nonbasics in
    let vars = Array.make numvars F.zero in
    Array.iteri
      (fun i v ->
         if v < numvars then
           vars.(v) <- dic.rows.(i).const)
      dic.basics;
    let eval = function
      | Unbounded (n1, n2) ->
        F.(vars.(n1) - vars.(n2))
      | Shift (n, x) ->
        F.(vars.(n) + x)
      | Swap_and_shift (n, x) ->
        F.(x - vars.(n))
      | Constant x ->
        x in
    let vals = Hashtbl.create (Hashtbl.length conv) in
    Hashtbl.iter
      (fun n c -> Hashtbl.add vals n (eval c))
      conv;
    vals

  let print_sol_tex chan vals =
    let print_var (v, x) =
      Printf.fprintf chan "$%s = %a$\\\\" v F.print x in
    Hashtbl.fold (fun var x acc -> (var, x)::acc) vals []
    |> List.sort (fun (v1, _) (v2, _) -> String.compare v1 v2)
    |> List.iter print_var

  let print_sol chan vals =
    let print_var (v, x) =
      Printf.fprintf chan "%s = %a\n" v F.print x in
    Hashtbl.fold (fun var x acc -> (var, x)::acc) vals []
    |> List.sort (fun (v1, _) (v2, _) -> String.compare v1 v2)
    |> List.iter print_var

  let varname ?special numvars i =
      match special with
      | Some n ->
        if i = n then "X"
        else if i < numvars - 1 then
          Printf.sprintf "x_{%d}" i
        else
          Printf.sprintf "y_{%d}" (1+ i-numvars)
      | None -> if i < numvars then
          Printf.sprintf "x_{%d}" i
        else
          Printf.sprintf "y_{%d}" (i-numvars)

  let print_conv chan conv =
    let open Printf in
    let print_var = function
      | v, Unbounded (n1, n2) -> fprintf chan "$%s \\Rightarrow x_{%d} - x_{%d}$\\\\" v n1 n2
      | v, Shift (n, x) -> fprintf chan "$%s \\Rightarrow x_{%d} - %a$\\\\" v n F.print x
      | v, Swap_and_shift (n, x) -> fprintf chan "$%s \\Rightarrow -x_{%d} + %a$\\\\" v n F.print x
      | v, Constant x -> fprintf chan "$%s \\Rightarrow %a$\\\\" v F.print x in
    Hashtbl.fold (fun var conv acc -> (var, conv)::acc) conv []
    |> List.sort (fun (v1, _) (v2, _) -> String.compare v1 v2)
    |> List.iter print_var

  let print ?special () chan {nonbasics; basics; coeffs; rows} =
    let open Printf in
    let numvars = Array.length nonbasics in
    let print_lc chan {body; const} =
      Array.iter (fun c -> fprintf chan "& $%a$" F.print c) body;
      fprintf chan "& $%a$\\\\" F.print const in
    fprintf chan "\\begin{tabular}{|r|%s|}\\hline Maximize %a\\hline Subject to &"
      (String.make (1 + Array.length nonbasics) 'c')
      print_lc coeffs;
    Array.iter (fun v -> fprintf chan "$%s$ &" (varname ?special numvars v)) nonbasics;
    fprintf chan "Const \\\\";
    for i = 0 to Array.length basics - 1 do
      fprintf chan "\\hline $%s$ %a"
        (varname ?special numvars basics.(i))
        print_lc rows.(i)
    done;
    fprintf chan "\\hline\\end{tabular}"
(*
  let print chan ({nonbasics; basics; coeffs; rows} as dic) =
    let open Printf in
    let numvars = Array.length dic.nonbasics in
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
    let rownames = Array.map varname basics in
    let lwidth = 2 + Array.fold_right max_length rownames hd in
    let const_name = F.to_string coeffs.const in
    let consts_names = Array.map (fun {const; _} -> F.to_string const) rows in
    let rwidth = Array.fold_right max_length consts_names (max (String.length cst) (String.length const_name)) in
    let varnames = Array.map varname nonbasics in
    let coeff_names = Array.map F.to_string coeffs.body in
    let body_names = Array.map (fun {body;_} -> Array.map (fun x -> sprintf "%a" (fun () -> F.to_string) x) body) rows in
    let colwidth_aux = Array.mapi (fun i n -> max (String.length n) (String.length coeff_names.(i))) varnames in
    let colwidth = Array.mapi (fun i w -> Array.fold_right (fun a w -> max_length a.(i) w) body_names w) colwidth_aux in
    let print_lc h body const =
      fprintf chan "%s" (pad_to lwidth h);
      Array.iteri (fun i w -> fprintf chan "%s |" (pad_to w body.(i))) colwidth;
      fprintf chan "%s\\\\" (pad_to rwidth const) in
    print_lc (mx^"  ") coeff_names const_name;
    print_lc (st^"  ") varnames cst;
    Array.iteri (fun i n -> print_lc (n^" |") body_names.(i) consts_names.(i)) rownames
*)
end
