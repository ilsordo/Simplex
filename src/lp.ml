open Printf
open Field

type 'a bound = Inf of 'a | Sup of 'a | Both of 'a * 'a | Unconstrained
type 'a varmap = (string * 'a) list

(* Linear program : Maximize objective function subject to constraints and bounds *)
type 'a t = { objective : 'a varmap * 'a (* Objective function with constant term *)
            ; constraints : ('a varmap * 'a) list (* The sum over the list + constant term must be positive *)
            ; bounds : (string, 'a bound) Hashtbl.t
            }

module Process (F:FIELD) = struct

  let eval values (vars, const) =
    List.fold_left
      (fun x (v, c) -> F.(x + c * (Hashtbl.find values v)))
      const
      vars

  let check values {objective; constraints; bounds} =
    let check_bound v b =
      let x = Hashtbl.find values v in
      match b with
      | Unconstrained -> ()
      | Inf a -> if not (F.compare x a >= 0) then assert false
      | Sup a -> assert (F.compare x a <= 0)
      | Both (a, b) -> assert (F.compare x a >= 0 && F.compare x b <= 0) in
    List.iteri
      (fun i c ->
         if F.(compare (eval values c) zero) < 0 then
           Printf.printf "Error : %d %a\n" i F.print (eval values c)
      )
      constraints;
    Hashtbl.iter check_bound bounds;
    eval values objective

  let dualize {objective; constraints; bounds} =
    ()

  let print chan {objective; constraints; bounds} =
    let print_bound chan = function
      | Inf x -> fprintf chan "[%a, +\\infty]" F.print x
      | Sup x -> fprintf chan "[-\\infty, %a]" F.print x
      | Both (x,y) -> fprintf chan "[%a, %a]" F.print x F.print y
      | Unconstrained -> fprintf chan "[-\\infty, +\\infty]" in
    let print_bounds chan bounds =
      fprintf chan "Bounds:\n";
      Hashtbl.fold (fun var bound acc -> (var, bound)::acc) bounds []
      |> List.sort (fun (v1, _) (v2, _) -> String.compare v1 v2)
      |> List.iter (fun (var, bound) -> fprintf chan "&$%s : %a$\\\\\n" var print_bound bound)
    in
    let rec print_lc_aux chan = function
      | [] -> ()
      | [(var, coeff)] ->
        fprintf chan "%a %s" F.print coeff var
      | (var1, coeff1)::(var2, coeff2)::t when F.(compare coeff2 zero) < 0 ->
        fprintf chan "%a %s - " F.print coeff1 var1;
        print_lc_aux chan ((var2, F.neg coeff2)::t)
      | (var, coeff)::t ->
        fprintf chan "%a %s + " F.print coeff var;
        print_lc_aux chan t in
    let print_lc chan (vars, const) =
      match vars with
      | [] -> F.print chan const
      | _ ->
        print_lc_aux chan vars;
        if F.(compare const zero) < 0 then
          fprintf chan " - %a" F.print (F.neg const)
        else
          fprintf chan " + %a" F.print const in
    let rec print_constraints chan = function
      | [] -> ()
      | lc::t ->
        fprintf chan "&$%a >= 0$\\\\" print_lc lc;
        print_constraints chan t in
    fprintf chan "\\begin{tabular}{rl}Maximize:&%a\\\\ Subject to:&\\\\ %a %a \\end{tabular}"
      print_lc objective
      print_constraints constraints
      print_bounds bounds
end
