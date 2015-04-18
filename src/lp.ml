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

  let print chan {objective; constraints; bounds} =
    let print_bound chan = function
      | Inf x -> fprintf chan "[%a, +inf]" F.print x
      | Sup x -> fprintf chan "[-inf, %a]" F.print x
      | Both (x,y) -> fprintf chan "[%a, %a]" F.print x F.print y
      | Unconstrained -> fprintf chan "[-inf, +inf]" in
    let print_bounds chan bounds =
      fprintf chan "Bounds:\n";
      Hashtbl.iter
        (fun var bound -> fprintf chan "%s : %a\n" var print_bound bound)
        bounds in
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
        fprintf chan "%a >= 0\n" print_lc lc;
        print_constraints chan t in
    fprintf chan "Maximize\n%a\n\nSubject to:\n%a\n%a\n%!"
      print_lc objective
      print_constraints constraints
      print_bounds bounds
end
