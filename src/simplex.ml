open Field
open Dictionary
open Profile

type 'a t = Empty of 'a Dictionary.t | Unbounded of ('a Dictionary.t)*int | Opt of 'a Dictionary.t

module Make(F:FIELD) = struct

  (************* Global functions ****************)

  let add_rows r1 r2 c = (* r1 <- r1 + c*r2 *)
    assert Array.(length r1.body = length r2.body);
    Array.iteri
      (fun n x -> r1.body.(n) <- r1.body.(n) + c*x)
      r2.body;
    r1.const <- r1.const + c*r2.const

  exception Found of int

  let array_find f arr = (* Some n if arr.[n] is the first elt of arr that verifies f x, None otherwise *)
    try
      Array.iteri
        (fun n x -> if f x then raise (Found n))
        arr;
      None
    with Found n -> Some n

  let array_doublemap arr1 arr2 f = (* arr1.(n) <- f arr1.(n) arr2.(n) *)
    assert (Array.length arr1 == Array.length arr2);
    Array.iteri
      (fun n x -> arr1.(n) <- f arr1.(n) x)
      arr2

  let iter_except except f =
    Array.iteri (fun n x -> if n <> except then f x)

  let partial_copy arr n = (* return a copy of array arr without entry n *)
    assert (n >= 0 && n < Array.length arr);
    let new_arr = Array.init
        (Array.length arr - 1)
        (fun i ->
           if i<n then
             arr.(i)
           else
             arr.(i+1)) in
    new_arr

  (************* Simplex without first phase ****************)

  let choose_entering dict = (* Some v if dict.nonbasics.(v) is the entering variable, None if no entering variable *)
    let (_,pos,w) = 
      Array.fold_left
        (fun (pos,n_max,w_max) x ->
          if F.(compare x w_max) > 0 then
            (pos+1,pos,x)
          else
            (pos+1,n_max,w_max))
        (0,0,F.zero) 
    dict.coeffs.body in
    if F.(compare w zero) > 0 then
      Some pos
    else
      None

    (*array_find (fun x -> F.(compare x F.zero) > 0) dict.coeffs.body*)

  let choose_leaving ent ?(first_phase = false) dict = (* Some v if dict.nonbasics.(v) is the leaving variable, None if unbounded *)
    let fp = if first_phase then F.(neg one) else F.one in
    let (_, max_var, _, denum) =
    Array.fold_left
      (fun (pos, pos_temp, num, denum) r ->
         let (num_r, denum_r) = (r.const, r.body.(ent)) in
         if F.(compare (fp * denum_r) F.zero) < 0 && F.(compare (fp * num_r * denum) (fp * denum_r * num)) >= 0 then (** marche aussi pour 1st phase ?*)
           (pos+1, pos, num_r, denum_r)
         else
           (pos+1, pos_temp, num, denum))
      (0 , 0, F.zero, F.zero)
      dict.rows in
    if F.(compare (fp * denum) F.zero) < 0 then
      Some max_var
    else
      None

  let update_row ent lea_r r = (* row lea_r has been updated according to ent. now, update row r *)
    let coeff = r.body.(ent) in
    r.body.(ent) <- F.zero;
    array_doublemap r.body lea_r.body (fun c1 c2 -> F.(c1 + (coeff * c2)));
    r.const <- F.(r.const + (coeff * lea_r.const))

  let update_dict ent lea dict = (* update all the dictionary, excepting row lea and nonbasics *)
    let lea_r = dict.rows.(lea) in
    iter_except lea (fun r -> update_row ent lea_r r) dict.rows;
    update_row ent lea_r dict.coeffs

  let pivot ent lea dict = (* Pivot colum ent and row lea *)
    Profile.register "Pivots";
    let ent_var = dict.nonbasics.(ent) in (* name of the entering variable *)
    let piv_row = dict.rows.(lea) in (* row to be pivot *)
    let coeff = piv_row.body.(ent) in (* coeff of the entering variable into piv_row *)
    dict.nonbasics.(ent) <- dict.basics.(lea);
    dict.basics.(lea) <- ent_var;
    piv_row.body.(ent) <- F.(neg one);
    piv_row.const <- F.(piv_row.const / (neg coeff));
    Array.iteri (fun i x -> piv_row.body.(i) <- F.(x / (neg coeff))) piv_row.body;
    update_dict ent lea dict (* update the other rows + the objective *)

  let rec pivots dict = (* Pivots the dictionnary until being blocked *)
    match choose_entering dict with
    | None ->
      time "Second phase";
      Opt dict
    | Some ent ->
      match choose_leaving ent dict with
      | None ->
        time "Second phase";
        Unbounded (dict, ent)
      | Some lea ->
        pivot ent lea dict;
        let module F_dic = Dictionary.Make(F) in
        Profile.dprintf "After pivoting %d to %d:\n%a\n" ent lea F_dic.print dict;
        pivots dict

(************* Simplex with First phase ****************)

  let auxiliary_dict aux_var (dict : F.t Dictionary.t) = (* Start of first phase: add an auxiliary variable to the dictionnary *)
    let aux_rows = Array.map (fun row -> {row with body = Array.append row.body [|F.one|]}) dict.rows in
    let aux_dic =
      { nonbasics = Array.append dict.nonbasics [|aux_var|]
      ; basics = Array.copy dict.basics (* Safer *)
      ; coeffs = {body = Array.(append (make (length dict.coeffs.body) F.zero) [|F.(neg one)|]); const = F.zero}
      ; rows = aux_rows
      } in
    aux_dic

  type place = Basic of int | Non_basic of int

  module Vars_map = Map.Make(struct type t = var_id let compare = compare end) (* place of each variable in the initial dictionary. If v -> Basic n, then basics.(n) = v. If v -> Non_basic n then coeffs.(n) = v *)

  let save_place basics nonbasics =
    let (_,save_basic) =
      Array.fold_left
        (fun (pos,m) v_basic ->
           (pos+1,Vars_map.add v_basic (Basic pos) m))
        (0,Vars_map.empty)
        basics in
    let (_,res) =
      Array.fold_left
        (fun (pos',m') v_nonbasic ->
           (pos'+1,Vars_map.add v_nonbasic (Non_basic pos') m'))
        (0,save_basic)
        nonbasics in
    res

  let rec project_var v coeff places dict =
    if coeff <> F.zero then
      match Vars_map.find v places with
      | Non_basic pos -> dict.coeffs.body.(pos) <- F.(dict.coeffs.body.(pos) + coeff)
      | Basic pos ->
        dict.coeffs.const <- F.(dict.coeffs.const+coeff*dict.rows.(pos).const);
        let _ = Array.fold_left
            (fun n var -> project_var var F.(dict.rows.(pos).body.(n) * coeff) places dict ; n+1) 0 dict.nonbasics in ()

    let project_basic pivot_pos coeffs_init basics_init nonbasics_init aux_var dict = (* project the dictionary when the auxiliary variable is non basic *) 
let module F_dic = Dictionary.Make(F) in Profile.dprintf "Before projection \n%a" F_dic.print dict; (*** <------------------- *)
    let pivot_pos = (* position of aux_var in dict.nonbasics *)
      match array_find (fun x -> x == aux_var) dict.nonbasics with
      | Some n -> n
      | None -> assert false in (** forcèment 0 - x0 ? *)
    let new_coeffs = { const = coeffs_init.const; body = Array.make (Array.length dict.coeffs.body - 1) F.zero } in
    let new_rows = Array.make (Array.length dict.rows) dict.rows.(0) in
    array_doublemap new_rows dict.rows (fun _ r -> { body = partial_copy r.body pivot_pos ; const = r.const});
    let proj_dict =
      { nonbasics = partial_copy dict.nonbasics pivot_pos
      ; basics = dict.basics
      ; coeffs = new_coeffs
      ; rows = new_rows
      } in
    let places = save_place proj_dict.basics proj_dict.nonbasics in
    let _ = Array.fold_left
        (fun n v -> project_var v coeffs_init.body.(n) places proj_dict ; n+1) 0 nonbasics_init in
      proj_dict

  let project coeffs_init basics_init nonbasics_init aux_var dict = 
    let module F_dic = Dictionary.Make(F) in Printf.printf "Before projection \n%a" F_dic.print dict; (*** <------------------- *)
    match array_find (fun x -> x == aux_var) dict.nonbasics with
      | Some pivot_pos -> project_basic pivot_pos coeffs_init basics_init nonbasics_init aux_var dict
      | None -> dict (** strange: how to remove the auxiliary var ? *)

  let first_phase dict = (* Simplex when first phase needed *)
    let coeffs_init = {body = Array.copy dict.coeffs.body ; const = dict.coeffs.const } in (* save the coeffs for later (projection of first phase) *)
    let basics_init = Array.copy dict.basics in (* save the basics for later (projection of first phase) *)
    let nonbasics_init = Array.copy dict.nonbasics in (* save the nonbasics for later (projection of first phase) *)
    let aux_var = Array.length dict.rows + Array.length dict.nonbasics + 1 in (* name of the auxiliary variable to add *)
    let dict = auxiliary_dict aux_var dict in (* add the auxiliary variable into the dictionary *)
    let module F_dic = Dictionary.Make(F) in Profile.dprintf "Auxiliary dic \n%a" F_dic.print dict;
    match choose_leaving (Array.length dict.nonbasics - 1) ~first_phase:true dict with (** ok ?*)
    | None -> assert false
    | Some lea ->
      begin
        pivot (Array.length dict.nonbasics - 1) lea dict; (* illegal pivot *)
        let module F_dic = Dictionary.Make(F) in Profile.dprintf "Illegal pivot: \n%a" F_dic.print dict;
        match pivots dict with
        | Opt dict | Unbounded (dict,_) ->
          let dict_proj = project coeffs_init basics_init nonbasics_init aux_var dict in (* projection of the dictionary, remove the auxiliary variable *)
          time "First phase";
          if F.(compare dict.coeffs.const F.zero) <> 0 then
            Empty dict_proj
          else
            pivots dict_proj
        | _ -> assert false
      end

  (************* Final function ****************)

  let simplex dict = (* Apply the whole simplex *)
    Profile.time "_";
    match array_find (fun r -> F.(compare r.const F.zero) < 0) dict.rows with
      | Some i ->
        Profile.dprintf "Starting first phase because of line %d\n\n" i;
        first_phase dict
      | None -> pivots dict

end
