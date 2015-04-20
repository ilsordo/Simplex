open Field
open Dictionary

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

  let array_doublemap arr1 arr2 ?(except = -1) f = (* arr1.(n) <- f arr1.(n) arr2.(n), except for n *)
    assert (Array.length arr1 == Array.length arr2);
    Array.iteri
      (fun n x -> if n <> except then arr1.(n) <- f arr1.(n) x)
      arr2

  let array_iter ?(except = -1) f =
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


  (* let (a,b) = Array.fold_left
      (fun (m,pos) x ->
        if n == m then
          (m+1,pos)
        else
          begin
            new_arr.(pos) <- x;
            (m+1,pos+1)
          end)
      (0,0) arr in
     assert (a <> b);*)

  (************* Simplex without first phase ****************)

  let choose_entering dict = (* Some v if dict.vars.(v) is the entering variable, None if no entering variable *)
    array_find (fun x -> F.(compare x F.zero) > 0) dict.coeffs.body

  let choose_leaving ent dict = (* Some v if dict.rows.(v).head is the leaving variable, None if unbounded *)
    let (_, max_var, _, denum) =
    Array.fold_left
      (fun (pos, pos_temp, num, denum) r ->
         let (num_r, denum_r) = (r.const,r.body.(ent)) in
         if F.(compare (num_r * denum_r) F.zero) < 0 && F.(compare (num_r * denum) (denum_r * num)) >= 0 then (** marche aussi pour 1st phase ?*)
           (pos+1, pos, num_r, denum_r)
         else
           (pos+1, pos_temp, num, denum))
      (0 , 0, F.zero, F.zero)
      dict.rows in
    if F.(compare denum F.zero) > 0 then
      Some max_var
    else
      None

  let update_row ent lea_r r = (* row lea_r has been updated according to ent. now, update row r *)
    let coeff = r.body.(ent) in
    r.body.(ent) <- F.zero;
    array_doublemap r.body lea_r.body (fun c1 c2 -> F.(c1 + (coeff * c2)));
    r.const <- F.(r.const + (coeff * lea_r.const))

  let update_dict ent lea dict = (* update all the dictionary, excepting row lea and vars *)
    let lea_r = dict.rows.(lea) in
    array_iter ~except:lea (fun r -> update_row ent lea_r r) dict.rows;
    update_row ent lea_r dict.coeffs

  let pivot ent lea dict = (* Pivot colum ent and row lea *)
    let ent_var = dict.vars.(ent) in (* name of the entering variable *)
    let piv_row = dict.rows.(lea) in (* row to be pivot *)
    let coeff = piv_row.body.(ent) in (* coeff of the entering variable into piv_row *)
    dict.vars.(ent) <- dict.heads.(lea);
    dict.heads.(lea) <- ent_var;
    piv_row.body.(ent) <- F.(neg one);
    Array.iteri (fun i x -> piv_row.body.(i) <- F.(x / (neg coeff))) piv_row.body; (*** Vérifie ici***)
    update_dict ent lea dict (* update the other rows + the objective *)

let rec pivots dict = (* Pivots the dictionnary until being blocked *)
  match choose_entering dict with
    | Some ent ->
        begin
          match choose_leaving ent dict with
            | Some lea ->
                pivot ent lea dict;
                pivots dict
            | None -> Unbounded (dict,ent)
        end
    | None -> Opt dict

(************* Simplex with First phase ****************)

let auxiliary_dict aux_var dict = (* Start of first phase: add an auxiliary variable, called aux_var, to the dictionnary *)
  let aux_dic =
    { vars = Array.append dict.vars [|aux_var|]
    ; heads = dict.heads
    ; coeffs = { body = Array.append (Array.make (Array.length dict.coeffs.body) F.zero) [|F.(neg F.one)|] ; const = F.zero }
    ; rows = dict.rows
    } in
  array_doublemap aux_dic.rows.body aux_dic.rows.body (fun r _ -> Array.append r [|F.one|]);
  aux_dic

type place = Basic of int | Non_basic of int

module Vars_map = Map.Make(struct type t = var_id*int let compare = compare end) (* place of each variable in the initial dictionary. If v -> Basic n, then heads.(n) = v. If v -> Non_basic n then coeffs.(n) = v *)

let save_place heads_init vars_init =
  let save_basic =
    Array.fold_left
      (fun (pos,m) v_basic ->
        (pos+1,Vars_map.add v_basic (Basic pos) m))
      (0,Vars_map.empty)
      heads_init in
  Array.fold_left
    (fun (pos',m') v_nonbasic ->
      (pos'+1,Vars_map.add v_nonbasic (Non_basic pos') m'))
    (0,save_basic)
    vars_init

let rec project_var v coeff places coeffs_init vars_init dict =
  if coeff <> 0 then
    match find v places with
      | Non_basic pos -> dict.coeffs.body.(pos) <- coeff
      | Basic pos ->
          dict.coeffs.const <- coeffs_init.const;
          let _ = Array.fold_left
            (fun n var -> project_var var (dict.rows.(pos).(n)*coeff) places coeffs_init vars_init dict ; n+1) 0 vars_init in ()

let project coeffs_init heads_init vars_init aux_var dict = (* project the dictionary when the auxiliary variable is non basic *)
  let places = save_place heads_init vars_init in
  let pivot_pos = (* position of aux_var in dict.vars *)
    match array_find dict.vars (fun x -> x == aux_var) with
      | Some n -> n
      | None -> assert false in (** forcèment 0 - x0 ? *)
  let new_coeffs = { const = coeffs_init.const; body = Array.make (Array.length dict.coeffs - 1) 0 } in
  let proj_dict =
    { vars = partial_copy dict.vars pivot_pos
    ; heads = dict.heads
    ; coeffs = new_coeffs
    ; rows = array_doublemap (Array.make (Array.length dict.rows) dict.rows.(0)) dict.rows (fun _ r -> partial_copy r pivot_pos)
    } in
    let _ = Array.fold_left
      (fun n v -> project_var v coeffs_init.(n) places coeffs_init vars_init proj_dict ; n+1) 0 vars_init in ()

let first_phase dict = (* Simplex when first phase needed *)
  let coeffs_init = Array.copy dict.coeffs in (* save the coeffs for later (projection of first phase) *)
  let heads_init = Array.copy dict.heads in (* save the heads for later (projection of first phase) *)
  let vars_init = Array.copy dict.vars in (* save the vars for later (projection of first phase) *)
  let aux_var = Array.length dict.rows + Array.length dict.vars + 1 in (* name of the auxiliary variable to add *)
  let dict = auxiliary_dict aux_var dict in (* add the auxiliary variable into the dictionary *)
  match choose_leaving (Array.length dict.vars - 1) dict with (** ok ?*)
    | None -> assert false
    | Some lea ->
        begin
          pivot ent lea dict; (* illegal pivot *)
          match pivots dict with
            | Opt dict | Unbounded (dict,ent) ->
                let dict_proj = project coeffs_init heads_init vars_init aux_var dict in (* projection of the dictionary, remove the auxiliary variable *)
                if F.compare(dict.coeffs.const F.zero) < 0 then
                  Empty dict_proj
                else
                  pivots dict_proj
            | _ -> assert false
        end

(************* Final function ****************)

let simplex dict = (* Apply the whole simplex *)
  match array_find dict.heads (fun x -> F.(compare x F.zero) < 0) with
    | Some _ -> first_phase dict
    | None -> pivots dict

end
