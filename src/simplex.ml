open Field
open Dictionary

exception First_phase

(*
module Simplex(F:FIELD) = struct
end
*)

type t = Empty of Dictionary.t | Unbounded of Dictionary.t*int | Opt of Dictionary.t (*| Unfinished of Dictionary.t *)

let choose_entering dict = (* Some v if dict.vars.(v) is the entering variable, None if no solution *)
  let (_,max_var,max_val) = Array.fold 
                              (fun (pos,pos_temp,val_temp) x -> 
                                 if F.(compare x val_temp) >= 0 then 
                                   (pos+1,pos+1,x) 
                                 else 
                                   (pos+1,pos_temp,val_temp))
                              dict.coeffs (-1,-1,neg F.one) in
  if F.(compare max_val F.zero) > 0 then
    Some max_var
  else
    None

let choose_leaving entering dict = (* Some v if dict.rows.(v).head is the leaving variable, None if unbounded *)
  let (_,max_var,_,denum) = Array.fold
             (fun (pos,pos_temp,num,denum) r -> 
                let (num_r,denum_r) = (r.const,r.body.(entering)) in
                if F.(compare num_r*denum_r F.zero) < 0 and F.(compare num_r*denum denum_r*num) >= 0 then (** and ?*) (** marche aussi pour 1st phase ?*)
                  (pos+1,pos+1,num_r,denum_r)
                else
                  (pos+1,pos_temp,num,denum))
             dict.rows (-1,-1,F.zero,F.zero) in
  if F.(compare denum F.zero) > 0 then
    Some max_var
  else
    None

let pivot entering leaving dict = (* Pivot colum "entering" and row "leaving" *)
  let ent_var = dict.vars.(ent) in
  let piv_row = dict.rows.(lea) in
  let coef_piv = piv_row.row.(ent) in
    dict.vars.(ent) = piv_row.head;
    piv_row.head = ent_var;
  piv_row.row.(ent) = F.one;  
  Array.map (fun x -> x / (neg coef_piv)) piv_row.row;
  
  let pivot_row r = 


let rec pivots dict =
  match choose_entering dict with
    | Some ent -> 
        begin
          match choose_leaving ent dict with
            | Some lea -> pivots (pivot ent lea dict)
            | None -> Unbounded (dict,ent)
        end
    | None -> Opt dict

let auxiliary_dict aux_var dict = (* First phase: add an auxiliary variable, called aux_var, to the dictionnary *)

let project aux_var dict = (* First phase: project the dictionary according to aux_var *)

let first_phase dict =
  let execute = (* True iff first phase needed *) 
    try
      Array.fold (fun _ r -> if F.(compare r.const F.zero) < 0 then raise First_phase else False) dict.rows False (** fold ? *)
    with
      First_phase -> True in
  if execute then
    let aux_var = Array.length dict.rows + Array.length dict.vars + 1 in (* name of the auxiliary variable to add *)
    let dict = auxiliary_dict aux_var dict in (* add the auxiliary variable *)
    match pivots dict with (* apply first phase's pivots *)
      | Opt dict | Unbounded (dict,ent) ->
          if F.compare(dict.(**?*) F.zero) <> 0 then
            Empty (**proj*)
          else
            pivots (**proj*)
      | _ -> assert false (** false ou False ?*)
  else
    pivots dict
