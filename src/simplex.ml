open Field
open Dictionary

type t = Empty of Dictionary.t | Unbounded of Dictionary.t*int | Opt of Dictionary.t

let add_rows r1 r2 c = (* r1 <- r1 + c*r2 *)
  let _ = 
    Array.fold
      (fun n x ->
        r1.(n) <- r1.(n) + c*x;
        n+1)
      0 r2 in
  r1.const <- r1.const + c*r2.const

exception Found of int

let array_find arr f = (* Some n if arr.[n] is the first elt of arr that verifies f x, None otherwise *)
  try
    let _ =
      Array.fold
        (fun n x ->
          if f x then
            raise Found n
          else
            n+1)
        0 arr in
    None
  with Found n -> Some n

let array_update arr1 arr2 f exc = (* arr2.(n) <- f n arr1.(n), except for n if exc = Some n *) (**mettre arg optionnel *)
  let pos = match exc with
    | Some n -> n
    | None -> -1 in
  let _ =
    Array.fold
      (fun n x ->
        if n <> pos then
          arr2.(n) <- f n x
      )
      0 arr1 in
  ()

let partial_copy arr n = (* return a copy of array arr without entry n *)
  let new_arr = Array.make (Array.length arr - 1) arr.(0) in
  let (a,b) =
    Array.fold
      (fun (m,pos) x ->
        if n == m then
          (m+1,pos)
        else
          begin
            new_arr.(pos) <- x;
            (m+1,pos+1)
          end)
      (0,0) arr in
  assert a <> b;
  new_arr

(*****************************)

let choose_entering dict = (* Some v if dict.vars.(v) is the entering variable, None if no entering variable *)
  array_find dict.coeffs (fun x -> F.(compare x F.zero) > 0)

let choose_leaving entering dict = (* Some v if dict.rows.(v).head is the leaving variable, None if unbounded *)
  let (_,max_var,_,denum) = 
    Array.fold
      (fun (pos,pos_temp,num,denum) r -> 
         let (num_r,denum_r) = (r.const,r.body.(entering)) in
         if F.(compare num_r*denum_r F.zero) < 0 && F.(compare num_r*denum denum_r*num) >= 0 then (** marche aussi pour 1st phase ?*)
           (pos+1,pos+1,num_r,denum_r)
         else
           (pos+1,pos_temp,num,denum))
      (-1,-1,F.zero,F.zero) dict.rows in
  if F.(compare denum F.zero) > 0 then
    Some max_var
  else
    None

let update_rows ent lea dict = (* row lea has been updated according to ent, update the other rows *)
  let update row = (* update one row *)
    begin
      row.const <- row.const + row.body.(ent)*dict.rows.(lea).const;
      let _ = Array.fold
        (fun n x ->
          if n == ent then
            row.body.(n) <- row.body.(ent)*dict.rows.(lea).body.(n);
          else
            row.body.(n) <- row.body.(n)+row.body.(ent)*dict.rows.(lea).body.(n);
          n+1)
        0 row.body in ()
    end in
  let _ = Array.fold
    (fun n r ->
      if n <> lea then
        update r;
      n+1)
    0 dict.rows in ()

let update_coeffs ent lea dict = (* row lea has been updated according to ent. update coeffs *) (** manque le coeff constant *) (** cette fonction est la précédante se simplifient si coeffs est une rows *)
  let _ = Array.fold
    (fun n x ->
      if n == ent then
        dict.coeffs.(n) <- dict.coeffs.(ent)*dict.rows.(lea).body.(n);
      else
        dict.coeffs.(n) <- dict.coeffs.(n)+dict.coeffs.(ent)*dict.rows.(lea).body.(n);
      n+1)
    0 dict.coeffs in 
  ()

let pivot ent lea dict = (* Pivot colum ent and row lea *)
  let ent_var = dict.vars.(ent) in (* name of the entering variable *)
  let piv_row = dict.rows.(lea) in (* row to be pivot *)
  let coef_piv = piv_row.body.(ent) in (* coeff of the entering variable into piv_row *)
    begin
      dict.vars.(ent) <- piv_row.head;
      piv_row.head <- ent_var;
      piv_row.body.(ent) <- neg F.one;  
      let _ = Array.fold 
        (fun n x -> 
          piv_row.row.(n) <- x / (neg coef_piv); 
          n+1) 
        0 piv_row.body;
      piv_row.const <- piv_row.const / (neg coef_piv);
      update_row ent lea dict; (* update the other rows *)
      update_coeffs ent lea dict (* update coeffs *)
    end

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

let auxiliary_dict aux_var dict = (* Start of first phase: add an auxiliary variable, called aux_var, to the dictionnary *)
  let aux_dic = 
    { vars = Array.append dict.var [|aux_var|]
    ; coeffs = Array.append (Array.make (Array.length dict.coeffs) F.zero) [|neg F.one|]
    ; rows = dict.rows
    } in
  array_update aux_dic.rows aux_dic.rows (fun n x -> Array.append x [|F.one|]) None in
  aux_dic

let project_basic position dict = (* project the dictionary when the auxiliary variable is basic *) (** possible ? *)
  { vars = dict.vars
  ; coeffs = dict.coeffs
  ; rows = partial_copy dict.rows position
  }

let project_non_basic coeffs_init vars_init aux_var dict = (* project the dictionary when the auxiliary variable is non basic *) (** <------ *)
  let pivot_pos = (* position of aux_var in dict.vars *)
    match array_find dict.vars (fun x -> x == aux_var) with
      | Some n -> n
      | None -> assert false in
  let new_rows = Array.make (Array.length dict.rows) dict.rows.(0) in
  array_update dict.rows new_rows (fun n r -> partial_copy r pivot_pos) None in
  let aux_var_coeff = dict.coeffs.(pivot_pos) in (* coefficient of the auxiliary variable in the objective function *)
  let new_coeffs = (** ne devrait contenir que des 0*) (** on suppose que coeffs est une row *)
    { head = dict.coeff.head
    ; body = partial_copy dict.coeffs pivot_pos 
    ; const = dict.const
    } in
  let _ = Array.fold
    (fun n v ->
      match (***) with
        | Some m -> add_rows new_coeffs dict.rows.(m) coeffs_init.(n) ; n+1
        | None -> (***) (** les dictionaires initiaux/actuels ne sont pas dans le même ordre *)
    )
    0 vars_init
    
  proj_dict =
    { vars = partial_copy dict.vars pivot_pos
    ; coeffs = new_coeffs
    ; rows = new_rows
    }
 
let project coeffs_init vars_init aux_var dict = (* End of first phase: project the dictionary according to aux_var *)
  let position =
    match array_find dict.rows (fun r -> r.head == aux_var) with
      | Some n -> n (* auxiliary variable is basic *) (* possible ? *)
      | None -> -1 in (* auxiliary variable is non basic *)
  if position <> -1 then
    project_basic coeffs_init vars_init aux_var dict
  else
    project_non_basic position dict

let first_phase dict = (* Simplex when first phase needed *)
  let aux_var = Array.length dict.rows + Array.length dict.vars + 1 in (* name of the auxiliary variable to add *)
  let dict = auxiliary_dict aux_var dict in (* add the auxiliary variable into the dictionary *)
  let coeffs_init = Array.copy dict.coeffs in (* save the coeffs for later (projection of first phase) *)
  let vars_init = Array.copy dict.vars in (* save the vars for later (projection of first phase) *)
  match pivots dict with (* apply first phase's pivots *)
    | Opt dict | Unbounded (dict,ent) ->
        let dict_proj = project coeffs_init vars_init aux_var dict in (* projection of the dictionary, remove the auxiliary variable *)
        if F.compare(dict.(**?*) F.zero) < 0 then
          Empty dict_proj
        else
          pivots dict_proj
    | _ -> assert false

let simplex dict = (* Apply the whole simplex *)
  match array_find dict.rows (fun r -> F.(compare r.const F.zero) < 0) with
    | Some _ -> first_phase dict
    | None -> pivots dict
