type var = Real of string | Slack of int | Expand

type varmap = { vars : var array
              ; ids : (var, int) Hashtbl.t
              }

let make ~(expand=false) names =
  let ids =
    if expand then
      Hashtbl.(add empty Expand 0)
    else
      Hashtbl.empty
  in
  let (len, ids) = List.fold_left (fun (len, ids) name -> )
  let vars = Array.make Expand
