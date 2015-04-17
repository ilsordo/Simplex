%parameter<Field : Field.FIELD>

%{
open Field
open Tokens

let add_const x (vars, const) = (vars, const + x)

let add_var x v (vars, const) = ((v,x)::vars, const)

let rec neg_lc_aux acc = function
  | [] -> acc
  | (v,x)::q -> neg_lc_aux ((v, neg x)::acc) q

let neg_lc (vars, const) =
  (neg_lc_aux [] vars, neg const)

let bounds = Hashtbl.create 10

let max a b = if compare a b > 0 then a else b
and min a b = if compare a b > 0 then b else a

let join b1 b2 =
  let open Lp in
  match (b1, b2) with
  | (Unconstrained, _) | (_, Unconstrained) -> Unconstrained
  | (Inf a, Sup b) -> Both (a, b)
  | (Sup a, Inf b) -> Both (b, a)
  | (Inf a, Inf b) -> Inf (max a b)
  | (Sup a, Sup b) -> Sup (min a b)
  | (Both (a1, a2), Sup b) -> Both (a1, min a2 b)
  | (Both (a1, a2), Inf b) -> Both (max a1 b, a2)
  | (Sup a, Both (b1, b2)) -> Both (b1, min a b2)
  | (Inf a, Both (b1, b2)) -> Both (max a b1, b2)
  | (Both (a1, a2), Both (b1, b2)) -> Both (max a1 b1, min a2 b2)

let set_bound var bound =
  try
    Hashtbl.(join bound (find bounds var) |> replace bounds var)
  with
    Not_found -> Hashtbl.add bounds var bound
%}

%start main
%type <Field.t Lp.t> main

%%

main:
  | program EOF                 { $1 }
;

program:
  | MAX lc ST constraints BOUNDS bounds VARS variables
    { Lp.{ objective = $2; constraints = $4; bounds = bounds } }
  | MIN lc ST constraints BOUNDS bounds VARS variables
    { Lp.{ objective = neg_lc $2; constraints = $4; bounds = bounds } }
;

constraints:
  | lc GEQ NUM                 { add_const (neg $3) $1 }
  | lc GEQ MINUS NUM           { add_const $3 $1 }
  | lc LEQ NUM                 { add_const $3 (neg_lc $1) }
  | lc LEQ MINUS NUM           { add_const (neg $3) (neg_lc $1) }
;

bounds:
  |                            { Hashtbl.clear bounds }
  | bounds VAR EQ NUM          { set_bound $2 (Both ($4, $4))}
  | bounds NUM LEQ VAR         { set_bound $4 (Inf $2)}
  | bounds VAR LEQ NUM         { set_bound $2 (Inf $4)}
  | bounds NUM GEQ VAR         { set_bound $4 (Sup $2)}
  | bounds VAR GEQ NUM         { set_bound $2 (Sup $4)}
  | bounds NUM LEQ VAR LEQ NUM { set_bound $4 (Both ($2, $6))}
  | bounds NUM GEQ VAR GEQ NUM { set_bound $4 (Both ($6, $2))}

lc:
  | VAR                        { ([$1, one], zero) }
  | NUM                        { ([], $1) }
  | NUM VAR                    { ([$1, $2], zero) }
  | MINUS VAR                  { ([$1, neg one], zero) }
  | MINUS NUM                  { ([], neg $1) }
  | MINUS NUM VAR              { ([$1, neg $2], zero) }
  | lc PLUS VAR                { add_var $3 one $1 }
  | lc PLUS NUM                { add_const $3 $1 }
  | lc PLUS NUM VAR            { add_var $4 $3 $1 }
  | lc MINUS VAR               { add_var $3 (neg one) $1 }
  | lc MINUS NUM               { add_const (neg $3) $1 }
  | lc MINUS NUM VAR           { add_var $4 (neg $3) $1 }
;

variables:
  |                            { () }
  | variables VAR              { set_bound $2 Unconstrained }
;

num:
  NUM                          { match F.of_string $1 with Some x -> x | None -> raise Failure ("Unable "}
