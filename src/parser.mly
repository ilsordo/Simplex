%parameter<F : Field.FIELD>

%{
open F
open Tokens

let add_const x (vars, const) = (vars, const + x)

let add_var v x (vars, const) = ((v,x)::vars, const)

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
  | (Unconstrained, c) | (c, Unconstrained) -> c
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
%type <F.t Lp.t> main program
%%

main:
  | program EOF                { $1 }
;

program:
  | MAX lc ST constraints BOUNDS bounds VARS variables
    { Lp.{ objective = $2; constraints = $4; bounds = bounds } }
  | MIN lc ST constraints BOUNDS bounds VARS variables
    { Lp.{ objective = neg_lc $2; constraints = $4; bounds = bounds } }
  | error                      { raise (Failure "program") }
;

constraints:
  |                              { [] }
  | constraints lc GEQ num       { (add_const (neg $4) $2)::$1 }
  | constraints lc GEQ MINUS num { (add_const $5 $2)::$1 }
  | constraints lc LEQ num       { (add_const $4 (neg_lc $2))::$1 }
  | constraints lc LEQ MINUS num { (add_const (neg $5) (neg_lc $2))::$1 }
  | constraints lc EQ num        { (add_const (neg $4) $2)::(add_const $4 (neg_lc $2))::$1}
  | constraints lc EQ MINUS num  { (add_const $5 $2)::(add_const (neg $5) (neg_lc $2))::$1}
  | error                        { raise (Failure "constraints") }
;

bounds:
  |                            { Hashtbl.clear bounds }
  | bounds VAR EQ num          { set_bound $2 (Lp.Both ($4, $4))}
  | bounds num LEQ VAR         { set_bound $4 (Lp.Inf $2)}
  | bounds VAR LEQ num         { set_bound $2 (Lp.Sup $4)}
  | bounds num GEQ VAR         { set_bound $4 (Lp.Sup $2)}
  | bounds VAR GEQ num         { set_bound $2 (Lp.Inf $4)}
  | bounds num LEQ VAR LEQ num { set_bound $4 (Lp.Both ($2, $6))}
  | bounds num GEQ VAR GEQ num { set_bound $4 (Lp.Both ($6, $2))}
  | error                      { raise (Failure "bounds") }
;

lc:
  | VAR                        { ([$1, one], zero) }
  | num                        { ([], $1) }
  | num VAR                    { ([$2, $1], zero) }
  | MINUS VAR                  { ([$2, neg one], zero) }
  | MINUS num                  { ([], neg $2) }
  | MINUS num VAR              { ([$3, neg $2], zero) }
  | lc PLUS VAR                { add_var $3 one $1 }
  | lc PLUS num                { add_const $3 $1 }
  | lc PLUS num VAR            { add_var $4 $3 $1 }
  | lc MINUS VAR               { add_var $3 (neg one) $1 }
  | lc MINUS num               { add_const (neg $3) $1 }
  | lc MINUS num VAR           { add_var $4 (neg $3) $1 }
  | lc error                   { raise (Failure "lc") }
;

variables:
  |                            { () }
  | variables VAR              { set_bound $2 Lp.Unconstrained }
  | error                      { raise (Failure "variables") }
;

num:
  NUM                          { match F.of_string $1 with
        Some x -> x | None -> raise (Failure( "Unable to parse number : "^$1))}
