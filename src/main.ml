open Field
open Lp
open Dictionary
open Simplex
open Profile
open Config

let fields = [
  ("float",(module F_float : FIELD));
  ("num",(module F_num : FIELD));
  ("gmp",(module F_gmp : FIELD))
]

let main =
  let config = parse_args fields in
  let exit n =
    dprintf "Done";
    print_stats stdout;
    print_times stdout;
    flush_all ();
    begin match config.action with
      | Explain (_, finalize) -> finalize ()
      | Solve -> ()
    end;
    exit n in
  let (module F : FIELD) = config.field in
  let lex = Lexing.from_channel config.input in
  let module F_parser = Parser.Make(F) in
  let module F_lp = Lp.Process(F) in
  let module F_dic = Dictionary.Make(F) in
  let module F_splx = Simplex.Make(F) in
  time "Init";
  let lp = try F_parser.main Lexer.token lex
    with Failure s ->
      let lexeme = Lexing.lexeme lex in
      let pos = Lexing.lexeme_start_p lex in
      let col = Lexing.(pos.pos_cnum - pos.pos_bol) in
      Printf.eprintf "Input error: (line %d, char %d) %s\n%s\n%!" pos.Lexing.pos_lnum col lexeme s;
      exit 1
  in
  time "Parsing";
  if !steps then Printf.printf "Problem:\n%a\n" F_lp.print lp;
  match F_dic.make lp with
  | exception Failure s ->
    Printf.eprintf "Error : %s" s;
    exit 1
  | Invalid_constraint (var, x1, x2) ->
      Printf.eprintf "Invalid constraint detected : %a <= %s <= %a\n"
        F.print x1 var F.print x2;
      exit 1
  | Conversion (conv, dic) ->
    time "Conversion";
    let dual = F_dic.dual dic in
    aprintf config.action "\\section{Problem} %a"
      F_lp.print lp;
    aprintf config.action "\\section{Initialization}\\subsection*{Conversion:}%a\\subsection*{Initial dictionary:}%a\\subsection*{Dual:}%a"
      F_dic.print_conv conv
      (F_dic.print ()) dic
      (F_dic.print ()) dual;
    match F_splx.simplex config.action dic with
    | Opt sol ->
      let s = F_dic.solution conv sol in
      let sol1 = (F_lp.check s lp) in
      if F.compare sol1 sol.coeffs.const <> 0 then
        (Printf.eprintf "Error:\nSolution from dict : %a\nSolution after eval : %a\n"
           F.print sol.coeffs.const
           F.print sol1;
         exit 1);
      begin match config.action with
        | Explain (c, _) ->
          Printf.fprintf c "\\section{Solution}Optimal solution: $%a$\\\\%a"
            F.print sol.coeffs.const
            F_dic.print_sol_tex s
        | Solve ->
          Printf.printf "Optimal solution : %a\n%a\n"
            F.print sol.coeffs.const
            F_dic.print_sol s
      end;
      exit 0
    | Empty sol ->
      begin match config.action with
        | Explain (c, _) ->
          Printf.fprintf c "\\section{Solution}Empty domain:\\\\%a"
            (F_dic.print ()) sol
        | Solve ->
          Printf.printf "Empty domain\n"
      end;
      exit 0
    | Unbounded (sol, n) ->
      let s = F_dic.solution conv sol in
      begin match config.action with
        | Explain (c, _) ->
          Printf.fprintf c "\\section{Solution}Unbounded domain: $%a$\\\\%a"
            F.print sol.coeffs.const
            F_dic.print_sol_tex s
        | Solve ->
          Printf.printf "Unbounded domain: %d\n%a\n"
            n
            F_dic.print_sol s
      end;
      exit 0
    | exception e ->
      Printf.eprintf "Uncaught exception : %s\n" (Printexc.to_string e);
      exit 1
