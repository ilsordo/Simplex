open Field
open Lp
open Dictionary
open Simplex
open Profile

type action = Print

type config =
  { mutable input : in_channel
  ; mutable field : (module FIELD)
  }

let parse_args fields =
  let default = {
    input = stdin;
    field = (module F_num : FIELD)
  } in

  let use_msg = "Usage:\nsimplex [input_file] [options]\n" in

  let parse_field s =
    try (default.field <- List.assoc s fields)
    with Not_found -> raise (Arg.Bad ("Unknown field: "^s)) in

  let parse_input f =
    try
      default.input <- open_in f
    with
    | Sys_error e ->
      raise (Arg.Bad ("Unable to read file "^f^"\nError:  "^e)) in

  let field_names =
    List.split fields
    |> fst
    |> String.concat "|"
    |> Printf.sprintf "[%s]" in

  let speclist = Arg.align [
      ("-f",     Arg.String parse_field,
       field_names^" Base Field");
      ("-d",     Arg.Set Profile.debug,
      " Debug");
      ("-s",     Arg.Set Profile.steps,
      " Steps");
      ("-p",     Arg.Set Profile.profile,
      " Steps")
    ] in

  Arg.parse speclist parse_input use_msg;
  default

let fields = [
  ("float",(module F_float : FIELD));
  ("num",(module F_num : FIELD));
  ("gmp",(module F_gmp : FIELD))
]

let exit n =
  print_stats stdout;
  print_times stdout;
  flush_all ();
  exit n

let main =
  let config = parse_args fields in
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
  begin
    time "Parsing";
    if !steps then Printf.printf "Problem:\n%a\n" (F_lp.print true) lp;
    match F_dic.make lp with
    | exception Failure s ->
      Printf.eprintf "Error : %s" s;
      exit 1
    | Invalid_constraint (var, x1, x2) ->
      Printf.printf "Invalid constraint detected : %a <= %s <= %a\n"
        F.print x1 var F.print x2;
    | Conversion (conv, dic) ->
      time "Conversion";
      if !steps then Printf.printf "Conversion:\n%a\nDictionary:\n%a\n"
          (F_dic.print_conv true) conv
          F_dic.print dic;
      match (F_splx.simplex dic) with
      | Opt sol ->
        Printf.printf "Optimal solution: %a\n%a\n"
          F.print sol.coeffs.const
          F_dic.print_sol (F_dic.solution conv sol)
      | Empty sol ->
        Printf.printf "Empty domain:\n%a\n"
          F_dic.print sol
      | Unbounded (sol, n) ->
        Printf.printf "Unbounded domain: %d\n%a\n"
          n
          F_dic.print sol
      | exception e ->
        Printf.eprintf "Uncaught exception : %s\n" (Printexc.to_string e);
        exit 1
  end;
  exit 0
