open Field
open Lp
open Dictionary
open Simplex

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
       field_names^" Base Field")
    ] in

  Arg.parse speclist parse_input use_msg;
  default

let fields = [
  ("float",(module F_float : FIELD));
  ("num",(module F_num : FIELD));
  ("gmp",(module F_gmp : FIELD))
]

let main =
  let config = parse_args fields in
  let (module F : FIELD) = config.field in
  let lex = Lexing.from_channel config.input in
  let module F_parser = Parser.Make(F) in
  let module F_lp = Lp.Process(F) in
  let module F_dic = Dictionary.Make(F) in
  let module F_splx = Simplex.Make(F) in
  try
    let lp = F_parser.main Lexer.token lex in
    Printf.printf "Problem:\n%a\n" (F_lp.print true) lp;
    match F_dic.make lp with
    | Invalid_constraint (var, x1, x2) ->
      Printf.printf "Invalid constraint detected : %a <= %s <= %a\n"
        F.print x1
        var
        F.print x2
    | Conversion (conv, dic) ->
      match (F_splx.simplex dic) with
      | Opt dic -> 
          Printf.printf "Conversion:\n%a\nDictionary:\n%a"
          (F_dic.print_conv true) conv
          F_dic.print dic
      | _ -> ()
  with
  | Failure s ->
    let lexeme = Lexing.lexeme lex in
    let pos = Lexing.lexeme_start_p lex in
    let col = Lexing.(pos.pos_cnum - pos.pos_bol) in
    Printf.eprintf "Input error: (line %d, char %d) %s\n%s\n%!" pos.Lexing.pos_lnum col lexeme s; exit 1
  (*|  _ ->
    Printf.eprintf "Input error\n%!"; exit 1
  *)
