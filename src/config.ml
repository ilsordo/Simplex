open Field
open Profile

type action = Solve
            | Explain of out_channel * (unit -> unit)

type config =
  { mutable input : in_channel
  ; mutable field : (module FIELD)
  ; mutable action : action
  }

let parse_args fields =
  let default =
    { input = stdin
    ; field = (module F_num : FIELD)
    ; action = Solve
    } in

  let use_msg = "Usage:\nsimplex [input_file] [options]\n" in

  let parse_field s =
    try (default.field <- List.assoc s fields)
    with Not_found -> raise (Arg.Bad ("Unknown field: "^s)) in

  let parse_output s =
    try
      let p = Unix.open_process_in "mktemp -qd --tmpdir simplexXXX" in
      let old_dir = Unix.getcwd () in
      let dir = Scanf.fscanf p ("%s\n") (fun x -> x) in
      ignore (Unix.close_process_in p);
      let out = open_out (dir^"/"^s^".tex") in
      Printf.fprintf out "\\documentclass{article}\\begin{document}";
      let finalize () =
        try
          Printf.fprintf out "\\end{document}\n%!";
          close_out out;
          Unix.chdir dir;
          let command = Printf.sprintf "pdflatex %s.tex && mv %s.pdf %s/%s.pdf"
              s s old_dir s in
          ignore (Unix.system command);
        with _ -> Printf.eprintf "Error while producing latex output\n%!"; exit 1
      in
      default.action <- Explain (out, finalize)
    with _ -> raise (Arg.Bad ("Unable to create directory for latex output")) in

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
      ("-field", Arg.String parse_field,
       field_names^" Base Field");
      ("-print", Arg.String parse_output,
       "file Print details about the execution in file.pdf");
      ("-d",     Arg.Set Profile.debug,
      " Debug");
      ("-p",     Arg.Set Profile.profile,
      " Profile")
    ] in

  Arg.parse speclist parse_input use_msg;
  default
