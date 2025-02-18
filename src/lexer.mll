{
open Tokens
}

let whitespace = [' ' '\t' '\r']
let var_name = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let int = ['0'-'9']+
let float = ['0'-'9']*'.'int
let rat = (int)'/'int
let num = '-'?(int|float|rat)

rule token = parse
| '\n'                 { Lexing.new_line lexbuf; token lexbuf }
| whitespace           { token lexbuf }
| "//"                 { comment lexbuf }

| '-'                  { MINUS }
| '+'                  { PLUS }

| "<="                 { LEQ }
| ">="                 { GEQ }
| '='                  { EQ }

| "MINIMIZE"           { MIN }
| "MAXIMIZE"           { MAX }
| "SUBJECT TO"         { ST }
| "BOUNDS"             { BOUNDS }
| "VARIABLES"          { VARS }

| num as s    	       { NUM s }
| var_name as s        { VAR s }

| eof                  { EOF }

and comment = parse
  | '\n'               { Lexing.new_line lexbuf; token lexbuf }
  | _                  { comment lexbuf }
