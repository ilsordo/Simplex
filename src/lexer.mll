{
open Tokens
}

let whitespace = [' ' '\t' '\n' '\r']
let var_name = ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
| whitespace           { token lexbuf }
| "//"                 { comment lexbuf }

| '-'                  { MINUS }
| '+'                  { PLUS }

| "<="                 { LEQ }
| ">="                 { GEQ }
| '<'                  { LT }
| '>'                  { GT }
| '='                  { EQ }

| "MINIMIZE"           { MIN }
| "MAXIMIZE"           { MAX }
| "SUBJECT TO"         { ST }
| "BOUNDS"             { BOUNDS }
| "VARIABLES"          { VARS }

| ['0'-'9']* as s    	 { NUM (int_of_string s) }
| var_name as s        { VAR s }

| eof                  { EOF }

and comment = parse
  | "\n"     { token lexbuf }
  | _        { comment lexbuf }
