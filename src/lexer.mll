{
open Tokens
}

let whitespace = [' ' '\t' '\n' '\r']

rule token = parse0
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

| eof                  { EOF }
               |


and comment = parse
  | "\n"     { token lexbuf }
  | _        { comment lexbuf }
