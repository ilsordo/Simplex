%parameter<Field : FIELD>

%start main
%type <int*(((bool*int) list) list)> main

%%

  main:
| program EOF                 { $1 }
  ;

  program:
| MAX objective ST constraints BOUNDS bounds VARS variables	      { ($2,$4,$6,$8) }
| MAX objective ST constraints BOUNDS bounds VARS variables	      { ($2,$4,$6,$8) }
  ;

  objective:
| clause ENDC liste_clause    { $1::$3}
|                             { [] }
  ;

  constraints:
| 

  clause:
| literal clause             {$1::$2}
|                             {[]}
  ;

literal:
| INT                         {(true,$1)}
| MINUS INT                   {(false,$2)}
