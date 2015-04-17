%parameter<Field : FIELD>

%start main
%type <Field.t Lp.t> main

%%

  main:
| program EOF                 { program }
  ;

  program:
| MAX objective ST constraints BOUNDS bounds VARS variables
  {  }
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
