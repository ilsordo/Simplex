%token <string> NUM VAR
%token MINUS PLUS
%token LEQ GEQ LT GT EQ
%token MIN MAX ST BOUNDS VARS EOF

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
