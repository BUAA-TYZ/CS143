/* Infix notation calculator. */

%{
  #include <ctype.h>
  #include <math.h>
  #include <stdio.h>
  #include <stdlib.h>

  extern int yylex (void);
  void yyerror (char const *);
  int yywrap() { return 1; }
  #define YYSTYPE double
  extern YYSTYPE yylval;
%}

/* Bison declarations. */
%define api.value.type {double}
%token NUM  ADD  MINUS  MUP  DIV
%left MINUS  ADD
%left MUP  DIV
%precedence NEG   /* negation--unary minus */
%right '^'        /* exponentiation */

%% /* The grammar follows. */
input:
  %empty
| input line
;

line:
  '\n'
| exp '\n'  { printf ("\tResult = %.10g\n", $1); }
;

exp:
  NUM
| exp ADD exp        { $$ = $1 + $3;      }
| exp MINUS exp        { $$ = $1 - $3;      }
| exp MUP exp        { $$ = $1 * $3;      }
| exp DIV exp        { $$ = $1 / $3;      }
| MINUS exp  %prec NEG { $$ = -$2;          }
| exp '^' exp        { $$ = pow ($1, $3); }
| '(' exp ')'        { $$ = $2;           }
;
%%

void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

int main (void)
{
  return yyparse ();
}