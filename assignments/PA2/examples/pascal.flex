/* scanner for a toy Pascal-like language */

%{
  /* need this for the call to atof() below */
  #include <math.h>
%}

/* definition */
DIGIT  [0-9]
ID  [a-zA-Z][a-zA-Z0-9]*

/* rules */
%%
{DIGIT}+ {
    printf("An integer: %s (%d)\n", yytext, atoi(yytext));
  }

{DIGIT}+"."{DIGIT}* {
    printf("A float: %s (%g)\n", yytext, atof(yytext));
  }

(?i:if|then|begin|end|procedure|function) {
    printf("A Keyword: %s\n", yytext);
  }

{ID}  printf("An identifier:%s\n", yytext);

"+"|"-"|"*"|"/"  printf("An operator:%s\n", yytext);

"{"[^{}\n]*"}"  

[ \t\n]+

.  printf("Unrecognized character:%s\n", yytext);

%%

int main(int argc, char **argv) {
  ++argv;  --argc;
  if (argc > 0) 
    yyin = fopen(argv[0], "r");
  else
    yyin = stdin;
  yylex();
}

int yywrap() {
  return 1;
}

