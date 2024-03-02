%{

#define YYSTYPE double
extern YYSTYPE yylval;
#include "calc.tab.h"

%}

DIGIT [0-9]

%%
[ \t]+ {}
\n { return '\n'; }
\+  { return ADD; }
\-  { return MINUS; }
\*  { return MUP; }
\/  { return DIV; }
\(|\)|\^  { return yytext[0]; }


{DIGIT}+|{DIGIT}+\.{DIGIT}+  { sscanf(yytext, "%lf", &yylval); return NUM; }

%%