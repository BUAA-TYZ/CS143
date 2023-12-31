/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

%}

/*
 * Define names for regular expressions here.
 */

DARROW  =>
DIGIT  [0-9]
CHAR   [a-zA-Z0-9_]

%x  str
%x  cmt

%%

 /*
  *  Nested comments
  */

 /* Single line comment. */
--[^\n]*  {}

 /* Multiple lines comment. */
\*\)  {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "Unmatched *)";
  return (ERROR);
}

\(\*  BEGIN(cmt);

<cmt>[^*\n]

<cmt>"*"+[^*)\n]*

<cmt>\n  curr_lineno++;

<cmt>"*"+")"  BEGIN(INITIAL);

<cmt><<EOF>> {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "EOF in comment";
  return (ERROR);
}


 /*
  *  The multiple-character operators.
  */

{DARROW}  { return (DARROW); }

 /*
  *  Integer.
  */
{DIGIT}+  { 
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST); 
}

 /*
  *  TYPEID. 
  */
[A-Z]{CHAR}+|SELF_TYPE {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}

 /*
  *  OBJECTID. It's put at the end.
  */


 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

(?i:class)  return (CLASS); 
(?i:if)   return (IF); 
(?i:fi)   return (FI); 
(?i:inherits)   return (INHERITS); 
(?i:else)   return (ELSE); 
(?i:in)   return (IN); 
(?i:isvoid)   return (ISVOID); 
(?i:let)   return (LET); 
(?i:pool)   return (POOL); 
(?i:loop)   return (LOOP); 
(?i:then)   return (THEN); 
(?i:while)   return (WHILE); 
(?i:case)   return (CASE); 
(?i:esac)   return (ESAC); 
(?i:new)   return (NEW); 
(?i:of)   return (OF); 
(?i:not)   return (NOT); 

t[Rr][Uu][Ee] { 
    yylval.boolean = 1;
    return (BOOL_CONST); 
}

f[Aa][Ll][Ss][Ee] {
  yylval.boolean = 0; 
  return (BOOL_CONST); 
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\"   string_buf_ptr = string_buf; BEGIN(str);

<str>\" {
  BEGIN(INITIAL);
  *string_buf_ptr = '\0';
  cool_yylval.symbol = stringtable.add_string(string_buf);
  return (STR_CONST);
}

<str>\n {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "Unterminated string constant";
  return (ERROR);
}

<str>\0 {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "String contains null character";
  return (ERROR);
}

<str>\\n  *string_buf_ptr++ = '\n';
<str>\\t  *string_buf_ptr++ = '\t';
<str>\\r  *string_buf_ptr++ = '\r';
<str>\\b  *string_buf_ptr++ = '\b';
<str>\\f  *string_buf_ptr++ = '\f';

 /* \x <=> x. Skip the '\' */
<str>\\(.|\n)  {
  if (string_buf_ptr - string_buf + 1 >= MAX_STR_CONST) {
    BEGIN(INITIAL);
    cool_yylval.error_msg = "String constant too long";
    return (ERROR);
  }
  *string_buf_ptr++ = yytext[1];
}
 /* copy the content */
<str>[^\\\n\"]+  {
  char *yptr = yytext;
  while (*yptr) {
    if (string_buf_ptr - string_buf + 1 >= MAX_STR_CONST) {
      BEGIN(INITIAL);
      cool_yylval.error_msg = "String constant too long";
      return (ERROR);
    }
    *string_buf_ptr++ = *yptr++;
  }
}

 /*
  *  OBJECTID. 
  */
[a-z]{CHAR}* {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}


\<-  return (ASSIGN);
\n  curr_lineno++;
[ \t\f\r\v]+ {}

 /* Invalid char */
[\[\]\'>] {
    cool_yylval.error_msg = yytext;
    return (ERROR);
}

. { return yytext[0]; }

%%
