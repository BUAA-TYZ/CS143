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

static int comment_depth;

%}

/*
 * Define names for regular expressions here.
 */

DARROW  =>
LE  <=
ASSIGN <-
DIGIT  [0-9]
CHAR   [a-zA-Z0-9_]

%x  STR 
%x  CMT

%%

 /*
  *  Nested comments
  */

 /* Single line comment. */
--[^\n]*  {}

 /* Multiple lines comment. */
\*\)  {
  BEGIN(INITIAL);
  yylval.error_msg = "Unmatched *)";
  return (ERROR);
}

<INITIAL,CMT>\(\*  { 
  BEGIN(CMT); 
  ++comment_depth;
}

<CMT>[^*\n]

<CMT>"*"+[^*)\n]*

<CMT>\n  curr_lineno++;

<CMT>"*"+")"  {
  if (--comment_depth == 0) {
    BEGIN(INITIAL);
  }
}

<CMT><<EOF>> {
  BEGIN(INITIAL);
  yylval.error_msg = "EOF in comment";
  return (ERROR);
}


 /*
  *  The multiple-character operators.
  */

{DARROW}  { return (DARROW); }
{LE}  { return (LE); }
{ASSIGN}  { return (ASSIGN); }

 /*
  *  Integer.
  */
{DIGIT}+  { 
  yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST); 
}


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

t(?i:rue) { 
    yylval.boolean = 1;
    return (BOOL_CONST); 
}

f(?i:alse) {
  yylval.boolean = 0; 
  return (BOOL_CONST); 
}


 /*
  *  TYPEID. 
  */
[A-Z]{CHAR}*|SELF_TYPE {
  cool_yylval.symbol = idtable.add_string(yytext);
  return (TYPEID);
}


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\"  BEGIN(STR); yymore();

<STR>[^\\\"\n]*  yymore();

<STR>\\[^\n]  yymore();

<STR>\\\n {
  curr_lineno++;
  yymore();
}

<STR><<EOF>> {
  BEGIN(INITIAL);
  yylval.error_msg = "EOF in string constant";
  /* flushes the scanner’s internal buffer so that the next time the scanner attempts to match a token,
   it will first refill the buffer using YY_INPUT(). Or use yyrestart(yyin); */
  YY_FLUSH_BUFFER;
  return (ERROR); 
}

<STR>\n {
  BEGIN(INITIAL);
  yylval.error_msg = "Unterminated string constant";
  curr_lineno++;
  return (ERROR); 
}

<STR>\" {
  std::string input(yytext, yyleng);

  // remove the '\"'s on both sides.
  input = input.substr(1, input.length() - 2);

  std::string output = "";
  std::string::size_type pos;

  if (input.find_first_of('\0') != std::string::npos) {
    yylval.error_msg = "String contains null character";
    BEGIN 0;
    return ERROR;
  }

  while ((pos = input.find_first_of("\\")) != std::string::npos) {
    output += input.substr(0, pos);

    switch (input[pos + 1]) {
      case 'b':
        output += "\b";
        break;
      case 't':
        output += "\t";
        break;
      case 'n':
        output += "\n";
        break;
      case 'f':
        output += "\f";
        break;
      default:
        output += input[pos + 1];
        break;
    }

    input = input.substr(pos + 2, input.length() - 2);
  }

  output += input;

  if (output.length() > MAX_STR_CONST - 1) {
    BEGIN (INITIAL);
    yylval.error_msg = "String constant too long";
    return ERROR;
  }

  yylval.symbol = stringtable.add_string((char*)output.c_str());
  BEGIN (INITIAL);
  return (STR_CONST);
}


 /*
  *  OBJECTID. 
  */
[a-z]{CHAR}* {
  yylval.symbol = idtable.add_string(yytext);
  return (OBJECTID);
}


\n  curr_lineno++;
\0 {
  BEGIN(INITIAL);
  yylval.error_msg = "Code contains null character";
  return (ERROR);
}
[ \t\f\r\v]+ {}

 /* There are some unprintable characters, so must list all allowed characters. */
[+/\-*=<\.~,;:()@{}] { return yytext[0]; }

 /* Invalid char.  */
. {
  cool_yylval.error_msg = yytext;
  return (ERROR);
}

%%
