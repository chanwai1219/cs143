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
unsigned int nested = 0;

%}

/*
 * Define names for regular expressions here.
 */
%x LINE_COMMENT BLOCK_COMMENT STRING

NEW_LINE        \n

DARROW          =>
ASSIGN          <-
LE              <=

CLASS           [cC]lass
IF              if
THEN            then
ELSE            else
FI              fi
IN              in
INHERITS        inherits
LET             let
WHILE           while
LOOP            loop
POOL            pool
CASE            case
OF              of
ESAC            esac
NEW             new
ISVOID          isvoid
NOT             not

INT_CONST       [0-9]+
BOOL_CONST      (t[rR][uU][eE]|f[aA][lL][sS][eE])

TYPEID          ([A-Z][a-zA-Z0-9_]*)
OBJECTID        ([a-z][a-zA-Z0-9_]*)

WHITE_SPACE     ([ \f\r\t\v]+)
OPERATOR        ([+\-*/{}():;,.@~<=])

%%

 /*
  *  Nested comments
  */
--                { BEGIN(LINE_COMMENT); }
<LINE_COMMENT>\n  { BEGIN(INITIAL); curr_lineno++; }
<LINE_COMMENT>.   { }

\(\*                          { BEGIN(BLOCK_COMMENT); nested++; }
\*\)                          { strcpy(cool_yylval.error_msg, "Unmatched *)"); return (ERROR); }
<BLOCK_COMMENT>[^*(\n]*      { /* eat anything that's not '*' or '\n' */ }
<BLOCK_COMMENT>\n             { curr_lineno++; }
<BLOCK_COMMENT>\(\*           { nested++; }
<BLOCK_COMMENT>\*\)           { nested--; if (nested == 0) { BEGIN(INITIAL); } }
<BLOCK_COMMENT>.              { }
<BLOCK_COMMENT><<EOF>> { 
	strcpy(cool_yylval.error_msg, "EOF in comment");
	BEGIN(INITIAL);
  return (ERROR);
}

 /*
  *  The multiple-character operators.
  */
{DARROW}		{ return (DARROW); }
{ASSIGN}    { return (ASSIGN); }
{LE}        { return (LE); }
{OPERATOR}  { return yytext[0]; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */
{CLASS}     { return (CLASS); }
{IF}        { return (IF); }
{THEN}      { return (THEN); }
{ELSE}      { return (ELSE); }
{FI}        { return (FI); } 
{IN}        { return (IN); } 
{INHERITS}  { return (INHERITS); } 
{LET}       { return (LET); } 
{WHILE}     { return (WHILE); } 
{LOOP}      { return (LOOP); } 
{POOL}      { return (POOL); } 
{OF}        { return (OF); } 
{CASE}      { return (CASE); } 
{ESAC}      { return (ESAC); } 
{NEW}       { return (NEW); } 
{ISVOID}    { return (ISVOID); } 
{NOT}       { return (NOT); } 

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */

\" {
  BEGIN(STRING);
  string_buf_ptr = string_buf;
}
<STRING>\" {
  BEGIN(INITIAL);
  *string_buf_ptr++ = '\0';
  cool_yylval.symbol = stringtable.add_string(string_buf);
  return (STR_CONST);
}
<STRING>\\t   { *string_buf_ptr++ = '\t'; }
<STRING>\\b   { *string_buf_ptr++ = '\b'; }
<STRING>\\f   { *string_buf_ptr++ = '\f'; }
<STRING>\\n   { *string_buf_ptr++ = '\n'; }
<STRING>\\\\  { *string_buf_ptr++ = '\\'; }
<STRING>\\\"  { *string_buf_ptr++ = '\"'; }
<STRING>\\\n  { *string_buf_ptr++ = '\n'; curr_lineno++; }
<STRING>\n    {
  BEGIN(INITIAL);
  curr_lineno++;
  strcpy(cool_yylval.error_msg, "Unterminated string constant");
  return (ERROR);
}
<STRING>.     {
  if (string_buf_ptr == &string_buf[MAX_STR_CONST-2]) {
    BEGIN(INITIAL);
    strcpy(cool_yylval.error_msg, "String constant too long"); 
    return (ERROR);
  }
  *string_buf_ptr++ = yytext[0];
}
<STRING><<EOF>> {
  BEGIN(INITIAL);
  strcpy(cool_yylval.error_msg, "String constant too long"); 
  return (ERROR);
}
<STRING>\\0 {
  BEGIN(INITIAL);
  strcpy(cool_yylval.error_msg, "String contains null character"); 
  return (ERROR);
}


{INT_CONST} {
  cool_yylval.symbol = inttable.add_string(yytext);
  return (INT_CONST);
}
{BOOL_CONST} {
  if (yytext[0] == 't')
    cool_yylval.boolean = 1;
  else
    cool_yylval.boolean = 0;

  return (BOOL_CONST); }

{WHITE_SPACE} { /* ignore white space */ }

{NEW_LINE}  { curr_lineno++; }
<<EOF>>     { return 0; }

{TYPEID}	{
	cool_yylval.symbol = idtable.add_string(yytext);
	return (TYPEID);
}

{OBJECTID}	{
	cool_yylval.symbol = idtable.add_string(yytext);
	return (OBJECTID);
}

.	{
	strcpy(cool_yylval.error_msg, yytext); 
	return (ERROR);
}

%%

int yywrap() { 
    return 1;
}
