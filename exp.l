%{
#include <stdio.h>
#include "y.tab.h"
%}
%x COMMENT
%%

    /* Types and Keywords */
bool        { return BOOL; }
char        { return CHAR; }
int         { return INT; }
double      { return DOUBLE; }
float       { return FLOAT; }
string      { return STRING; }
int\*       { return INTPTR; }
char\*      { return CHARPTR; }
double\*    { return DOUBLEPTR; }
float\*     { return FLOATPTR; }
var         { return VARIABLE; }

    /* Statement and Loops */
if          { return IF; }
else        { return ELSE; }
while       { return WHILE; }
do          { return DO; }
for         { return FOR; }

    /* Function Lexemes */
args>>      { return ARGS; }
public      { return PUBLIC; }
private     { return PRIVATE; }
static      { return STATIC; }
return      { return RETURN; }
void        { return VOID; }
main        { return MAIN; }

    /* Operators */
"&&"        { return AND; }
"/"         { return DIVISION; }
"<-"        { return ASSIGNMENT; }
"=="        { return EQL; }
">"         { return GREATER; }
">="        { return GREATEREQL; }
"<"         { return LESS; }
"<="        { return LESSEQL; }
"-"         { return MINUS; }
"!"         { return NOT; }
"!="        { return NOTEQL; }
"||"        { return OR; }
"+"         { return PLUS; }
"*"         { return MULTI; } 
"&"         { return ADDRESS; }

    /* Array-specific Lexemes */
"["         { return OPENBRACKET; }
"]"         { return CLOSEBRACKET; }
"|"       { return LENGTH; }


    /* Comment handling */
"/*" { BEGIN(COMMENT); }      
<COMMENT>"*/" { BEGIN(INITIAL); } 
<COMMENT>\n {;}               
<COMMENT>. {;}   


    /* Other Lexemes */
";"         { return SEMICOLON; }
":"         { return COLON; }
","         { return COMMA; }
"{"         { return OPENBRACE; }
"}"         { return CLOSEBRACE; }
"("         { return OPENPAREN; }
")"         { return CLOSEPAREN; }
"null"      { return NULL_TOKEN; }
[\n]        { yylineno++; } 
[ \t]+      ;

true    { return BOOLTRUE; }
false   { return BOOLFALSE; }
\'[^\']\' { yylval.string = strdup(yytext); return CHAR_LTL; }
[1-9][0-9]*|[0-9] { yylval.string = strdup(yytext); return NUM; }
0[xX][0-9A-Fa-f]+ { yylval.string = strdup(yytext); return HEX_LTL; }
[-+]?[0-9]*\.?[0-9]*[eE]?[-+]?[0-9]+ { yylval.string = strdup(yytext); return DOUBLE_LTL; }
[0-9]*\.?[0-9]+[fF] { yylval.string = strdup(yytext); return FLOAT_LTL; }
\"[^\"]*\" { yylval.string = strdup(yytext); return STRING_LTL; }
[a-zA-Z_][a-zA-Z0-9_]* { yylval.string = strdup(yytext); return IDENTIFIER; }
. { return yytext[0]; }

%%

int yywrap() {
    return 1;
}
