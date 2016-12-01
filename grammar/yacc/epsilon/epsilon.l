%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"

int yywrap(void) {
  return 1;
}

%}

%x DOUBLE_STRING
%x B_COMMENT

space [ \n\r\t]+

comment //[^\n]*


%%

"="         return '=';
"+="        return AADD;
"-="        return ASUB;
"*="        return AMUL;
"/="        return ADIV;
"%="        return AMOD;
"<<="       return ALEFTSHIFT;
">>="       return ARIGHTSHIFT;
">>>="      return ALOGICALRIGHTSHIFT;
"&="        return ABITAND;
"^="        return ABITXOR;
"|="        return ABITOR;
"."         return '.';
"'"         return '\'';
"("         return '(';
")"         return ')';
"{"         return '{';
"}"         return '}';
"["         return '[';
"]"         return ']';
","         return ',';
"..."       return VAR_LEN_PARAM;
":"         return ':';
"::"        return LAMBDA;
"/"         return '/';
"%"         return '%';
"<"         return '<';
"<="         return LTEQ;
">"         return '>';
">="         return GTEQ;
"=="        return EQ;
"!="        return NE;
"&&"        return AND;
"||"        return OR;
";"         return ';';
"++"        return INC;
"--"        return DEC;
"&"         return '&';
"*"         return '*';
"+"         return '+';
"-"         return '-';
"!"         return '!';
"?"         return '?';
"if"        return IF;
"else1"        return ELSE1;
"else2"        return ELSE2;
"else3"        return ELSE3;
"else4"        return ELSE4;
"else5"        return ELSE5;
"else6"        return ELSE6;
"else7"        return ELSE7;
"else8"        return ELSE8;
"else9"        return ELSE9;
"elseA"        return ELSEA;
"elseB"        return ELSEB;
"elseC"        return ELSEC;
"for"       return FOR;
"else"      return ELSE;
"return"    return RETURN;
"false"     return FALSE;
"true"      return TRUE;
"null"      return NULL_LITERAL;
"function"  return FUNCTION;
"var"       return VAR;


0|[1-9][0-9]*             { return INT;}

\" { BEGIN DOUBLE_STRING; }
<DOUBLE_STRING>(\\.|[^\\"])* { return STRING; }
<DOUBLE_STRING>\" { BEGIN INITIAL; }

"/*"            { BEGIN(B_COMMENT); }
<B_COMMENT>"*/" { BEGIN(INITIAL); }
<B_COMMENT>.    { }
{space} {}

[/][/][^\n]* {}

[a-zA-Z0-9_]+ return NAME;

%%
