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
"/"         return '/';
"%"         return '%';
"<"         return '<';
">"         return '>';
"=="        return EQ;
"!="        return NE;
"^"         return '^';
"|"         return '|';
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
"string"    return STRING_TYPE;
"int"       return INT_TYPE;
"long"      return LONG_TYPE;
"boolean"   return BOOLEAN_TYPE;
"if"        return IF;
"for"       return FOR;
"else"      return ELSE;
"return"    return RETURN;
"false"     return FALSE;
"true"      return TRUE;
"null"      return NULL_LITERAL;
"import"    return IMPORT;

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
