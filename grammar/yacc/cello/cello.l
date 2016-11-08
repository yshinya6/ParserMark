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
"int"       return INT_TYPE;
"long"      return LONG_TYPE;
"boolean"   return BOOLEAN_TYPE;
"if"        return IF;
"if0"        return IF0;
"if1"        return IF1;
"if2"        return IF2;
"if3"        return IF3;
"if4"        return IF4;
"if5"        return IF5;
"if6"        return IF6;
"if7"        return IF7;
"if8"        return IF8;
"if9"        return IF9;
"ifA"        return IFA;
"ifB"        return IFB;
"ifC"        return IFC;
"ifD"        return IFD;
"ifE"        return IFE;
"for"       return FOR;
"else"      return ELSE;
"return"    return RETURN;
"false"     return FALSE;
"true"      return TRUE;
"null"      return NULL_LITERAL;
"import"    return IMPORT;
"dummy1"    return DUMMY1;
"dummy2"    return DUMMY2;

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
