%{
#include <stdio.h>
#include <stdlib.h>
#include "y.tab.h"

int yywrap(void) {
  return 1;
}

%}

%x INTEGER_LITERAL
%x FLOATING_LITERAL
%x CHARACTER_LITERAL
%x DOUBLE_STRING
%x B_COMMENT

space [ \n\r\t]+

comment //[^\n]*

DIGIT [0-9]
DIGITS {DIGIT}([_]*{DIGIT})*
HEXDIGIT [a-fA-F0-9]
HEXDIGITS {HEXDIGIT}([_]*{HEXDIGIT})*
INT_SUFFIX [lL]

DECIMAL 0|[1-9]{DIGITS}
HEX 0[xX]{HEXDIGITS}
BINARY 0[bB]{BINARYDIGITS}
OCTAL 0("_"*{OCTALDIGIT})*


FLOAT_SUFFIX [fF]
DOUBLE_SUFFIX [dD]
BINARYDIGIT [01]
BINARYDIGITS {BINARYDIGIT}(_*{BINARYDIGIT})*
OCTALDIGIT [0-7]
SIMPLE_ESCAPE "\\\\"['\"\\bfnrt]
FRACTION ("."{DIGITS})|({DIGITS}"."{DIGITS}?)
EXPONENT [eE][+-]?{DIGITS}
BINARYEXPONENT [pP][+-]?{DIGITS}
HEXFRACTION 0[xX]"."{HEXDIGITS}

FLOAT ({FRACTION}{EXPONENT}?|{DIGITS}{EXPONENT}|{HEXFRACTION}{BINARYEXPONENT})





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
"@"         return '@';
"->"        return RARROW;
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
"::"        return MR;
"/"         return '/';
"%"         return '%';
"<<"        return LEFTSHIFT;
">>"        return RIGHTSHIFT;
">>>"       return LOGICALRIGHTSHIFT;
"<="        return LTEQ;
">="        return GTEQ;
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
"~"         return '~';
"!"         return '!';
"?"         return '?';

"abstract"  return ABSTRACT;
"assert"    return ASSERT;
"boolean"   return BOOLEAN_TYPE;
"break"     return BREAK;
"byte"      return BYTE_TYPE;
"case"      return CASE;
"catch"     return CATCH;
"char"      return CHAR_TYPE;
"class"     return CLASS;
"const"     return CONST;
"continue"  return CONTINUE;
"default"   return DEFAULT;
"do"        return DO;
"double"    return DOUBLE_TYPE;
"else"      return ELSE;
"enum"      return ENUM;
"extends"   return EXTENDS;
"false"     return FALSE;
"final"     return FINAL;
"finally"   return FINALLY;
"float"     return FLOAT_TYPE;
"for"       return FOR;
"goto"      return GOTO;
"if"        return IF;
"implements" return IMPLEMENTS;
"import"    return IMPORT;
"instanceof" return INSTANCEOF;
"int"       return INT_TYPE;
"interface" return INTERFACE;
"long"      return LONG_TYPE;
"native"    return NATIVE;
"new"       return NEW;
"null"      return NULL_LITERAL;
"package"   return PACKAGE;
"private"   return PRIVATE;
"protected" return PROTECTED;
"public"    return PUBLIC;
"return"    return RETURN;
"short"     return SHORT_TYPE;
"static"    return STATIC;
"strictfp"  return STRICTFP;
"super"     return SUPER;
"switch"    return SWITCH;
"synchronized" return SYNCHRONIZED;
"this"      return THIS;
"throw"     return THROW;
"throws"    return THROWS;
"transient" return TRANSIENT;
"true"      return TRUE;
"try"       return TRY;
"void"      return VOID_TYPE;
"volatile"  return VOLATILE;
"while"     return WHILE;


({DECIMAL}|{HEX}|{BINARY}|{OCTAL}){INT_SUFFIX}? {return INT;}

{FRACTION}{EXPONENT}?{FLOAT_SUFFIX}? {return FLOAT;}
{DIGITS}{EXPONENT}{FLOAT_SUFFIX}? {return FLOAT;}
{HEXFRACTION}{BINARYEXPONENT}{FLOAT_SUFFIX}? {return FLOAT;}

\" { BEGIN DOUBLE_STRING; }
<DOUBLE_STRING>(\\.|[^\\"])* { return STRING; }
<DOUBLE_STRING>\" { BEGIN INITIAL; }

[a-zA-Z_]?'(\\.|[^\\'])+'	{ return CHAR; }



"/*"            { BEGIN(B_COMMENT); }
<B_COMMENT>"*/" { BEGIN(INITIAL); }
<B_COMMENT>.    { }
{space} {}

[/][/][^\n]* {}

[a-zA-Z0-9_$]+ {return NAME;}

%%
