%{
  #include <stdio.h>
  #include <stdlib.h>
  #include "xml.h"
  #include "y.tab.h"

  extern int yyerror(const char *s);

  //#undef YY_INPUT
  //#define YY_INPUT(buf, result, max_size) (result = json_yyinput(buf, max_size))

  int my_yyinput(char *buf, int max_size);

  //extern "C"{
  int yywrap(void){
    return 1;
  }

  int my_yyinput(char *buf, int max_size) {
    int len;
    char c;
    extern FILE *yyin;

    if (feof(yyin)) {
      return 0;
    }

    for (len = 0; len < max_size; len++) {
      if ((c = fgetc(yyin)) == EOF) {
        break;
      }
      buf[len] = c;
    }

    return len;
  }
  //}

%}

%x STRING
%x STRING_UCS_2
%x XMLTAG
%x XMLENDTAG
%x ATTRIBUTE
%x COMMENT
%x CDATASEC
%x PROLOGSEC
%x DTDSEC

space   [ \t\r\n]
alpha   [a-zA-Z]
name    [a-zA-Z:]((-|[a-zA-Z:0-9._])*)
text    [^<]*
value   [^\"]
hex     [0-9a-fA-F]
cdata   [^<>&]
prolog  (([^?])*)
dtd     [^>]*

%%

<INITIAL>"<" {
  //fprintf(stderr, "<");
  BEGIN XMLTAG;
  return '<';
}

<INITIAL>">" {
  //fprintf(stderr,"_END>");
  return '>';
}

<INITIAL>"<!" {
  BEGIN DTDSEC;
  return DTDSTART;
}

<INITIAL>"<?xml" {
  BEGIN PROLOGSEC;
  //fprintf(stderr, "<?xml");
  return PROLOGPREFIX;
}

<INITIAL>"</" {
  BEGIN XMLENDTAG;
  //fprintf(stderr, "</END ");
  return ENDTAG;
}

<INITIAL>{space}* {
  //fprintf(stderr, "(S)\n");
}

<INITIAL>{text} {
  //fprintf(stderr, "\"%s\"", yytext);
  return TEXT;
}

<INITIAL>"<![CDATA[" {
  BEGIN CDATASEC;
  return CDATASTART;
}

<INITIAL>"<!--" {
  BEGIN COMMENT;
}

<PROLOGSEC>{prolog} {
  //fprintf(stderr, "prologdata");
  return PROLOG;
}

<PROLOGSEC>"?>" {
  BEGIN INITIAL;
  //fprintf(stderr, " ?>");
  return PROLOGSUFFIX;
}

<DTDSEC>{dtd} {
  return DTD;
}

<DTDSEC>">" {
  BEGIN INITIAL;
  return '>';
}

<XMLTAG>{name} {
  //fprintf(stderr, "NAME(%s)", yytext);
  return NAME;
}

<XMLTAG>{space}* {
  //fprintf(stderr, "(S)\n");
}

<XMLENDTAG>{name} {
  //fprintf(stderr, " NAME(%s)", yytext);
  return NAME;
}

<XMLENDTAG>">" {
  //fprintf(stderr, ">ENDTAG");
  BEGIN INITIAL;
  return '>';
}

<XMLENDTAG>{space}* {
  //fprintf(stderr, "(S)");
}

<XMLTAG>"=" {
  //fprintf(stderr, "%s", yytext);
  return '=';
}

<XMLTAG>\" {
  BEGIN STRING;
}

<XMLTAG>"/>" {
  BEGIN INITIAL;
  //fprintf(stderr, "/>");
  return FIRSTTAGEND;
}

<XMLTAG>">" {
  BEGIN INITIAL;
  //fprintf(stderr, " BEGINTAG>");
  return '>';
}

<COMMENT>"-->" {
  BEGIN INITIAL;
}

<STRING>{value} {
  //fprintf(stderr, "%s", yytext);
  //yylval.xvalue = xml_new_string(yytext);
  //return VALUE;
}

<STRING>\" {
  BEGIN XMLTAG;
  return VALUE;
}

<CDATASEC>"]]>" {
  BEGIN INITIAL;
  return CDATAEND;
}

<CDATASEC>{cdata}* {
  return CDATA;
}

%%
