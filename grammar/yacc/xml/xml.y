%{
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  #include <sys/time.h> // gettimeofday
  #include "xml.h"

  int yyerror(const char *s);

  uint64_t timer() {
  struct timeval tv;
  gettimeofday(&tv, NULL);
  return tv.tv_sec * 1000 + tv.tv_usec / 1000;
  }


  XML *xml;
%}

%union {
  XMLProlog *xprolog;
  XMLElement *xelement;
  XMLDtd *xdtd;
  XMLContent *xcontent;
  XMLValue *xvalue;
  XMLAttr *xattr;
}

%token <xvalue> VALUE TEXT NAME CDATASTART CDATA CDATAEND FIRSTTAGEND ENDTAG PROLOGPREFIX PROLOG PROLOGSUFFIX DTDSTART DTD
%start input

%%
input
  :
  | element
  | prolog element
  | dtd element
  | prolog dtd element
  ;

prolog
  :
  | PROLOGPREFIX PROLOG PROLOGSUFFIX
  ;

dtd
  :
  | DTDSTART DTD '>'
  ;

element
  : '<' NAME attr FIRSTTAGEND
  | '<' NAME attr '>' content ENDTAG NAME '>'
  ;

content
  :
  | value
  | content value
  ;

attr
  : NAME '=' VALUE
  | attr NAME '=' VALUE
  |
  ;

value
  : element
  | CDATASTART CDATA CDATAEND
  | TEXT
  ;

%%

int yyerror(const char* s) {
  printf("Error: %s\n", s);
  return 0;
}

static double timediff(struct timeval *s, struct timeval *e) {
  double t1 = (e->tv_sec - s->tv_sec) * 1000.0;
  double t2 = (e->tv_usec - s->tv_usec) / 1000.0;
  return t1 + t2; /* ms */
}

int main(int argc, char *const argv[])
{
    extern int yyparse(void);
    extern FILE *yyin;
    // const char *input_file = NULL;
    int input_size = 0;
    const char *orig_argv0 = argv[0];
    // int opt;


    if (argc == 1 ) {
      fprintf(stdout,"Usage: %s [inputfiles]",argv[0]);
      return 1;
    }

    // for (int i = 1; i < argc; i++) {
      // if (!(yyin = fopen(argv[i], "rb"))) {
      //   fprintf(stderr, "File [%s] is not found!\n", argv[i]);
      //   return 1;
      // }
      // int result = 0;
      // struct timeval start, end;
      // gettimeofday(&start, NULL);
      // result = yyparse();
      // gettimeofday(&end, NULL);
      // // printf("%d\n",result );
      // if (result) {
      //   fprintf(stderr, "[%s] Parse Error!!!\n", argv[i]);
      //   break;
      // }
      // fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], timediff(&start, &end));
    // }

    for (int i = 1; i < argc; i++) {
      if (!(yyin = fopen(argv[i], "rb"))) {
        fprintf(stderr, "File [%s] is not found!\n", argv[i]);
        return 1;
      }
      double tsum = 0.0;
      double t[5];
      for (int c = 0; c < 5; c++) {
        int result = 0;
        struct timeval start, end;
        gettimeofday(&start, NULL);
        result = yyparse();
        gettimeofday(&end, NULL);
        // printf("%d\n",result );
        if (result) {
          fprintf(stdout, "%s FAIL %.4f [ms]\n", argv[i], timediff(&start, &end));
          break;
        }
        t[c] = timediff(&start, &end);
        tsum += t[c];
      }
      if (tsum != 0.0) {
        fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], tsum/5);
      }
    }

    // uint64_t start, end;
    // start = timer();
    // if (yyparse()) {
    //     fprintf(stderr, "[%s] Parse Error!!!\n", argv[2]);
    //     exit(1);
    // } else {
    //     // fprintf(stderr, "Match!!\n");
    // }
    // end = timer();
    //
    // printf("[%s] %llu [ms]\n",argv[2],end - start);
    //
    // return 0;
}
