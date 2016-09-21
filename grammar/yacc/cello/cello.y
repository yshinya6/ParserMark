%{
#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <sys/time.h> // gettimeofday
#define YYDEBUG 1

int yyerror(char const *str);

int yylex(void);

uint64_t timer() {
struct timeval tv;
gettimeofday(&tv, NULL);
return tv.tv_sec * 1000 + tv.tv_usec / 1000;
}

%}

%start Program
%token VAR_LEN_PARAM EQ NE AND OR INC DEC
%token STRING_TYPE INT_TYPE BOOLEAN_TYPE LONG_TYPE
%token IF FOR ELSE RETURN FALSE TRUE IMPORT
%token INT STRING NAME EOF_SYMBOL NULL_LITERAL

%%

Program
  : TopLevel
  | Program TopLevel
  ;

TopLevel
  : ImportDeclarations Declaration
  | Declaration
  | ';'
  ;

ImportDeclarations
  : ImportDeclaration
  | ImportDeclarations ImportDeclaration
  ;
ImportDeclaration
  : IMPORT PackageName ';'
  ;
PackageName
  : QualifiedName '.' '*'
  | QualifiedName
  ;

Declaration
  : FunctionDeclaration
  | VariableDeclaration
  ;

FunctionDeclaration
  : Type NAME '(' FunctionParamList ')' Block
  | Type NAME '(' FunctionParamList ')' ';'
  | Type NAME '(' ')' Block
  | Type NAME '(' ')' ';'
  | Type Block
  ;

FunctionParamList
  : FunctionParam
  | FunctionParamList ',' FunctionParam
  | ',' VAR_LEN_PARAM
  ;

FunctionParam
  : Type
  | Type NAME
  ;

Block
  : '{' BlockInner '}'
  | '{' '}'
  ;

BlockInner
  : Statement
  | Declaration
  | BlockInner Statement
  | BlockInner Declaration
  ;

Statement
  : Block
  | IfStatement
  | ReturnStatement
  | ExpressionStatement
  | ';'
  ;

IfStatement
  : IF '(' Expression ')' Block
  | IF '(' Expression ')' Block ELSE Block
  ;

ReturnStatement
  : RETURN ';'
  | RETURN Expression ';'
  ;

ExpressionStatement
  : Expression ';'

VariableDeclaration
  : Type VariableList ';'
  ;

VariableList
  : InitDecl
  | VariableList ',' InitDecl
  ;

InitDecl
  : NAME
  | NAME '=' Initializer
  ;

Initializer
  : AssignmentExpression
  ;

// Type
Type
  : INT_TYPE
  | BOOLEAN_TYPE
  | STRING_TYPE
  | LONG_TYPE
  ;

Expression
  : AssignmentExpression
  | Expression  ',' AssignmentExpression
  ;

AssignmentExpression
  : UnaryExpression '=' AssignmentExpression //FIXME
  | ConditionalExpression
  ;

ConditionalExpression
  : LogicalANDExpression
  | ConditionalExpression OR LogicalANDExpression
  ;

LogicalANDExpression
  : EqualityExpression
  | LogicalANDExpression AND EqualityExpression
  ;

EqualityExpression
  : RelationalExpression
  | EqualityExpression  EQ RelationalExpression
  | EqualityExpression  NE RelationalExpression
  ;

RelationalExpression
  : UnaryExpression
  | RelationalExpression '<' UnaryExpression
  | RelationalExpression '>' UnaryExpression
  ;

UnaryExpression
  : PostfixExpression
  | '!' UnaryExpression
  ;

PostfixExpression
  : PrimaryExpression
  | PrimaryExpression FunctionCall
  ;

FunctionCall
  : '(' ArgumentExpressionList ')'
  | '(' ')'
  ;

ArgumentExpressionList
  : AssignmentExpression
  | ArgumentExpressionList ',' AssignmentExpression
  ;

PrimaryExpression
  : Literal
  | '(' Expression ')'
  ;

QualifiedName
  : NAME
  | QualifiedName '.' NAME
  ;

Literal
  : INT
  | TRUE
  | FALSE
  | STRING
  | NULL_LITERAL
  | NAME
  ;

  %%
  int
  yyerror(char const *str)
  {
      extern char *yytext;
      extern int yylineno;
      fprintf(stderr, "line:%d parse error near %s\n",yylineno, yytext);
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

      for (int i = 1; i < argc; i++) {
        if (!(yyin = fopen(argv[i], "rb"))) {
    			fprintf(stderr, "File [%s] is not found!\n", argv[i]);
    			return 1;
        }
        int result = 0;
        struct timeval start, end;
        gettimeofday(&start, NULL);
        result = yyparse();
        gettimeofday(&end, NULL);
        // printf("%d\n",result );
        if (result) {
          fprintf(stderr, "[%s] Parse Error!!!\n", argv[i]);
          break;
        }
        fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], timediff(&start, &end));
      }

      // for (int i = 1; i < argc; i++) {
      //   if (!(yyin = fopen(argv[i], "rb"))) {
      //     fprintf(stderr, "File [%s] is not found!\n", argv[i]);
      //     return 1;
      //   }
      //   double tsum = 0.0;
      //   double t[5];
      //   for (int c = 0; c < 5; c++) {
      //     int result = 0;
      //     struct timeval start, end;
      //     gettimeofday(&start, NULL);
      //     result = yyparse();
      //     gettimeofday(&end, NULL);
      //     // printf("%d\n",result );
      //     if (result) {
      //       fprintf(stdout, "%s FAIL %.4f [ms]\n", argv[i], timediff(&start, &end));
      //       break;
      //     }
      //     t[c] = timediff(&start, &end);
      //     tsum += t[c];
      //   }
      //   if (tsum != 0.0) {
      //     fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], tsum/5);
      //   }
      // }


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
