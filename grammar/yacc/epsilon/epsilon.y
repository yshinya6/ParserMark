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
%token VAR_LEN_PARAM EQ NE AND OR INC DEC GTEQ LTEQ
%token IF FOR ELSE RETURN FALSE TRUE IMPORT
%token INT STRING NAME EOF_SYMBOL NULL_LITERAL
%token ELSE1 ELSE2 ELSE3 ELSE4 ELSE5 ELSE6 ELSE7 ELSE8 ELSE9 ELSEA ELSEB ELSEC  LAMBDA FUNCTION VAR
%token AADD ASUB AMUL ADIV AMOD ALEFTSHIFT ARIGHTSHIFT ALOGICALRIGHTSHIFT ABITAND ABITXOR ABITOR

%%

Program
  : TopLevel
  | Program TopLevel
  ;

TopLevel
  : Declaration
  ;

Declaration
  : FunctionDeclaration
  | VariableDeclaration
  ;

FunctionDeclaration
  : FUNCTION NAME '(' FunctionParamList ')' Block
  | FUNCTION NAME '(' ')' Block
  ;

FunctionParamList
  : FunctionParam
  | FunctionParamList ',' FunctionParam
  | ',' VAR_LEN_PARAM
  ;

FunctionParam
  : NAME
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
  | If1Statement
  | If2Statement
  | If3Statement
  | If4Statement
  | If5Statement
  | If6Statement
  | If7Statement
  | If8Statement
  | If9Statement
  | IfAStatement
  | IfBStatement
  | IfCStatement
  | ReturnStatement
  | ExpressionStatement
  ;

IfStatement
  : IF '(' Expression ')' Block
  | IF '(' Expression ')' Block ELSE Block
  ;

If1Statement
  : IF '(' Expression ')' Block ELSE1 Block
  ;
If2Statement
  : IF '(' Expression ')' Block ELSE2 Block
  ;
If3Statement
  : IF '(' Expression ')' Block ELSE3 Block
  ;
If4Statement
  : IF '(' Expression ')' Block ELSE4 Block
  ;
If5Statement
  : IF '(' Expression ')' Block ELSE5 Block
  ;
If6Statement
  : IF '(' Expression ')' Block ELSE6 Block
  ;
If7Statement
  : IF '(' Expression ')' Block ELSE7 Block
  ;
If8Statement
  : IF '(' Expression ')' Block ELSE8 Block
  ;
If9Statement
  : IF '(' Expression ')' Block ELSE9 Block
  ;
IfAStatement
  : IF '(' Expression ')' Block ELSEA Block
  ;
IfBStatement
  : IF '(' Expression ')' Block ELSEB Block
  ;
IfCStatement
  : IF '(' Expression ')' Block ELSEC Block
  ;

ReturnStatement
  : RETURN ';'
  | RETURN Expression ';'
  ;

ExpressionStatement
  : Expression ';'

VariableDeclaration
  : VAR VariableList ';'
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

Expression
  : AssignmentExpression
  | Expression  ',' AssignmentExpression
  ;

AssignmentExpression
  : UnaryExpression AssignmentOperator AssignmentExpression //FIXME
  | ConditionalExpression
  ;

AssignmentOperator
  : '='
  | AMUL
  | ADIV
  | AMOD
  | AADD
  | ASUB
  | ALEFTSHIFT
  | ARIGHTSHIFT
  | ALOGICALRIGHTSHIFT
  | ABITAND
  | ABITXOR
  | ABITOR
  ;

ConditionalExpression
  : LogicalOrExpression
  | ConditionalExpression '?' Expression ':' LogicalOrExpression
  ;

LogicalOrExpression
  : LogicalANDExpression
  | LogicalOrExpression OR LogicalANDExpression
  ;

LogicalANDExpression
  : EqualityExpression
  | LogicalANDExpression AND EqualityExpression
  ;

EqualityExpression
  : RelationalExpression
  | EqualityExpression EQ RelationalExpression
  | EqualityExpression NE RelationalExpression
  ;

RelationalExpression
  : UnaryExpression
  | RelationalExpression '<' UnaryExpression
  | RelationalExpression '>' UnaryExpression
  | RelationalExpression LTEQ UnaryExpression
  | RelationalExpression GTEQ UnaryExpression
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
  | FunctionExpression
  ;

FunctionExpression
  : FUNCTION '(' FunctionParamList ')' Block LAMBDA
  | FUNCTION '(' ')' Block LAMBDA
  | FUNCTION NAME '(' FunctionParamList ')' Block LAMBDA
  | FUNCTION NAME '('  ')' Block LAMBDA
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
      int i;
      for (i = 1; i < argc; i++) {
        double fastest = 0.0;
        int j;
        for (int j = 0; j < 5; j++) {
          if (!(yyin = fopen(argv[i], "rb"))) {
            fprintf(stderr, "File [%s] is not found!\n", argv[i]);
            return 1;
          }
          int result = 0;
          struct timeval start, end;
          gettimeofday(&start, NULL);
          result = yyparse();
          gettimeofday(&end, NULL);
          if (result) {
            fprintf(stderr, "[%s] Parse Error!!!\n", argv[i]);
            break;
          }
          double execTime = timediff(&start, &end);
          if (fastest == 0.0 || (execTime < fastest) ) {
            fastest = execTime;
          }
          fclose(yyin);
        }
        fprintf(stdout, "%s OK %.4f [ms]\n", argv[i], fastest);
      }
  }
