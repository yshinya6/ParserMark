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
%token STRING_TYPE INT_TYPE BOOLEAN_TYPE
%token IF FOR ELSE RETURN FALSE TRUE
%token INT STRING NAME EOF_SYMBOL

%%

Program
  : TopLevel
  | Program TopLevel
  ;

TopLevel
  : Declaration
  | ';'
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
  | ForStatement
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

ForStatement
  : FOR '(' VariableDeclaration ForInnerExpr ';' ForInnerExpr ')' Block
  | FOR '(' AssignmentExpression ';' ForInnerExpr ';' ForInnerExpr ')' Block
  ;

ForInnerExpr
  : Expression
  |
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
  : AdditiveExpression
  | RelationalExpression '<' AdditiveExpression
  | RelationalExpression '>' AdditiveExpression
  ;

AdditiveExpression
  : MultiplicativeExpression
  | AdditiveExpression '+' MultiplicativeExpression
  | AdditiveExpression '-' MultiplicativeExpression
  ;

MultiplicativeExpression
  : UnaryExpression
  | MultiplicativeExpression '*' UnaryExpression
  | MultiplicativeExpression '/' UnaryExpression
  ;

UnaryExpression
  : PostfixExpression
  | INC UnaryExpression
  | DEC UnaryExpression
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

Literal
  : INT
  | TRUE
  | FALSE
  | STRING
  | NAME
  ;

  %%
  int
  yyerror(char const *str)
  {
      extern char *yytext;
      fprintf(stderr, "parser error near %s\n", yytext);
      return 0;
  }

  int main(int argc, char *const argv[])
  {
      extern int yyparse(void);
      extern FILE *yyin;
      const char *input_file = NULL;
      int input_size = 0;
      const char *orig_argv0 = argv[0];
      int opt;

      while ((opt = getopt(argc, argv, "p:i:t:o:c:h:")) != -1) {
        switch (opt) {
          case 'i':
            input_file = optarg;
        		if (!(yyin = fopen(input_file, "r"))) {
        			fprintf(stderr, "File [%s] is not found!\n", argv[1]);
        			return 1;
        		}
            break;
          case 'h':
            printf("help");
            return 0;
          default: /* '?' */
            yyin = stdin;
            break;
        }
      }

      uint64_t start, end;
      start = timer();
      if (yyparse()) {
          fprintf(stderr, "Error ! Error ! Error !\n");
          exit(1);
      } else {
          // fprintf(stderr, "Match!!\n");
      }
      end = timer();

      printf("ElapsedTime: %llu [ms]\n", end - start);

      return 0;
  }
