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

%start StatementTest

%token VAR_LEN_PARAM EQ NE AND OR INC DEC MR
%token AADD ASUB AMUL ADIV AMOD ALEFTSHIFT ARIGHTSHIFT ALOGICALRIGHTSHIFT ABITAND ABITXOR ABITOR
%token RARROW LEFTSHIFT RIGHTSHIFT LOGICALRIGHTSHIFT LTEQ GTEQ

%token STRING_TYPE INT_TYPE BOOLEAN_TYPE DOUBLE_TYPE FLOAT_TYPE SHORT_TYPE
%token CHAR_TYPE BYTE_TYPE LONG_TYPE VOID_TYPE

%token IF FOR ELSE RETURN FALSE TRUE
%token ABSTRACT ASSERT BREAK CASE CATCH CLASS CONST CONTINUE DEFAULT DO ENUM
%token EXTENDS FINAL FINALLY GOTO IMPLEMENTS IMPORT INSTANCEOF INTERFACE NATIVE
%token NEW NULL_LITERAL PACKAGE PRIVATE PROTECTED PUBLIC STATIC STRICTFP SUPER SWITCH
%token SYNCHRONIZED THIS THROW THROWS TRANSIENT TRY VOLATILE WHILE
%token INT STRING IDENTIFIER EOF_SYMBOL LONG FLOAT DOUBLE CHAR

%%
/*
Program
  : TopLevel
  | Program TopLevel
  ;

TopLevel
  : PackageDeclaration ImportDeclaration TypeDeclarations
  | ImportDeclarations TypeDeclarations
  | TypeDeclarations
  | ';'
  ;
*/
/* Annotation */
/*
OptAnnotations
  :
  | Annotations
  ;
Annotations
  : Annotation
  | Annotations Annotation
  ;
Annotation
  : '@' QualifiedName '(' ElementValuePairList ')'
  | '@' QualifiedName '(' ElementValue ')'
  | '@' QualifiedName
  ;
ElementValuePairList
  : ElementValuePair
  | ElementValuePair ',' ElementValuePair
  ;
ElementValuePair
  : IDENTIFIER '=' ElementValue
  ;
ElementValue
  : ElementValueArrayInitializer
  | ConditionalExpression
  | Annotation
  ;
ElementValueList
  : ElementValue
  | ElementValue ',' ElementValue
ElementValueArrayInitializer
  : '{' ElementValueList ',' '}'
  | '{' ElementValueList  '}'
  ;
*/
/* Declaration */
// package
/*
PackageDeclaration
  : Annotations PACKAGE QualifiedName ';'
  | PACKAGE QualifiedName ';'
  ;
// import
ImportDeclarations
  :
  | ImportDeclaration
  | ImportDeclarations ImportDeclaration
  ;
ImportDeclaration
  : IMPORT STATIC PackageName ';'
  | IMPORT PackageName ';'
  ;
PackageName
  : QualifiedName '.' '*'
  | QualifiedName
  ;

// type
TypeDeclarations
  :
  | TypeDeclaration
  | TypeDeclarations TypeDeclaration
  ;
TypeDeclaration
  : ClassDeclaration
  | InterfaceDeclaration
  | ';'
  ;

ClassDeclaration
  : OptClassModifiers CLASS IDENTIFIER OptTypeParameters OptSuperClass OptSuperInterfaces ClassBody
  | OptClassModifiers ENUM IDENTIFIER OptSuperInterfaces EnumBody
  ;

OptClassModifiers
  :
  | ClassModifiers
  ;

ClassModifiers
  : ClassModifier
  | ClassModifiers ClassModifier
  ;
ClassModifier
  : Annotation
  | PUBLIC
  | PROTECTED
  | PRIVATE
  | ABSTRACT
  | FINAL
  | STATIC
  | STRICTFP
  ;
OptSuperClass
  :
  | SuperClass
  ;
SuperClass
  : EXTENDS ClassOrInterfaceType
  ;
OptSuperInterfaces
  :
  | SuperInterfaces
  ;
SuperInterfaces
  : IMPLEMENTS ClassOrInterfaceTypes
  ;
OptClassBody
  :
  | ClassBody
  ;
ClassBody
  : '{' OptClassBodyDeclarations '}'
  ;
OptClassBodyDeclarations
  :
  | ClassBodyDeclarations
  ;
ClassBodyDeclarations
  : ClassBodyDeclaration
  | ClassBodyDeclarations ClassBodyDeclaration
  ;
ClassBodyDeclaration
  : ClassMemberDeclaration
  | Block
  | STATIC Block
  | ConstructorDeclaration
  ;
ClassMemberDeclaration
  : FieldDeclaration
  | MethodDeclaration
  | ClassDeclaration
  | InterfaceDeclaration
  | ';'
  ;
EnumBody
  : '{' AddEnumeratorList ',' ';' ClassBodyDeclarations '}'
  | '{' AddEnumeratorList  ';' ClassBodyDeclarations '}'
  | '{' AddEnumeratorList ',' ';' '}'
  | '{' AddEnumeratorList ';' '}'
  | '{' AddEnumeratorList '}'
  ;
AddEnumeratorList
  :
  | Enumerator
  | Enumerator ',' Enumerator
  ;
Enumerator
  : OptAnnotations IDENTIFIER ArgumentExpressionList OptClassBody
  | OptAnnotations IDENTIFIER ClassBody
  | Annotations IDENTIFIER
  ;

// interface, annotation
InterfaceDeclaration
  : OptInterfaceModifiers INTERFACE IDENTIFIER OptTypeParameters ExtendsInterfaces InterfaceBody
  | OptInterfaceModifiers '@' INTERFACE IDENTIFIER AnnotationTypeBody
  ;
OptInterfaceModifiers
  :
  | InterfaceModifiers
  ;
InterfaceModifiers
  : InterfaceModifier
  | InterfaceModifiers InterfaceModifier
  ;
InterfaceModifier
  : Annotation
  | PUBLIC
  | PROTECTED
  | PRIVATE
  | ABSTRACT
  | STATIC
  | STRICTFP
  ;
ExtendsInterfaces
  :
  | EXTENDS ClassOrInterfaceTypes
  ;
InterfaceBody
  : '{' InterfaceMemberDeclarations '}'
  ;
InterfaceMemberDeclarations
  :
  | InterfaceMemberDeclaration
  | InterfaceMemberDeclarations InterfaceMemberDeclaration
  ;
InterfaceMemberDeclaration
  : ConstantDeclaration
  | InterfaceMethodDeclaration
  | ClassDeclaration
  | InterfaceDeclaration
  | ';'
  ;
AnnotationTypeBody
  : '{' AnnotationTypeMemberDeclarations '}'
  ;
AnnotationTypeMemberDeclarations
  :
  | AnnotationTypeMemberDeclaration
  | AnnotationTypeMemberDeclarations AnnotationTypeMemberDeclaration
  ;
AnnotationTypeMemberDeclaration
  : AnnotationTypeElementDeclaration
  | ConstantDeclaration
  | ClassDeclaration
  | InterfaceDeclaration
  | ';'
  ;
AnnotationTypeElementDeclaration
  : AnnotationTypeElementModifiers Type IDENTIFIER '(' ')' AnnotationTypeElementDefaultValue ';'
  ;
AnnotationTypeElementModifiers
  :
  | AnnotationTypeElementModifier
  | AnnotationTypeElementModifiers AnnotationTypeElementModifier
  ;
AnnotationTypeElementModifier
  : Annotation
  | PUBLIC
  | ABSTRACT
  ;
AnnotationTypeElementDefaultValue
  :
  | DEFAULT ElementValue
  ;

// local variable
VariableDeclaration
  : OptVariableModifiers Type AddVariableDeclarations
  ;
OptVariableModifiers
  :
  | VariableModifiers
  ;
VariableModifiers
  : VariableModifier
  | VariableModifiers VariableModifier
  ;
VariableModifier
  : Annotation
  | FINAL
  ;
AddVariableDeclarations
  : VarName
  | VarName '=' Initializer
  | InitDeclList
  ;
InitDecl
  : VarName
  | VarName '=' Initializer
  ;
InitDeclList
  : InitDecl
  | InitDecl ',' InitDecl
  ;
VarName
  : IDENTIFIER ArrayModifiers
  ;
ArrayModifiers
  :
  | ArrayModifier
  | ArrayModifiers ArrayModifier
  ;
ArrayModifier
  : '[' OptConstantExpression ']'
  ;
Initializer
  : Expression
  | ArrayInitializer
  ;
InitializerList
  :
  | Initializer
  | Initializer ',' Initializer
ArrayInitializer
  : '{' InitializerList '}'
  | '{' InitializerList ',' '}'
  ;

// field
FieldDeclaration
  : FieldModifiers Type InitDeclList ';'
  | Type InitDeclList ';'
  ;
FieldModifiers
  : FieldModifier
  | FieldModifiers FieldModifier
  ;
FieldModifier
  : Annotation
  | PUBLIC
  | PROTECTED
  | PRIVATE
  | FINAL
  | STATIC
  | TRANSIENT
  | VOLATILE
  ;

// constant
ConstantDeclaration
  : ConstantModifiers Type InitDeclList ';'
  ;
ConstantModifiers
  :
  | ConstantModifier
  | ConstantModifiers ConstantModifier
  ;
ConstantModifier
  : Annotation
  | PUBLIC
  | FINAL
  | STATIC
  ;

// method
MethodDeclaration
  : OptMethodModifiers OptTypeParamAnnotations TypeOrVoid IDENTIFIER '(' MethodParamList ')' OptThrows ';'
  | OptMethodModifiers OptTypeParamAnnotations TypeOrVoid IDENTIFIER '(' MethodParamList ')' OptThrows Block
  ;
OptTypeParamAnnotations
  :
  | TypeParamAnnotations
  ;
TypeParamAnnotations
  : TypeParameters
  | TypeParameters Annotation
  ;
OptMethodModifiers
  :
  | MethodModifiers
  ;
MethodModifiers
  : MethodModifier
  | MethodModifiers MethodModifier
  ;
MethodModifier
  : Annotation
  | PUBLIC
  | PROTECTED
  | PRIVATE
  | ABSTRACT
  | FINAL
  | STATIC
  | SYNCHRONIZED
  | NATIVE
  | STRICTFP
  ;
InterfaceMethodDeclaration
  : OptInterfaceMethodModifiers OptTypeParamAnnotations TypeOrVoid IDENTIFIER '(' MethodParamList ')' OptThrows Block
  | OptInterfaceMethodModifiers OptTypeParamAnnotations TypeOrVoid IDENTIFIER '(' MethodParamList ')' OptThrows ';'
  ;
OptInterfaceMethodModifiers
  :
  | InterfaceMethodModifiers
InterfaceMethodModifiers
  : InterfaceMethodModifier
  | InterfaceMethodModifiers InterfaceMethodModifier
  ;
InterfaceMethodModifier
  : Annotation
  | PUBLIC
  | ABSTRACT
  | DEFAULT
  | STATIC
  | STRICTFP
  ;
MethodParamList
  :
  | MethodParam
  | MethodParamList ',' VarParam
  | MethodParamList ',' MethodParam
  ;
MethodParam
  : OptVariableModifiers Type VarName
  ;
VarParam
  : OptVariableModifiers Type VAR_LEN_PARAM VarName
  ;
OptThrows
  :
  | Throws
  ;
Throws
  : THROWS ClassOrInterfaceTypes
  ;

// constructor
ConstructorDeclaration
  : ConstructorModifiers ReferenceType '(' MethodParamList ')' OptThrows ConstructorBody
  | ReferenceType '(' MethodParamList ')' OptThrows ConstructorBody
  ;
ConstructorModifiers
  : ConstructorModifier
  | ConstructorModifiers ConstructorModifier
  ;
ConstructorModifier
  : Annotation
  | PUBLIC
  | PROTECTED
  | PRIVATE
  ;
ConstructorBody
  : '{' OptExplicitConstructorInvocation OptBlockStatements '}'
  ;
OptExplicitConstructorInvocation
  :
  | ExplicitConstructorInvocation
  ;
ExplicitConstructorInvocation
  : OptTypeArguments THIS '(' OptExpressions ')' ';'
  | OptTypeArguments SUPER '(' OptExpressions ')' ';'
  | PostfixExpression '.' OptTypeArguments SUPER '(' OptExpressions ')' ';'
  ;
*/
/* Types, Values, Variables */
/*
TypeTest
  : Type
  | TypeTest Type
  ;

Type
  : ReferenceType
  | PrimitiveType
  ;
PrimitiveType
  : Annotations UnannoPrimitiveType
  | UnannoPrimitiveType
  ;
UnannoPrimitiveType
  : NumericType
  | BooleanType
  ;
NumericType
  : IntegralType
  | FloatingPointType
  ;
IntegralType
  : BYTE_TYPE
  | CHAR_TYPE
  | SHORT_TYPE
  | INT_TYPE
  | LONG_TYPE
  ;
FloatingPointType
  : FLOAT_TYPE
  | DOUBLE_TYPE
  ;
BooleanType
  : BOOLEAN_TYPE
  ;
ReferenceType
  : ArrayType
  | ClassOrInterfaceType
  // | TypeVariable
  ;
ArrayType
  : PrimitiveType InitArrays
  | ClassOrInterfaceType InitArrays
  // | TypeVariable InitArrays
  ;
InitArrays
  : InitArray
  | InitArrays InitArray
  ;
InitArray
  : Annotations '[' ']'
  | '[' ']'
  ;
ClassOrInterfaceType
  : ClassType
  ;
ClassType
  : SimpleClassType
  | ClassType '.' SimpleClassType
  ;
SimpleClassType
  : Annotations UnannoSimpleClassType
  | UnannoSimpleClassType
  ;
UnannoSimpleClassType
  : IDENTIFIER TypeArguments
  | IDENTIFIER
  ;

// Followings are deprecated because it deal same input consumed by SimpleClassType production.
// TypeVariable
//   : Annotations IDENTIFIER
//   | IDENTIFIER
//   ;

// OptTypeParameters
//   :
//   | TypeParameters
//   ;
// TypeParameters
//   : '<' TypeParameterList '>'
//   ;
// TypeParameterList
//   : TypeParameter
//   | TypeParameterList TypeParameter
//   ;
// TypeParameter
//   : Annotations UnannoTypeParameter
//   | UnannoTypeParameter
//   ;
// UnannoTypeParameter
//   : IDENTIFIER ExtendsTypeModifier
//   ;
// ExtendsTypeModifier
//   :
//   | EXTENDS ClassOrInterfaceType InterfaceTypeList
//   ;
// InterfaceTypeList
//   :
//   | '&' InterfaceType
//   | InterfaceTypeList '&' InterfaceType
//   ;
OptTypeArguments
  :
  | TypeArguments
  ;
TypeArguments
  : '<' TypeArgumentList'>'
  ;
TypeArgumentList
  :
  | TypeArgument
  | TypeArgumentList ',' TypeArgument
  ;
TypeArgument
  : ReferenceType
  | '?'
  | '?' ExtendsOrSuper
  | Annotations '?'
  | Annotations '?' ExtendsOrSuper
  ;
ExtendsOrSuper
  : EXTENDS ReferenceType
  | SUPER ReferenceType
  ;
Void
  : VOID_TYPE
  ;
TypeOrVoid
  : Type
  | Void
  ;
NonArrayType
  : ClassOrInterfaceType
  | PrimitiveType
  ;
ClassOrInterfaceTypes
  : ClassOrInterfaceType
  | ClassOrInterfaceTypes ',' ClassOrInterfaceType
  ;
*/
/* Block, Statement */

StatementTest
  : Statement
  | StatementTest Statement
  ;
Block
  : '{' '}'
  | '{' BlockStatements '}'
  ;
//FIXME deprecated production
OptBlockStatements
  :
  | BlockStatements
  ;
BlockStatements
  : BlockStatement
  | BlockStatements BlockStatement
  ;
BlockStatement
  : Statement
  | VariableDeclaration ';'
  | ClassDeclaration
  ;
Statement
  : Block
  | ASSERT Expression ';'
  | ASSERT Expression ':' Expression ';'
  | IF '(' Expression ')' Statement
  | IF '(' Expression ')' Statement ELSE Statement
  | SwitchStatement
  | WHILE '(' Expression ')' Statement
  | DO Statement WHILE '(' Expression ')' ';'
  | FOR '(' OptExpressions ';' OptExpression ';' OptExpressions ')' Statement
  | FOR '(' VariableDeclaration ';' OptExpression ';' OptExpressions ')' Statement
  | FOR '(' Type IDENTIFIER ':' OptExpression ')' Statement
  // | FOR '(' OptVariableModifiers Type IDENTIFIER ':' OptExpression ')' Statement
  | CONTINUE OptIDENTIFIER ';'
  | BREAK OptIDENTIFIER ';'
  | RETURN OptExpression ';'
  | TryStatement
  | THROW Expression ';'
  | SYNCHRONIZED '(' Expression ')' Block
  | IDENTIFIER ':'
  | Expression ';'
  | ';'
  ;

// try-catch-finally
TryStatement
  : TRY ResourceList Block OptCatches
  | TRY ResourceList Block OptCatches FINALLY Block
  | TRY Block OptCatches FINALLY Block
  | TRY Block Catches
  ;
ResourceList
  : '(' Resources ')'
  | '(' Resources ';' ')'
  ;
Resources
  : Resource
  | Resources ';' Resource
  ;
Resource
  : OptVariableModifiers Type IDENTIFIER '=' Expression
  ;
OptCatches
  :
  | Catches
  ;
Catches
  : Catch
  | Catches Catch
  ;
Catch
  : CATCH '(' AddCatchParameter ')' Block
  ;
AddCatchParameter
  : OptVariableModifiers ClassOrInterfaceType IDENTIFIER
  | OptVariableModifiers AddClassOrInterfaceTypes IDENTIFIER
  ;
AddClassOrInterfaceTypes
  : ClassOrInterfaceType
  | AddClassOrInterfaceTypes '|' ClassOrInterfaceType
  ;

// switch
SwitchStatement
  : SWITCH '(' Expression ')' SwitchBlock
  ;
SwitchBlock
  : '{' SwitchConditions '}'
  ;
SwitchConditions
  : SwitchCondition
  | SwitchConditions SwitchCondition
  ;
SwitchCondition
  : CASE ConstantExpression ':'
  | CASE ConstantExpression ':' CaseBlock
  | DEFAULT ':'
  | DEFAULT ':' CaseBlock
  ;
CaseBlock
  : BlockStatement
  | CaseBlock BlockStatement
  ;

/* Expression */

// ExpressionTest
//  : Expression
//  | ExpressionTest Expression
//  ;
// Expression
//   : LambdaExpression
//   | AssignmentExpression
//   ;
// OptExpression
//   :
//   | Expression
//   ;
// OptExpressions
//   :
//   | Expressions
//   ;
// Expressions
//   : Expression
//   | Expressions ',' Expression
//   ;
// AssignmentExpression
//   : UnaryExpression AddAssignmentOperator Expression
//   | ConditionalExpression
//   ;
// AddAssignmentOperator
//   : '='
//   | AMUL
//   | ADIV
//   | AMOD
//   | AADD
//   | ASUB
//   | ALEFTSHIFT
//   | ARIGHTSHIFT
//   | ALOGICALRIGHTSHIFT
//   | ABITAND
//   | ABITXOR
//   | ABITOR
//   ;
// OptConstantExpression
//   :
//   | ConditionalExpression
//   ;
// ConstantExpression
//   : ConditionalExpression
//   ;
// ConditionalExpression
//   : LogicalOrExpression
//   | ConditionalExpression '?' Expression ':' LogicalOrExpression
//   ;
// LogicalOrExpression
//   : LogicalAndExpression
//   | LogicalOrExpression OR LogicalAndExpression
//   ;
// LogicalAndExpression
//   : InclusiveOrExpression
//   | LogicalAndExpression AND InclusiveOrExpression
//   ;
// InclusiveOrExpression
//   : ExclusiveOrExpression
//   | InclusiveOrExpression '|' ExclusiveOrExpression
//   ;
// ExclusiveOrExpression
//   : AndExpression
//   | ExclusiveOrExpression '^' AndExpression
//   ;
// AndExpression
//   : EqualityExpression
//   | AndExpression '&' EqualityExpression
//   ;
// EqualityExpression
//   : RelationalExpression
//   | EqualityExpression EQ RelationalExpression
//   | EqualityExpression NE RelationalExpression
//   ;
// RelationalExpression
//   : ShiftExpression
//   | RelationalExpression LTEQ ShiftExpression
//   | RelationalExpression GTEQ ShiftExpression
//   | RelationalExpression '<' ShiftExpression
//   | RelationalExpression '>' ShiftExpression
//   | INSTANCEOF ReferenceType
//   ;
// ShiftExpression
//   : AdditiveExpression
//   | ShiftExpression LEFTSHIFT AdditiveExpression
//   | ShiftExpression RIGHTSHIFT AdditiveExpression
//   | ShiftExpression LOGICALRIGHTSHIFT AdditiveExpression
//   ;
// AdditiveExpression
//   : MultiplicativeExpression
//   | AdditiveExpression '+' MultiplicativeExpression
//   | AdditiveExpression '-' MultiplicativeExpression
//   ;
// MultiplicativeExpression
//   : CastNewExpression
//   | MultiplicativeExpression '*' CastNewExpression
//   | MultiplicativeExpression '/' CastNewExpression
//   | MultiplicativeExpression '%' CastNewExpression
//   ;
// CastNewExpression
//   : '(' Type ')' CastNewExpression
//   | UnaryExpression
//   ;
// UnaryExpression
//   : PostfixExpression
//   | INC UnaryExpression
//   | DEC UnaryExpression
//   | '+' CastNewExpression
//   | '-' CastNewExpression
//   | '~' CastNewExpression
//   | '!' CastNewExpression
//   ;
// PostfixExpression
//   : PrimaryExpression
//   | PostfixExpression '.' IDENTIFIER ArgumentExpressionList
//   | PostfixExpression '.' TypeArguments IDENTIFIER ArgumentExpressionList
//   | PostfixExpression '.' NEW /*ClassOrInterfaceType*/ IDENTIFIER ArgumentExpressionList OptClassBody
//   | PostfixExpression '.' NEW OptTypeArguments OptAnnotations /*ClassOrInterfaceType*/IDENTIFIER ArgumentExpressionList OptClassBody
//   | PostfixExpression '[' Expression ']'
//   // | PostfixExpression '.' IDENTIFIER
//   | PostfixExpression ArgumentExpressionList
//   // | PostfixExpression MR OptTypeArguments IDENTIFIER
//   | PostfixExpression INC
//   | PostfixExpression DEC
//   ;
// ArgumentExpressionList
//   : '(' Expressions ')'
//   ;
// PrimaryExpression
//   : Constant
//   | THIS
//   | SUPER
//   | '(' Expression ')'
//   | ClassLiteral
//   | QualifiedName '.' THIS
//   | QualifiedName '.' SUPER
//   | InstanceCreationExpression
//   | ArrayCreationExpression
//   | MethodReference
//   | QualifiedName
//   ;
// ClassLiteral
//   : TypeOrVoid '.' CLASS
//   ;
// InstanceCreationExpression
//   : NEW TypeArguments Annotations /*ClassOrInterfaceType*/ IDENTIFIER ArgumentExpressionList OptClassBody
//   | NEW Annotations /*ClassOrInterfaceType*/ IDENTIFIER ArgumentExpressionList OptClassBody
//   | NEW TypeArguments /*ClassOrInterfaceType*/ IDENTIFIER ArgumentExpressionList OptClassBody
//   | NEW /*ClassOrInterfaceType*/ IDENTIFIER ArgumentExpressionList OptClassBody
//   ;
// ArrayCreationExpression
//   : NEW OptAnnotations ArrayCreationModifiers OptArrayCreationLastModifiers
//   | NEW OptAnnotations ArrayCreationModifiers ArrayInitializer
//   ;
// ArrayCreationModifier
//   : NonArrayType OptAnnotations '[' OptExpression ']'
//   ;
// ArrayCreationModifiers
//   : ArrayCreationModifier
//   | ArrayCreationModifiers ArrayCreationModifier
//   ;
// OptArrayCreationLastModifiers
//   :
//   | ArrayCreationLastModifiers
//   ;
// ArrayCreationLastModifiers
//   : '[' ']'
//   | Annotations '[' ']'
//   | ArrayCreationLastModifiers '[' ']'
//   | ArrayCreationLastModifiers Annotations '[' ']'
//   ;
// MethodReference
//   : ReferenceType MR IDENTIFIEROrNew
//   | ReferenceType MR TypeArguments IDENTIFIEROrNew
//   ;
// IDENTIFIEROrNew
//   : IDENTIFIER
//   | NEW
//   ;
// LambdaExpression
//   : LambdaParameters RARROW LambdaBody
//   ;
// LambdaParameters
//   : IDENTIFIER
//   | '(' ')'
//   | '(' MethodParamList ')'
//   | '(' InferredParamList ')'
//   ;
// //FIXME Followings occurr reduce/reduce conflicts
// InferredParamList
//   : IDENTIFIER
//   | InferredParamList ',' IDENTIFIER
//   ;
// LambdaBody
//   : Expression
//   | Block
//   ;

/* Identifier */
OptIDENTIFIER
  :
  | IDENTIFIER
  ;
QualifiedName
  : IDENTIFIER
  | QualifiedName '.' IDENTIFIER
  ;

// stub
// TypeArguments: '<' IDENTIFIER '>';
// OptTypeArguments:'<' IDENTIFIER '>';
// ArrayInitializer:'{''}';
// TypeOrVoid:INT_TYPE;
// NonArrayType:INT_TYPE;
// ReferenceType:IDENTIFIER;
// OptClassBody:'{' '}';
// Block:'{' '}';
// Annotations:'@'IDENTIFIER;
// OptAnnotations:'@'IDENTIFIER;
// MethodParamList:IDENTIFIER','IDENTIFIER;
Type: INT_TYPE|BOOLEAN_TYPE;
VariableDeclaration: Type IDENTIFIER '=' Constant;
ClassOrInterfaceType: IDENTIFIER;
OptVariableModifiers: /* empty */ ;
ConstantExpression: Constant;
Expression: IDENTIFIER;
OptExpression: Expression ;
OptExpressions: Expression ;
ClassDeclaration: CLASS IDENTIFIER '{''}';

/* Literal, Constant */
LiteralTest
  : Literal
  | LiteralTest Literal
  ;
Literal
  : FLOAT
  | DOUBLE
  | INT
  | TRUE
  | FALSE
  | CHAR
  | STRING
  | NULL_LITERAL
  ;
Constant
  : Literal
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
