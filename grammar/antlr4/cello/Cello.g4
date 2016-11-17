grammar Cello;

topLevel: importDeclaration* declaration* EOF;

/*L: (WhiteSpace
      | BlockComment
      | LineComment
      )*
      ;*/

WhiteSpace: [ \n\r\t]+
              -> skip
          ;

LineComment: '//' ~[\r\n]*
              -> skip
           ;

BlockComment: '/*' .*? '*/'
              -> skip
            ;

NAME: [a-zA-Z$_] W*;
fragment W: [a-zA-Z0-9$_];

// Declaration
importDeclaration: 'import' NAME ('.' NAME)*
                 ;

declaration: methodDeclaration
           | variableDeclaration
           | dummy1
           | dummy2
           ;

methodDeclaration: type NAME '(' methodParamList ')' block
                   ;
methodParamList: (methodParam (',' methodParam)*)?
                 ;
methodParam: type NAME
             ;

variableDeclaration: type variableList ';'
                   ;
variableList: initDecl (',' initDecl)*
            ;
initDecl: NAME ('=' initializer)?
        ;
initializer: expression
           ;
dummy1: 'dummy1' variableList ';'
      ;
dummy2: 'dummy2' variableList ';'
      ;

// Type
type: primitiveType
    | referenceType
    ;
referenceType: NAME
             ;
primitiveType: 'int'
             | 'boolean'
             | 'long'
             ;

//Block, Statement
block: '{' (statement|declaration)* '}'
     ;
statement: block
         | 'if' '(' expression ')' block ( 'else' block )?
         | 'if1' '(' expression ')' block ( 'else' block )?
         | 'if2' '(' expression ')' block ( 'else' block )?
         | 'if3' '(' expression ')' block ( 'else' block )?
         | 'if4' '(' expression ')' block ( 'else' block )?
         | 'if5' '(' expression ')' block ( 'else' block )?
         | 'if6' '(' expression ')' block ( 'else' block )?
         | 'if7' '(' expression ')' block ( 'else' block )?
         | 'if8' '(' expression ')' block ( 'else' block )?
         | 'if9' '(' expression ')' block ( 'else' block )?
         | 'ifA' '(' expression ')' block ( 'else' block )?
         | 'ifB' '(' expression ')' block ( 'else' block )?
         | 'ifC' '(' expression ')' block ( 'else' block )?
         | 'ifD' '(' expression ')' block ( 'else' block )?
         | 'ifE' '(' expression ')' block ( 'else' block )?
         | 'return' expression? ';'
         | expression ';'
         | ';'
         ;

//Expression
expression: assignmentExpression;
assignmentExpression: <assoc=right> unaryExpression
                  ( '='
                  | '*='
                  | '/='
                  | '%='
                  | '+='
                  | '-='
                  | '<<='
                  | '>>='
                  | '>>>='
                  | '&='
                  | '^='
                  | '|='
                  ) expression
                  | conditionalExpression
                  ;
conditionalExpression: equalityExpression (('||' | '&&') equalityExpression)*
                 ;
equalityExpression: relationalExpression (('=='|'!=') relationalExpression)*
                  ;
relationalExpression: unaryExpression (('<'|'>'|'<='|'>=') unaryExpression)*
                    ;
unaryExpression: postfixExpression
               | '!' unaryExpression
               ;
postfixExpression: methodInvocation
                 | primaryExpression
                 ;

primaryExpression: '(' expression ')'
                | literal
                | NAME
                | functionExpression
                ;

methodInvocation: primaryExpression '(' expressionList? ')'
                ;

functionExpression: type NAME? '(' methodParamList ')' block '::'
                  ;

expressionList: expression ( ',' expression)*
             ;

//Literal
literal: IntegerLiteral
       | StringLiteral
       | BooleanLiteral
       | NullLiteral
       ;

IntegerLiteral: '0'
              | NONZERODIGIT DIGIT*
              ;
fragment DIGIT: [0-9];
fragment NONZERODIGIT: [1-9];

StringLiteral: '"' STRING_CONTENT* '"';
fragment STRING_CONTENT: ~["\n\\];

BooleanLiteral: 'true' | 'false';
NullLiteral: 'null';
