grammar Cello;

topLevel: declaration EOF;

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
declaration: functionDeclaration
           | variableDeclaration
           ;

functionDeclaration: type NAME '(' functionParamList ')' block
                   ;
functionParamList: (functionParam (',' functionParam)*)?
                 ;
functionParam: type NAME
             ;

variableDeclaration: type variableList ';'
                   ;
variableList: initDecl (',' initDecl)*
            ;
initDecl: NAME ('=' initializer)?
        ;
initializer: expression
           ;

// Type
type: primitiveType
    ;
primitiveType: 'int'
             | 'string'
             | 'boolean'
             | 'long'
             ;

//Block, Statement
block: '{' (statement|declaration)* '}'
     ;
statement: block
         | 'if' '(' expression ')' block ( 'else' block )?
         | 'return' expression? ';'
         | expression ';'
         | ';'
         ;
//Expression
expression: assignmentExpression;
assignmentExpression: <assoc=right> unaryExpression '=' expression
                    | conditionalExpression
                    ;
conditionalExpression: equalityExpression (('||' | '&&') equalityExpression)*
                     ;
equalityExpression: relationalExpression (('=='|'!=') relationalExpression)*
                  ;
relationalExpression: unaryExpression (('<'|'>') unaryExpression)*
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
                ;

methodInvocation: primaryExpression '(' expressionList? ')'
                ;

expressionList: expression ( ',' expression)*
             ;
/*expression: primaryExpression
          | expression '(' expressionList? ')'
          | expression ('++'|'--')
          | ('++'|'--') expression
          | '!' expression
          | expression ('*'|'/') expression
          | expression ('+'|'-') expression
          | expression ('<'|'>') expression
          | expression ('=='|'!=') expression
          | expression '&&' expression
          | expression '||' expression
          | <assoc=right> expression '=' expression
          ;*/

//Literal
literal: IntegerLiteral
       | StringLiteral
       | BooleanLiteral
       ;

IntegerLiteral: '0'
              | NONZERODIGIT DIGIT*
              ;
fragment DIGIT: [0-9];
fragment NONZERODIGIT: [1-9];

StringLiteral: '"' STRING_CONTENT* '"';
fragment STRING_CONTENT: ~["\n\\];

BooleanLiteral: 'true' | 'false';
