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

NAME: [a-zA-Z_] W*;
fragment W: [a-zA-Z0-9_];

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
             ;

//Block, Statement
block: '{' (statement|declaration)* '}'
     ;
statement: block
         | 'if' '(' expression ')' block ( 'else' block )?
         | 'return' expression? ';'
         | 'for' '(' expression? ';' expression? ';' expression? ')' block
         | 'for' '(' variableDeclaration expression? ';' expression? ')' block
         | expression ';'
         | ';'
         ;
//Expression
expression: primaryExpression
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
          ;
primaryExpression: '(' expression ')'
                 | literal
                 | NAME
                 ;

expressionList: expression ( ',' expression)*
              ;

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
