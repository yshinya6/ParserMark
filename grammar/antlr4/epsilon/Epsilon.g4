grammar Epsilon;

topLevel: declaration* EOF;

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

functionDeclaration: 'function' NAME '(' functionParamList ')' block
                   ;
functionParamList: (functionParam (',' functionParam)*)?
                 ;
functionParam: NAME
             ;

variableDeclaration: 'var' variableList ';'
                   ;
variableList: initDecl (',' initDecl)*
            ;
initDecl: NAME ('=' initializer)?
        ;
initializer: expression
           ;

//Block, Statement
block: '{' (statement|declaration)* '}'
     ;
statement: block
         | 'if' '(' expression ')' block ( 'else1' block )
         | 'if' '(' expression ')' block ( 'else2' block )
         | 'if' '(' expression ')' block ( 'else3' block )
         | 'if' '(' expression ')' block ( 'else4' block )
         | 'if' '(' expression ')' block ( 'else5' block )
         | 'if' '(' expression ')' block ( 'else6' block )
         | 'if' '(' expression ')' block ( 'else7' block )
         | 'if' '(' expression ')' block ( 'else8' block )
         | 'if' '(' expression ')' block ( 'else9' block )
         | 'if' '(' expression ')' block ( 'elseA' block )
         | 'if' '(' expression ')' block ( 'elseB' block )
         | 'if' '(' expression ')' block ( 'elseC' block )
         | 'if' '(' expression ')' block ( 'else' block )?
         | 'return' expression? ';'
         | expression ';'
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
postfixExpression: functionCall
                 | primaryExpression
                 ;

primaryExpression: '(' expression ')'
                | literal
                | NAME
                | functionExpression
                ;

functionCall: primaryExpression '(' expressionList? ')'
                ;

functionExpression: 'function' NAME? '(' functionParamList ')' block '::'
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
