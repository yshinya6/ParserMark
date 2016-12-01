grammar Epsilon;
options {backtrack=true;}


topLevel: declaration;

WhiteSpace
    :   (
             ' '
        |    '\r'
        |    '\t'
        |    '\n'
        )
            {
                skip();
            }
    ;
BlockComment:   '/*' (options {greedy=false;} : . )* '*/'
            {
                skip();
            }
    ;

LineComment
    :   '//' ~('\n'|'\r')*  ('\r\n' | '\r' | '\n')
            {
                skip();
            }
    |   '//' ~('\n'|'\r')*
            {
                skip();
            }
    ;

NAME:('a'..'z'|'A'..'Z'|'_') W*;
fragment W: ('a'..'z'|'A'..'Z'|'0'..'9'|'_');

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
expression: assignmentExpression (',' assignmentExpression)*;
assignmentExpression: unaryExpression '=' assignmentExpression
                    | conditionalExpression
                    ;
conditionalExpression: equalityExpression (('||' | '&&') equalityExpression)*;
equalityExpression: relationalExpression (('=='|'!=') relationalExpression)*;
relationalExpression: additiveExpression (('<'|'>') additiveExpression)*;
additiveExpression: multiplicativeExpression (('+'|'-') multiplicativeExpression)*;
multiplicativeExpression: unaryExpression (('*'|'/') unaryExpression)*;
unaryExpression: postfixExpression
               | '!' unaryExpression
               ;
postfixExpression: primaryExpression (functionCall)*;
functionCall: '(' expressionList? ')';
primaryExpression: '(' expression ')'
                 | literal
                 | NAME
                 | functionExpression
                 ;

functionExpression: 'function' NAME? '(' functionParamList ')' block '::'
                 ;


expressionList: expression ( ',' expression)*
              ;

//Literal
literal: IntegerLiteral
       | StringLiteral
       | 'true'
       | 'false'
       ;

IntegerLiteral: '0'
              | NONZERODIGIT DIGIT*
              ;
fragment DIGIT: '0'..'9';
fragment NONZERODIGIT: '1'..'9';

StringLiteral: '"' STRING_CONTENT* '"';
fragment STRING_CONTENT: ~( '\\' | '"' | '\r' | '\n' );
