grammar Cello;
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
               | '++' unaryExpression
               | '--' unaryExpression
               | '!' unaryExpression
               ;
postfixExpression: primaryExpression (functionCall)*;
functionCall: '(' expressionList? ')';
primaryExpression: '(' expression ')'
                 | literal
                 | NAME
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
