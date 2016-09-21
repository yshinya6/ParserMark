grammar Cello_nonAST;

topLevel: Declaration EOF;

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

fragment NAME: [a-zA-Z_] W*;
fragment W: [a-zA-Z0-9_];

// Declaration
fragment Declaration: FunctionDeclaration
           | VariableDeclaration
           ;

fragment FunctionDeclaration: Type NAME '(' FunctionParamList ')' Block
                   ;
fragment FunctionParamList: (FunctionParam (',' FunctionParam)*)?
                 ;
fragment FunctionParam: Type NAME
             ;

fragment VariableDeclaration: Type VariableList ';'
                   ;
fragment VariableList: InitDecl (',' InitDecl)*
            ;
fragment InitDecl: NAME ('=' Initializer)?
        ;
fragment Initializer: Expression
           ;

// Type
fragment Type: PrimitiveType
    ;
fragment PrimitiveType: 'int'
             | 'string'
             | 'boolean'
             ;

//Block, Statement
fragment Block: '{' (Statement|Declaration)* '}'
     ;
fragment Statement: Block
         | 'if' '(' Expression ')' Block ( 'else' Block )?
         | 'return' Expression? ';'
         | Expression ';'
         ;
fragment Expression                : AssignmentExpression
                                   ;
fragment AssignmentExpression      : UnaryExpression '=' AssignmentExpression
                                   | ConditionalExpression
                                   ;
fragment ConstantExpression        : ConditionalExpression
                                   ;
fragment ConditionalExpression     : EqualityExpression ('||' | '&&') EqualityExpression;
fragment EqualityExpression        : RelationalExpression ('==' | '!=') RelationalExpression;
fragment RelationalExpression      : UnaryExpression ('<' | '>') UnaryExpression ;
fragment UnaryExpression           : PostfixExpression
                                   | '!' UnaryExpression
                                   ;
fragment PostfixExpression         : PrimaryExpression FunctionCall;
fragment FunctionCall              :  '(' ArgumentExpressionList ')' ;
fragment ArgumentExpressionList     : AssignmentExpression ( ',' AssignmentExpression)*
                                   ;

fragment PrimaryExpression: '(' Expression ')'
                 | Literal
                 | NAME
                 ;

fragment ExpressionList: Expression ( ',' Expression)*
              ;

//Literal
fragment Literal: IntegerLiteral
       | StringLiteral
       | BooleanLiteral
       | NullLiteral
       ;

fragment IntegerLiteral: '0'
              | NONZERODIGIT DIGIT*
              ;
fragment DIGIT: [0-9];
fragment NONZERODIGIT: [1-9];

fragment StringLiteral: '"' STRING_CONTENT* '"';
fragment STRING_CONTENT: ~["\n\\];

fragment BooleanLiteral: 'true' | 'false';
fragment NullLiteral: 'null';
