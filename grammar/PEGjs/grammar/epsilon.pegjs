File = _ (TopLevel _ )* EOT 
_ = ((S  / BLOCKCOMMENT  / LINECOMMENT ) )* 
S = [\t-\n\r ]
BLOCKCOMMENT = '/''*'(!('*''/').)* '*''/'
LINECOMMENT = '/''/'(!('\n').)* 
EOT = !(.)
Identifier = NAME _ 
NAME = !(DIGIT )!(KEYWORD )(W )+ 
DIGIT = [0-9]
W = [$0-9A-Z_a-z]
TopLevel = ((Statement  / Declaration ) )+ 
KEYWORD = ('e''l''s''e' / 'i''f' / 'r''e''t''u''r''n' / 'f''u''n''c''t''i''o''n' / 'v''a''r') !(W )
Declaration = (FunctionDeclaration  / VariableDeclaration ) 
FunctionDeclaration = 'f''u''n''c''t''i''o''n'!(W )_ Identifier _ '('_ FunctionParamList ')'_ Block 
FunctionParamList = (FunctionParam (','_ FunctionParam )* )?
FunctionParam = Identifier _ 
VariableDeclaration = 'v''a''r'!(W )_ VariableList ';'_ 
VariableList = InitDecl (','_ InitDecl )* 
InitDecl = Identifier ('='!('=')_ Initializer )?_ 
Initializer = AssignmentExpression 
Block = '{'_ ((Statement _  / Declaration _ ) )* '}'_ 
Statement = (Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''1'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''2'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''3'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''4'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''5'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''6'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''7'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''8'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''9'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''A'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''B'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block 'e''l''s''e''C'_ Block  / 'i''f'!(W )_ '('_ Expression ')'_ Block ('e''l''s''e'!(W )_ Block )? / 'r''e''t''u''r''n'!(W )_ (Expression )?';'_  / Expression ';'_ ) 
Expression = AssignmentExpression (','_ AssignmentExpression )* 
AssignmentExpression = (UnaryExpression _AssignmentOperator AssignmentExpression  / ConditionalExpression ) 
_AssignmentOperator = ('='!('=')_  / '*''='_  / '/''='_  / '%''='_  / '+''='_  / '-''='_  / '<''<''='_  / '>''>''='_  / '>''>''>''='_  / '&''='_  / '^''='_  / '|''='_ ) 
ConstantExpression = ConditionalExpression 
ConditionalExpression = LogicalOrExpression ('?'_ Expression ':'!('>')_ LogicalOrExpression )* 
LogicalOrExpression = LogicalAndExpression ('|''|'_ LogicalAndExpression )* 
LogicalAndExpression = EqualityExpression ('&''&'_ EqualityExpression )* 
EqualityExpression = RelationalExpression (('=''='_  / '!''='_ ) RelationalExpression )* 
RelationalExpression = UnaryExpression (('<'!('=')_  / '<''='!('=')_  / '>'!('=')_  / '>''='!('=')_ ) UnaryExpression )* 
UnaryExpression = (PostfixExpression  / '!'!('=')_ UnaryExpression ) 
PostfixExpression = (FunctionCall  / PrimaryExpression ) 
FunctionCall = PrimaryExpression (_FunctionCall )+ 
_FunctionCall = '('_ (_ArgumentExpressionList )?')'_ 
_ArgumentExpressionList = AssignmentExpression (','_ AssignmentExpression )* 
PrimaryExpression = (Constant  / '('_ Expression ')'_  / FunctionExpression  / Identifier ) 
FunctionExpression = 'f''u''n''c''t''i''o''n'!(W )_ (Identifier _ )?'('_ FunctionParamList ')'_ Block ':'':'_ 
Constant = (IntegerLiteral  / BooleanLiteral  / StringLiteral  / NullLiteral ) 
IntegerLiteral = DECIMAL _ 
DECIMAL = ([1-9](DIGIT )*  / '0') 
BooleanLiteral = ('t''r''u''e'!(W )_  / 'f''a''l''s''e'!(W )_ ) 
StringLiteral = '"'(STRING_CONTENT )* '"'_ 
STRING_CONTENT = !([\n"\\]).
NullLiteral = 'n''u''l''l'!(W )_ 