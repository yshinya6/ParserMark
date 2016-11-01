/**
  * Created by ryo on 2016/11/01.
  */

import scala.util.parsing.combinator._

class cello {

  object CSVParser extends RegexParsers {
    def Program = TopLevel | Program ~ TopLevel
    def TopLevel = ImportDeclarations ~ Declaration | Declaration | ';'
    def ImportDeclarations = ImportDeclaration | ImportDeclarations ~ ImportDeclaration
    def ImportDeclaration = IMPORT ~ PackageName ~ ';'
    def PackageName = QualifiedName ~ '.' ~ '*'
    def Declaration = FunctionDeclaration | VariableDeclaration
    def FunctionDeclaration = Type ~ NAME ~ '(' ~ FunctionParamList ~ ')' ~ Block　| Type ~ NAME ~ '(' ~ FunctionParamList ~ ')' ~ ';' | Type ~ NAME ~ '(' ~ ')' ~ Block | Type ~ NAME ~ '(' ~ ')' ~ ';' | Type ~ Block
    def FunctionParamList = FunctionParam | FunctionParamList ~ ',' ~ FunctionParam | ',' ~ VAR_LEN_PARAM
    def FunctionParam = Type | Type ~ NAME
    def Block = '{' ~ BlockInner ~ '}' | '{' ~ '}'
    def BlockInner = Statement | Declaration | BlockInner ~ Statement | BlockInner ~ Declaration
    def Statement = Block | IfStatement | ReturnStatement | ExpressionStatement | ';'
    def IfStatement = IF ~ '(' ~ Expression ~ ')' ~ Block | IF ~ '(' ~ Expression ~ ')' ~ Block ~ ELSE ~ Block
    def ReturnStatement = RETURN ~ ';' | RETURN ~ Expression ~ ';'
    def ExpressionStatement = Expression ~ ';'
    def VariableDeclaration = Type ~ VariableList ';'
    def VariableList = InitDecl | VariableList ~ ',' ~ InitDecl
    def InitDecl = NAME | NAME ~ '=' ~ Initializer
    def Initializer = AssignmentExpression
    def Type = INT_TYPE | BOOLEAN_TYPE | STRING_TYPE | LONG_TYPE
    def Expression = AssignmentExpression | Expression ~ ',' ~ AssignmentExpression
    def AssignmentExpression = UnaryExpression ~ '=' ~ AssignmentExpression | ConditionalExpression //FIXME
    def ConditionalExpression = LogicalANDExpression | ConditionalExpression ~ OR ~ LogicalANDExpression
    def LogicalANDExpression = EqualityExpression | LogicalANDExpression ~ AND ~ EqualityExpression
    def EqualityExpression = RelationalExpression | EqualityExpression ~ EQ ~ RelationalExpression | EqualityExpression ~ NE ~ RelationalExpression
    def RelationalExpression = UnaryExpression | RelationalExpression ~ '<' ~ UnaryExpression | RelationalExpression ~ '>' ~ UnaryExpression
    def UnaryExpression = PostfixExpression | '!' ~ UnaryExpression
    def PostfixExpression = PrimaryExpression | PrimaryExpression ~ FunctionCall
    def FunctionCall = '(' ~ ArgumentExpressionList ~ ')' | '(' ~ ')'
    def ArgumentExpressionList = AssignmentExpression | ArgumentExpressionList ~ ',' ~ AssignmentExpression
    def PrimaryExpression = Literal | '(' ~ Expression ~ ')'
    def QualifiedName = NAME | QualifiedName ~ '.' ~ NAME
    def Literal = INT | TRUE | FALSE | STRING | NULL_LITERAL | NAME

  }
}
