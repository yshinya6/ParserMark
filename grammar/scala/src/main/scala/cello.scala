/**
  * Created by ryo on 2016/11/01.
  */

import scala.util.parsing.combinator._

object CelloParser extends RegexParsers {
  def Program = TopLevel ~ ( TopLevel ).*
  def TopLevel = ( ImportDeclarations ~ Declaration ) | Declaration | ';'
  def ImportDeclarations = ImportDeclaration ~ ( ImportDeclaration ).*
  def ImportDeclaration = IMPORT ~ PackageName ~ ';'
  def PackageName = QualifiedName ~ '.' ~ '*'
  def Declaration = FunctionDeclaration | VariableDeclaration
  def FunctionDeclaration = ( Type ~ NAME ~ '(' ~ FunctionParamList ~ ')' ~ Block ) | ( Type ~ NAME ~ '(' ~ FunctionParamList ~ ')' ~ ';' ) | ( Type ~ NAME ~ '(' ~ ')' ~ Block ) | ( Type ~ NAME ~ '(' ~ ')' ~ ';' ) | ( Type ~ Block )
  // def FunctionParamList = FunctionParam | ( FunctionParamList ~ ',' ~ FunctionParam ) | ',' ~ VAR_LEN_PARAM
  def FunctionParamList = ( FunctionParam | ( ',' ~ VAR_LEN_PARAM ) ) ( ',' ~  FunctionParam | ( ',' ~ VAR_LEN_PARAM )).*
  def FunctionParam = Type | ( Type ~ NAME )
  def Block = ( '{' ~ BlockInner ~ '}' ) | '{' ~ '}'
  def BlockInner = ( Statement | Declaration ) ~ ( Statement | Declaration ).*
  def Statement = Block | IfStatement | ReturnStatement | ExpressionStatement | ';'
  def IfStatement = ( IF ~ '(' ~ Expression ~ ')' ~ Block ) | ( IF ~ '(' ~ Expression ~ ')' ~ Block ~ ELSE ~ Block )
  def ReturnStatement = ( RETURN ~ ';' ) | ( RETURN ~ Expression ~ ';' )
  def ExpressionStatement = Expression ~ ';'
  def VariableDeclaration = Type ~ VariableList ';'
  def VariableList = InitDecl ~ ( ',' ~ InitDecl ).*
  def InitDecl = NAME | ( NAME ~ '=' ~ Initializer )
  def Initializer = AssignmentExpression
  def Type = INT_TYPE | BOOLEAN_TYPE | STRING_TYPE | LONG_TYPE
  def Expression = AssignmentExpression ~ ( ',' ~ AssignmentExpression ).*
  def AssignmentExpression = ( UnaryExpression ~ '=' ).* ~ ConditionalExpression //FIXME
  def ConditionalExpression = LogicalANDExpression ~ ( OR ~ LogicalANDExpression ).*
  def LogicalANDExpression = EqualityExpression ~ ( AND ~ EqualityExpression ).*
  def EqualityExpression = RelationalExpression ~ ( ( EQ | NE ) ~ RelationalExpression ).*
  def RelationalExpression = UnaryExpression ~ (( '<' | '>' ) ~ UnaryExpression ).*
  def UnaryExpression = '!'.* ~ PostfixExpression
  def PostfixExpression = PrimaryExpression | ( PrimaryExpression ~ FunctionCall )
  def FunctionCall = ( '(' ~ ArgumentExpressionList ~ ')' ) | ( '(' ~ ')' )
  def ArgumentExpressionList = AssignmentExpression ~ ( ',' ~ AssignmentExpression ).*
  def PrimaryExpression = Literal | ( '(' ~ Expression ~ ')' )
  def QualifiedName = NAME ~ ( '.' ~ NAME ).*
  def Literal = INT | TRUE | FALSE | STRING | NULL_LITERAL | NAME

  def VAR_LEN_PARAM = "..."
  def EQ = "=="
  def NE = "!="
  def AND = "&&"
  def OR = "||"
  def STRING_TYPE = "string"
  def INT_TYPE = "int"
  def BOOLEAN_TYPE = "boolean"
  def LONG_TYPE = "long"
  def IF = "if"
  def FOR = "for"
  def ELSE = "else"
  def RETURN = "return"
  def FALSE = "false"
  def TRUE = "true"
  def IMPORT = "import"
  def INT = """[0-9][1-9].*""".r
  def STRING = "[a-zA-Z]+".r
  def NAME = "[a-zA-Z0-9_]+".r
  def NULL_LITERAL = "null"



  def apply(input: String): Either[String, Any] = parseAll(file, input) match {
    case Success(csvData, next) => Right(csvData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}

