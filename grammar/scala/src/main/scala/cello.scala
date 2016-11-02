/**
  * Created by ryo on 2016/11/01.
  */

import scala.util.parsing.combinator._

object CelloParser extends RegexParsers {
  def Program = TopLevel | ( Program ~ TopLevel )
  //def Program = TopLevel | ( HOGE ~ TopLevel )
  def TopLevel = ( ImportDeclarations ~ Declaration ) | Declaration | ';'
  def ImportDeclarations = ImportDeclaration | ( ImportDeclarations ~ ImportDeclaration )
  //def ImportDeclarations = ImportDeclaration | ( HOGE ~ ImportDeclaration )
  def ImportDeclaration = IMPORT ~ PackageName ~ ';'
  def PackageName = QualifiedName ~ '.' ~ '*'
  def Declaration = FunctionDeclaration | VariableDeclaration
  def FunctionDeclaration = ( Type ~ NAME ~ '(' ~ FunctionParamList ~ ')' ~ Block ) | ( Type ~ NAME ~ '(' ~ FunctionParamList ~ ')' ~ ';' ) | ( Type ~ NAME ~ '(' ~ ')' ~ Block ) | ( Type ~ NAME ~ '(' ~ ')' ~ ';' ) | ( Type ~ Block )
  def FunctionParamList = FunctionParam | ( FunctionParamList ~ ',' ~ FunctionParam ) //| ',' ~ VAR_LEN_PARAM
  //def FunctionParamList = FunctionParam | ( HOGE ~ ',' ~ FunctionParam ) //| ',' ~ VAR_LEN_PARAM
  def FunctionParam = Type | ( Type ~ NAME )
  def Block = ( '{' ~ BlockInner ~ '}' ) | '{' ~ '}'
  def BlockInner = Statement | Declaration | ( BlockInner ~ Statement ) | ( BlockInner ~ Declaration )
  //def BlockInner = Statement | Declaration | ( HOGE ~ Statement ) | ( HOGE ~ Declaration )
  def Statement = Block | IfStatement | ReturnStatement | ExpressionStatement | ';'
  def IfStatement = ( IF ~ '(' ~ Expression ~ ')' ~ Block ) | ( IF ~ '(' ~ Expression ~ ')' ~ Block ~ ELSE ~ Block )
  def ReturnStatement = ( RETURN ~ ';' ) | ( RETURN ~ Expression ~ ';' )
  def ExpressionStatement = Expression ~ ';'
  def VariableDeclaration = Type ~ VariableList ';'
  def VariableList = InitDecl | ( VariableList ~ ',' ~ InitDecl )
  //def VariableList = InitDecl | ( HOGE ~ ',' ~ InitDecl )
  def InitDecl = NAME | ( NAME ~ '=' ~ Initializer )
  def Initializer = AssignmentExpression
  def Type = INT_TYPE | BOOLEAN_TYPE | STRING_TYPE | LONG_TYPE
  def Expression = AssignmentExpression | ( Expression ~ ',' ~ AssignmentExpression )
  //def Expression = AssignmentExpression | ( HOGE ~ ',' ~ AssignmentExpression )
  def AssignmentExpression = ( UnaryExpression ~ '=' ~ AssignmentExpression ) | ConditionalExpression //FIXME
  //def AssignmentExpression = ( UnaryExpression ~ '=' ~ HOGE ) | ConditionalExpression
  def ConditionalExpression = LogicalANDExpression | ( ConditionalExpression ~ OR ~ LogicalANDExpression )
  //def ConditionalExpression = LogicalANDExpression | ( HOGE ~ OR ~ LogicalANDExpression )
  def LogicalANDExpression = EqualityExpression | ( LogicalANDExpression ~ AND ~ EqualityExpression )
  //def LogicalANDExpression = EqualityExpression | ( HOGE ~ AND ~ EqualityExpression )
  def EqualityExpression = RelationalExpression | ( EqualityExpression ~ EQ ~ RelationalExpression ) | ( EqualityExpression ~ NE ~ RelationalExpression )
  //def EqualityExpression = RelationalExpression | ( HOGE ~ EQ ~ RelationalExpression ) | ( HOGE ~ NE ~ RelationalExpression )
  def RelationalExpression = UnaryExpression | ( RelationalExpression ~ '<' ~ UnaryExpression ) | ( RelationalExpression ~ '>' ~ UnaryExpression )
  //def RelationalExpression = UnaryExpression | ( HOGE ~ '<' ~ UnaryExpression ) | ( HOGE ~ '>' ~ UnaryExpression )
  def UnaryExpression = PostfixExpression | ( '!' ~ UnaryExpression )
  //def UnaryExpression = PostfixExpression | ( '!' ~ HOGE )
  def PostfixExpression = PrimaryExpression | ( PrimaryExpression ~ FunctionCall )
  def FunctionCall = ( '(' ~ ArgumentExpressionList ~ ')' ) | ( '(' ~ ')' )
  def ArgumentExpressionList = AssignmentExpression | ( ArgumentExpressionList ~ ',' ~ AssignmentExpression )
  //def ArgumentExpressionList = AssignmentExpression | ( HOGE ~ ',' ~ AssignmentExpression )
  def PrimaryExpression = Literal | ( '(' ~ Expression ~ ')' )
  def QualifiedName = NAME | ( QualifiedName ~ '.' ~ NAME )
  //def QualifiedName = NAME | ( HOGE ~ '.' ~ NAME )
  def Literal = INT | TRUE | FALSE | STRING | NULL_LITERAL | NAME

  //def VAR_LEN_PARAM =
  def EQ = '='
  def NE = "!="
  def AND = "&&"
  def OR = "||"
  def STRING_TYPE = "String"
  def INT_TYPE = "int"
  def BOOLEAN_TYPE = "bool"
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
  def NAME = STRING
  def NULL_LITERAL = "null"


  def HOGE = ' '




  def apply(input: String): Either[String, Any] = parseAll(file, input) match {
    case Success(csvData, next) => Right(csvData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}

println(CelloParser("a"))

