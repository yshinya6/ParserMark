/**
  * Created by ryo on 2016/11/01.
  */

import scala.util.parsing.combinator._
import scala.io.Source
import scala.collection.mutable.StringBuilder

object CelloParser extends RegexParsers {
  def Program = TopLevel ~ ( TopLevel ).*
  def TopLevel = ( ImportDeclarations ~ Declaration ) | Declaration | ";"
  def ImportDeclarations = ImportDeclaration ~ ( ImportDeclaration ).*
  def ImportDeclaration = IMPORT ~ PackageName ~ ";"
  def PackageName = QualifiedName ~ ".*?".r
  def Declaration = FunctionDeclaration | VariableDeclaration
  def FunctionDeclaration = ( Type ~ Block ) | ( Type ~ NAME ~ "();" ) | ( Type ~ NAME ~ "()" ~ Block ) | ( Type ~ NAME ~ "(" ~ FunctionParamList ~ ");" ) | ( Type ~ NAME ~ "(" ~ FunctionParamList ~ ")" ~ Block )
  def FunctionParamList: Parser[Any] = FunctionParam | ( FunctionParamList ~ "," ~ FunctionParam ) | "," ~ VAR_LEN_PARAM
  def FunctionParam = ( Type ~ NAME ) | Type
  def Block = "{}" | ( "{" ~ BlockInner ~ "}" )
  def BlockInner: Parser[Any] = ( Statement | Declaration ) ~ ( Statement | Declaration ).*
  def Statement: Parser[Any] = Block | IfStatement | ReturnStatement | ExpressionStatement | ";"
  def IfStatement = ( IF ~ "(" ~ Expression ~ ")" ~ Block ) | ( IF ~ "(" ~ Expression ~ ")" ~ Block ~ ELSE ~ Block )
  def ReturnStatement = ( RETURN ~ ";" ) | ( RETURN ~ Expression ~ ";" )
  def ExpressionStatement = Expression ~ ";"
  def VariableDeclaration = Type ~ VariableList ~ ";"
  def VariableList = InitDecl ~ ( "," ~ InitDecl ).*
  def InitDecl = ( NAME ~ "=" ~ Initializer ) | NAME
  def Initializer = AssignmentExpression
  def Type = INT_TYPE | BOOLEAN_TYPE | STRING_TYPE | LONG_TYPE
  def Expression = AssignmentExpression ~ ( "," ~ AssignmentExpression ).*
  def AssignmentExpression = ( UnaryExpression ~ "!==".r ~ "=" ).* ~ ConditionalExpression //FIXME
  def ConditionalExpression = LogicalANDExpression ~ ( OR ~ LogicalANDExpression ).*
  def LogicalANDExpression = EqualityExpression ~ ( AND ~ EqualityExpression ).*
  def EqualityExpression = RelationalExpression ~ ( ( EQ | NE ) ~ RelationalExpression ).*
  def RelationalExpression = UnaryExpression ~ ( "<|>".r  ~ UnaryExpression ).*
  def UnaryExpression = "!*".r ~ PostfixExpression
  def PostfixExpression = ( PrimaryExpression ~ FunctionCall ) | PrimaryExpression
  def FunctionCall: Parser[Any] = ( "()" ) | ( "(" ~ ArgumentExpressionList ~ ")" )
  def ArgumentExpressionList = AssignmentExpression ~ ( "," ~ AssignmentExpression ).*
  def PrimaryExpression : Parser[Any] = ( "(" ~ Expression ~ ")" ) | Literal
  def QualifiedName = NAME ~ ( "." ~ NAME ).*
  def Literal = INT | TRUE | FALSE | NULL_LITERAL | STRING | NAME

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
  def INT = "[0-9][1-9]*".r
  def STRING = "[a-zA-Z]+".r
  def NAME = "[a-zA-Z0-9_]+".r
  def NULL_LITERAL = "null"



  def apply(input: String): Either[String, Any] = parseAll(Program, input) match {
    case Success(postalCodeData, next) => Right(postalCodeData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}

object input{
  def main (args:Array[String]):Unit={
    var firstest = 1000000.0
    for (i <- 1 to 5){
      val source2 = Source.fromFile("text.txt", "UTF-8")
      val aBuffer = new StringBuilder
      try {
        for (line <- source2.getLines) {
          aBuffer.append(line)
        }
      }
      finally {
        source2.close
      }
      val start = System.nanoTime()
      CelloParser(aBuffer.toString)
      val end = System.nanoTime()
      val time = (end-start)/1000000.0
      if ( firstest > time ) firstest = time
    }
    println(firstest + " [ms]")
  }
}