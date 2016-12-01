/**
  * Created by ryo on 2016/11/01.
  */

import scala.util.parsing.combinator._
import scala.io.Source
import scala.collection.mutable.StringBuilder

object epsilon{
  def main (args:Array[String]):Unit={
    var firstest = 1000000.0
    for (i <- 1 to 5){
      val source2 = Source.fromFile(args(0), "UTF-8")
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
      //println(epsilonParser(aBuffer.toString))
      epsilonParser(aBuffer.toString)
      val end = System.nanoTime()
      val time = (end-start)/1000000.0
      if ( firstest > time ) firstest = time
    }
    println(firstest + " [ms]")
  }
}

object epsilonParser extends RegexParsers {
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r

  def Program = TopLevel.*
  def TopLevel = Declaration | ";"
  def Declaration = FunctionDeclaration | VariableDeclaration
  def FunctionDeclaration = ( "function"  ~ NAME  ~ "()" ~ Block ) | ( "function"  ~ NAME  ~ "(" ~ FunctionParamList ~ ")" ~ Block )
  def FunctionParamList: Parser[Any] = FunctionParam ~ ("," ~ ( VAR_LEN_PARAM | FunctionParam ) ).*
  def FunctionParam = NAME
  def Block = "{" ~ "}" | ( "{" ~ BlockInner ~ "}" )
  def BlockInner: Parser[Any] = ( Statement | Declaration ) ~ ( Statement | Declaration ).*
  def Statement: Parser[Any] = Block | IfStatement | ReturnStatement | ExpressionStatement | ";"
  def IfStatement = ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else1"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else2"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else3"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else4"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else5"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else6"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else7"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else8"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "else9"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "elseA"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "elseB"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ "elseC"  ~ Block ) |
    ( IF  ~ "(" ~ Expression ~ ")" ~ Block ~ ELSE  ~ Block )
  def ReturnStatement = ( RETURN ~ ";" ) | ( RETURN ~ Expression ~ ";" )
  def ExpressionStatement = Expression ~ ";"
  def VariableDeclaration = "var"  ~ VariableList ~ ";"
  def VariableList = InitDecl ~ ( "," ~ InitDecl ).*
  def InitDecl = ( ( NAME ~ "=" ~ Initializer ) | NAME )
  def Initializer = AssignmentExpression
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
  def PrimaryExpression : Parser[Any] = ( "(" ~ Expression ~ ")" ) | Literal | FunctionExpression
  def FunctionExpression : Parser[Any] = "function" ~ "(" ~ FunctionParamList ~ ")" ~ Block ~ "lambda" |
    "function" ~ "()" ~ Block ~ "lambda" |
    "function" ~ NAME ~ "(" ~ FunctionParamList ~ ")" ~ Block ~ "lambda" |
    "function" ~ NAME ~ "()" ~ Block ~ "lambda"
  def Literal = INT | TRUE | FALSE | NULL_LITERAL | STRING | NAME
  def STRING = "\"" ~ ( not("\"") ~ ".".r ).* ~ "\""

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
  def INT = ( "[1-9][0-9]*".r | "0" )
  def NAME = "[a-zA-Z0-9_$]+".r
  def NULL_LITERAL = "null"



  def apply(input: String): Either[String, Any] = parseAll(Program, input) match {
    case Success(postalCodeData, next) => Right(postalCodeData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}
