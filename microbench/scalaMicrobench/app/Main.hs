import scala.util.parsing.combinator._
import scala.io.Source
import scala.collection.mutable.StringBuilder
object jointChoice1{
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
    Parser(aBuffer.toString)
    val end = System.nanoTime()
    val time = (end-start)/1000000.0
    if ( firstest > time ) firstest = time
  }
  println(firstest + " [ms]")
}
}

object Parser extends RegexParsers {
def S: Parser[Any] =(Na|Nb).*
def Prefix= '@'
def Na: Parser[Any] =Prefix~'a'
def Nb: Parser[Any] =Prefix~'b'
def apply(input: String): Either[String, Any] = parseAll(S, input) match {    case Success(postalCodeData, next) => Right(postalCodeData)
    case NoSuccess(errorMessage, next) => Left(s"$errorMessage on line ${next.pos.line} on column ${next.pos.column}")
  }
}
