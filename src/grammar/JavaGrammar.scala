package grammar

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import org.junit.{Assert, Test}

object JavaGrammar extends StandardTokenParsers with PackratParsers {

  trait Expression extends Statement
  trait Statement
  case class Variable(name: String) extends Expression
  case class While(condition: Expression, body: Seq[Statement]) extends Statement
  case class Clazz(name: String, statements: Seq[Statement])

  def getGrammar(input: String) : ParseResult[Clazz] =
  {
    lexical.delimiters ++= List("{","}","(",")")
    lexical.reserved ++= List("while","class")
    lazy val expression : PackratParser[Expression] = ident.map(name => Variable(name))
    lazy val statement : PackratParser[Statement] = expression | whileLoop
    lazy val body = statement*
    lazy val whileLoop : PackratParser[While] = "while" ~> parens(expression) ~ block(body) ^^
      {case condition~body => new While(condition,body)}
    lazy val clazz = "class" ~> ident ~ ("{" ~> body <~ "}") ^^ { case name~body => new Clazz(name,body) }
    clazz.apply(new PackratReader(new lexical.Scanner(input)))
  }

  def block[T](parser: Parser[T]) = "{" ~> parser <~ "}"
  def parens[T](parser: Parser[T]) = "(" ~> parser <~ ")"
}

class TestJavaGrammar
{
  @Test
  def testGrammar
  {
    val clazz = JavaGrammar.getGrammar("class Test { while(hallo) { hallo}}")
    Assert.assertNotNull(clazz)
  }
}