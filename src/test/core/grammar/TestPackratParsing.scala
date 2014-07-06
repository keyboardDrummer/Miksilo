package core.grammar

import org.junit.{Test, Assert}

import scala.util.parsing.combinator.PackratParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers

class TestPackratParsing  extends StandardTokenParsers with PackratParsers
{
  lazy val parseTypeInner : PackratParser[TestType] =
    parseTypeInner <~ "[" <~ "]" ^^ (inner => new ArrayType(inner)) | "String" ^^ (_ => StringType)
  val parseType : PackratParser[TestType] = parseTypeInner

  lexical.reserved += "String"
  lexical.delimiters += "["
  lexical.delimiters += "]"

  @Test
  def testPackrat() {

    Assert.assertEquals(new ArrayType(StringType),
      parseType(new PackratReader(new lexical.Scanner("String[]"))).get)
  }

  trait TestType
  object StringType extends TestType
  case class ArrayType(inner: TestType) extends TestType
}
