package core.parsers

import org.scalatest.FunSuite

class JsonTest extends FunSuite with CommonParsers {

  private lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  private lazy val objectParser = "{" ~> Many(memberParser) ~< "}"
  object UnknownExpression
  private lazy val jsonParser: Parser[Any] = WithDefault(stringLiteral | objectParser | wholeNumber, UnknownExpression)

  test("basic") {
    val input = """{"person":3}"""
    val result = jsonParser.parse(StringReader(input.toCharArray))
    assert(result.isInstanceOf[ParseSuccess[_]])
  }

  test("did not finish") {
    val input = """3blaa"""
    val result = jsonParser.parse(StringReader(input.toCharArray))
    assert(result.isInstanceOf[ParseFailure[_]])
    //val failure = result.asInstanceOf[ParseFailure[Any]]
  }

  test("basic error") {
    val input = """{"person""""
    val result = jsonParser.parse(StringReader(input.toCharArray))
    assert(result.isInstanceOf[ParseFailure[_]])
    val failure = result.asInstanceOf[ParseFailure[Any]]
    assertResult(Some(List((""""person"""", UnknownExpression))))(failure.partialResult)
  }
}
