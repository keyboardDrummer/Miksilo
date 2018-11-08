package core.parsers

import org.scalatest.FunSuite

class JsonTest extends FunSuite with CommonParsers {

  private lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  private lazy val objectParser = "{" ~> Many(memberParser) ~< "}"
  object UnknownExpression
  private lazy val jsonParser: Parser[Any] = WithDefault(stringLiteral | objectParser | wholeNumber, UnknownExpression)

  test("object with single member with number value") {
    val input = """{"person":3}"""
    val result = jsonParser.parse(StringReader(input.toCharArray))
    val value = getSuccessValue(result)
    assertResult(List(("person","3")))(value)
  }

  test("object with single member with string value") {
    val input = """{"person":"remy"}"""
    val result = jsonParser.parse(StringReader(input.toCharArray))
    val value = getSuccessValue(result)
    assertResult(List(("person","remy")))(value)
  }

  test("garbage after number") {
    val input = """3blaa"""
    val result = jsonParser.parseWhole(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some("3"))(failure.partialResult)
  }

  test("nothing as input") {
    val input = ""
    val parser = jsonParser
    val result = parser.parse(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some(UnknownExpression))(failure.partialResult)
  }

  test("object start with nothing else") {
    val input = """{"""
    val parser = jsonParser
    val result = parser.parse(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some(List()))(failure.partialResult)
  }

  test("object member with only the key") {
    val input = """{"person""""
    val result = jsonParser.parse(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some(List(("""person""", UnknownExpression))))(failure.partialResult)
  }

  test("object member with no expression") {
    val input = """{"person":"""
    val parser = jsonParser
    val result = parser.parse(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some(List(("person", UnknownExpression))))(failure.partialResult)
  }

  test("object member with only an unfinished key") {
    val input = """{"person"""
    val parser = jsonParser
    val result = parser.parse(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some(List(("person", UnknownExpression))))(failure.partialResult)
  }

  test("object member with an unfinished value") {
    val input = """{"person":"remy"""
    val parser = jsonParser
    val result = parser.parse(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assertResult(Some(List(("person", "remy"))))(failure.partialResult)
  }

  private def getFailure(result: ParseResult[Any]): ParseFailure[Any] = {
    assert(result.isInstanceOf[ParseFailure[_]])
    result.asInstanceOf[ParseFailure[Any]]
  }

  private def getSuccessValue(result: ParseResult[Any]) = {
    assert(result.isInstanceOf[ParseSuccess[_]])
    result.asInstanceOf[ParseSuccess[Any]].result
  }
}
