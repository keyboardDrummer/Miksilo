package core.parsers

import org.scalatest.FunSuite
import editorParsers.EditorParserWriter

trait PartiallyParseJsonTest extends FunSuite with CommonStringReaderParser with EditorParserWriter {
  private lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  private lazy val objectParser = "{" ~> memberParser.manySeparated(",") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  protected lazy val jsonParser: EditorParser[Any] = (stringLiteral | objectParser | wholeNumber).withDefault(UnknownExpression)

  test("object with single member with number value") {
    val input = """{"person":3}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getSuccessValue(result)
    assertResult(List(("person","3")))(value)
  }

  test("object with single member with string value") {
    val input = """{"person":"remy"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getSuccessValue(result)
    assertResult(List(("person","remy")))(value)
  }

  test("garbage after number") {
    val input = """3blaa"""
    assertInputGivesPartialFailureExpectation(input, "3")
  }

  test("nothing as input") {
    val input = ""
    assertInputGivesPartialFailureExpectation(input, UnknownExpression)
  }

  test("object start with nothing else") {
    val input = """{"""
    assertInputGivesPartialFailureExpectation(input, List.empty)
  }

  test("object member with only the key") {
    val input = """{"person""""
    val expectation = List(("""person""", UnknownExpression))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("object member with no expression") {
    val input = """{"person":"""
    val expectation = List(("person", UnknownExpression))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("object member with only an unfinished key") {
    val input = """{"person"""
    val expectation = List(("person", UnknownExpression))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("object member with an unfinished value") {
    val input = """{"person":"remy"""
    assertInputGivesPartialFailureExpectation(input, List(("person", "remy")))
  }

  test("object with a single member and comma") {
    val input = """{"person":3,"""
    val expectation = List(("person", "3"))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("object with a single member and half second member") {
    val input = """{"person":3,"second""""
    val expectation = List(("person", "3"), ("second", UnknownExpression))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  private def assertInputGivesPartialFailureExpectation(input: String, expectation: Any) = {
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assert(result.resultOption.nonEmpty)
    assertResult(expectation)(result.resultOption.get)
  }

  private def getSuccessValue(result: ParseWholeResult[Any]) = {
    assert(result.successful)
    result.resultOption.get
  }
}


