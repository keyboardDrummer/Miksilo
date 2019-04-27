package core.parsers

import org.scalatest.FunSuite
import editorParsers.CorrectingParserWriter

class CorrectJsonTest extends FunSuite with CommonStringReaderParser with CorrectingParserWriter {
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

  ignore("garbage after number") {
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

  test("real life example") {
    val input = """{"Resources":{"NotificationTopic":{"Type":"AWS::SNS::Topic","Properties":{"Subscription"}}}}"""
    val expectation = List(("Resources",List(("NotificationTopic",List(("Type","AWS::SNS::Topic"), ("Properties",List(("Subscription",UnknownExpression))))))))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("real life example 2") {
    val input = """{"Resources":{"NotificationTopic":{"Properties":{"Subsc"""
    val expectation = List(("Resources",List(("NotificationTopic",List(("Properties",List(("Subsc",UnknownExpression))))))))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("garbage before key") {
    val input = """{g"person":3}"""
    val expectation = List("person" -> "3")
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  // Partially Parse tests start
  test("object with single member with string value, where the colon is missing") {
    val input = """{"person""remy"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy")))(result.resultOption.get)
  }

  test("two members but the first misses a colon") {
    val input = """{"person""remy","friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members but the comma is missing") {
    val input = """{"person":"remy""friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members both missing colon") {
    val input = """{"person""remy","friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members, no first colon and comma") {
    val input = """{"person""remy""friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("two members, no colons or comma") {
    val input = """{"person""remy""friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person","remy"), ("friend","jeroen")))(result.resultOption.get)
  }

  test("nested two members without colons/comma") {
    val input = """{"directory"{"person""remy""friend""jeroen"}}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List("directory" -> List("person" -> "remy", "friend" -> "jeroen")))(result.resultOption.get)
  }

  test("starting brace insertion") {
    val input = """{"person""remy":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult(List(("person",List(("remy","jeroen")))))(result.resultOption.get)
  }

  test("virtual left recursion through error correction") {
    val input = """doesNotMatchGrammar"""
    lazy val parser: EditorParser[Any] = "{" ~ parser | "x"
    val result = parser.parseWholeInput(new StringReader(input))
    assert(!result.successful)
    assertResult("x")(result.resultOption.get)
  }

  // Add test for left recursion and errors
  // Add test with multiple errors in one branch "b" => "a" "b" "c"
  // Add test with three way branch with 0,1,2 errors, and 0,2,1 errors.

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


