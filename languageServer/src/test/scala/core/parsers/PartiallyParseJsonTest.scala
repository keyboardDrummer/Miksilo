package core.parsers

import org.scalatest.FunSuite
import editorParsers.EditorParserWriter

trait PartiallyParseJsonTest extends FunSuite with CommonStringReaderParser with EditorParserWriter {
  private lazy val memberParser = stringLiteral ~< ":" ~ jsonParser
  private lazy val objectParser = "{" ~> memberParser.manySeparated(",") ~< "}"
  object UnknownExpression {
    override def toString = "unknown"
  }
  private lazy val jsonParser: EditorParser[Any] = (stringLiteral | objectParser | wholeNumber).withDefault(UnknownExpression)

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
    val failure: ParseFailure[Any] = getFailure(result)
    assert(failure.partialResult.nonEmpty)
    assertResult(expectation)(failure.partialResult.get)
  }

  private def getFailure(result: ParseResult[Any]): ParseFailure[Any] = {
    assert(!result.successful)
    result.biggestFailure.asInstanceOf[ParseFailure[Any]]
  }

  private def getSuccessValue(result: ParseResult[Any]) = {
    assert(result.successful)
    result.resultOption.get
  }

  test("object with single member with string value, where the colon is missing") {
    val input = """{"person""remy"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy")))(value.partialResult.get)
  }

  test("two members but the first misses a colon") {
    val input = """{"person""remy","friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy"), ("friend","jeroen")))(value.partialResult.get)
  }

  test("two members but the comma is missing") {
    val input = """{"person":"remy""friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy"), ("friend","jeroen")))(value.partialResult.get)
  }

  test("two members both missing colon") {
    val input = """{"person""remy","friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy"), ("friend","jeroen")))(value.partialResult.get)
  }

  test("two members, no first colon and comma") {
    val input = """{"person""remy""friend":"jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy"), ("friend","jeroen")))(value.partialResult.get)
  }

  test("two members, no colons or comma") {
    val input = """{"person""remy""friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy"), ("friend","jeroen")))(value.partialResult.get)
  }

  test("nested two members without colons/comma") {
    val input = """{"directory"{"person""remy""friend""jeroen"}"""
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("directory",List(("person","remy"), ("friend","jeroen")))))(value.partialResult.get)
  }

  test("ambiguous problem") {
    val input = """{"person""remy":"jeroen"}""" // person:unknown,remy:jeroen of person:{remy:jeroen}
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy")))(value.partialResult.get)
  }
}


