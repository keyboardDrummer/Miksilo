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

  test("multiple recoverable errors") {
    val input = """{"person""remy","friend""jeroen"}"""
    // This case shows two problem, deze case is probleem. laat zien dat errors en offset geen stricte importance volgorde hebben.
    // person:remy (1 errors) vs person:remy,friend:jeroen (2 errors)
    // remy (0 errors) vs (remy:{friend:{jeroen:{}}}} (6 errors)
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy"), ("friend","jeroen")))(value.partialResult.get)
  }

  test("ambigious problem") {
    val input = """{"person""remy":"jeroen"}""" // person:{remy:jeroen} of person:unknown,remy:jeroen of person:remy
    val result = jsonParser.parseWholeInput(new StringReader(input))
    val value = getFailure(result)
    assertResult(List(("person","remy")))(value.partialResult.get)
  }
}


