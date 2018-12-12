package core.bigrammar

import core.bigrammar.grammars.{Labelled, NumberGrammar, StringLiteral}
import core.language.node.GrammarKey
import core.parsers.strings.{StringParserWriter, StringReader}
import org.scalatest.FunSuite

class PartiallyParseJsonTest extends FunSuite with DefaultBiGrammarWriter with StringParserWriter {

  object Json extends GrammarKey
  val jsonGrammar = new Labelled(Json)
  private val memberParser: BiGrammar = StringLiteral ~< ":" ~ jsonGrammar
  private val objectParser: BiGrammar = "{" ~> memberParser.manySeparated(",") ~< "}"
  object UnknownExpression
  jsonGrammar.inner = new core.bigrammar.grammars.WithDefault(StringLiteral | objectParser | NumberGrammar, UnknownExpression)
  val jsonParser = BiGrammarToParser.toParser(jsonGrammar)

  test("object with single member with number value") {
    val input = """{"person":3}"""
    val result = jsonParser.parseWholeInput(StringReader(input.toCharArray))
    val value = getSuccessValue(result)
    assertResult(List(("person","3")))(value)
  }

  test("object with single member with string value") {
    val input = """{"person":"remy"}"""
    val result = jsonParser.parseWholeInput(StringReader(input.toCharArray))
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

  test("real life example") {
    val input = """{"Resources":{"NotificationTopic":{"Type":"AWS::SNS::Topic","Properties":{"Subsc"""
    val expectation = List(("Resources",List(("NotificationTopic",List(("Type","AWS::SNS::Topic"), ("Properties",List(("Subsc",UnknownExpression))))))))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("some test that triggers the case that Sequence left and right fail, the left failure is bigger and the right has no default but a partialResult") {

  }

  private def assertInputGivesPartialFailureExpectation(input: String, expectation: Any) = {
    val result = jsonParser.parseWholeInput(StringReader(input.toCharArray))
    val failure: ParseFailure[Any] = getFailure(result)
    assert(failure.partialResult.nonEmpty)
    assertResult(expectation)(failure.partialResult.get)
  }

  private def getFailure(result: BiGrammarToParser.ParseResult[Any]): ParseFailure[Any] = {
    assert(result.isInstanceOf[ParseFailure[_]])
    result.asInstanceOf[ParseFailure[Any]]
  }

  private def getSuccessValue(result: BiGrammarToParser.ParseResult[Any]) = {
    assert(result.isInstanceOf[ParseSuccess[_]])
    result.asInstanceOf[ParseSuccess[Any]].result
  }
}


