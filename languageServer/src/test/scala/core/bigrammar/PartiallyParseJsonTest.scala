package core.bigrammar

import core.bigrammar.grammars.{BiFallback, Labelled, NumberGrammar, StringLiteral}
import core.language.node.GrammarKey
import org.scalatest.FunSuite

class PartiallyParseJsonTest extends FunSuite with DefaultBiGrammarWriter {

  import BiGrammarToParser._

  object Json extends GrammarKey
  val jsonGrammar = new Labelled(Json, new BiFallback(UnknownExpression, "value"))
  private val memberParser: BiGrammar = StringLiteral ~< ":" ~ jsonGrammar
  private val objectParser: BiGrammar = "{" ~> memberParser.manySeparated(",") ~< "}"
  object UnknownExpression {
    override def toString = "<unknown>"
  }
  jsonGrammar.addAlternative(StringLiteral | objectParser | NumberGrammar)
  val jsonParser = toParserBuilder(jsonGrammar)

  test("object with single member with number value") {
    val input = """{"person":3}"""
    val result = jsonParser.getWholeInputParser.parse(new Reader(input))
    val value = getSuccessValue(result)
    assertResult(List(("person","3")))(value)
  }

  test("object with single member with string value") {
    val input = """{"person":"remy"}"""
    val result = jsonParser.getWholeInputParser.parse(new Reader(input))
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
    val input = """{"Resources":{"NotificationTopic":{"Type":"AWS::SNS::Topic","Properties":{"Subscription"}}}}"""
    val expectation = List(("Resources",List(("NotificationTopic",List(("Type","AWS::SNS::Topic"), ("Properties",List(("Subscription",UnknownExpression))))))))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("real life example 2") {
    val input = """{"Resources":{"NotificationTopic":{"Properties":{"Subsc"""
    val expectation = List(("Resources",List(("NotificationTopic",List(("Properties",List(("Subsc",UnknownExpression))))))))
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  test("some test that triggers the case that Sequence left and right fail, the left failure is bigger and the right has no default but a partialResult") {

  }

  ignore("garbage before key") {
    val input = """{g"person":3}"""
    val expectation = List("person" -> "3")
    assertInputGivesPartialFailureExpectation(input, expectation)
  }

  private def assertInputGivesPartialFailureExpectation(input: String, expectation: Any) = {
    val result = jsonParser.getWholeInputParser.parseUntilBetterThanNextOrXSteps(new Reader(input))
    assert(!result.successful)
    assert(result.resultOption.nonEmpty)
    assertResult(expectation)(result.resultOption.get)
  }

  private def getSuccessValue(result: SingleParseResult[Any]) = {
    assert(result.successful)
    result.resultOption.get
  }
}


