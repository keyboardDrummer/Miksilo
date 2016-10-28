package core.bigrammar

import core.bigrammar.printer.{BiGrammarToPrinter, BiGrammarToPrinter$}
import org.junit.{Assert, Ignore, Test}
import org.scalatest.FunSuite

class TestRecursion extends FunSuite with GrammarDocumentWriter {

  val input = "!!!!!"

  test("RightRecursion") {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.addOption("!" ~ grammar)
    grammar.addOption(produce(null))

    testUsingGrammar(grammar)
  }


  test("DirectRecursion") {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.addOption("!" ~ grammar)
    grammar.addOption(grammar)
    grammar.addOption(produce(null))

    testUsingGrammar(grammar)
  }

  def testUsingGrammar(grammar: Labelled) {
    TestGrammarUtils.parseAndPrintSame(input, Some(getExpectedRightRecursiveResult), grammar)
  }

  def getExpectedRightRecursiveResult: AnyRef = {
    "!!!!!".map(s => s.toString).foldRight[AnyRef](null)((a, b) => core.grammar.~(a, b))
  }

  ignore("LeftRecursion") {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.addOption(produce(null))
    grammar.addOption(grammar ~ "!")

    testUsingGrammar(grammar)
  }

  test("LeftRecursionPrintOnly") {
    val grammarDocument: Labelled = new Labelled("leftRec")
    grammarDocument.addOption(grammarDocument ~ "!")
    grammarDocument.addOption(produce(null))

    val result = getExpectedLeftRecursiveResult

    val document = BiGrammarToPrinter.toDocument(result, grammarDocument)
    val documentResult = document.renderString()
    assertResult(input)(documentResult)
  }

  def getExpectedLeftRecursiveResult: AnyRef = {
    "!!!!!".map(s => s.toString).foldLeft[AnyRef](null)((a, b) => core.grammar.~(a, b))
  }


  test("PrintingIndirectLeftRecursion") {
    val inner = new Labelled("boep")
    val outer = new Labelled("woep", inner)
    inner.addOption(outer ~ "!")
    inner.addOption(produce(null))

    val document = BiGrammarToPrinter.toDocument(getExpectedLeftRecursiveResult, outer)
    val documentResult = document.renderString()
    assertResult(input)(documentResult)
  }
}
