package core.bigrammar

import core.bigrammar.grammars.Labelled
import core.bigrammar.printer.BiGrammarToPrinter
import org.scalatest.FunSuite

import scala.collection.immutable.StringOps

class TestRecursion extends FunSuite with DefaultBiGrammarWriter {

  val input = "!!!!!"

  test("RightRecursion") {
    val grammar: Labelled = new Labelled(StringKey("leftRec"))
    grammar.addAlternative("!" ~ grammar)
    grammar.addAlternative(valueGrammar(null))

    testUsingGrammar(grammar)
  }

  test("DirectRecursion") {
    val grammar: Labelled = new Labelled(StringKey("leftRec"))
    grammar.addAlternative("!" ~ grammar)
    grammar.addAlternative(grammar)
    grammar.addAlternative(valueGrammar(null))

    testUsingGrammar(grammar)
  }

  def testUsingGrammar(grammar: Labelled) {
    TestGrammarUtils.parseAndPrintSame(input, Some(getExpectedRightRecursiveResult), grammar)
  }

  def getExpectedRightRecursiveResult: AnyRef = {
    new StringOps("!!!!!").map(s => s.toString).foldRight[AnyRef](null)((a, b) => (a, b))
  }

  ignore("LeftRecursion") {
    val grammar: Labelled = new Labelled(StringKey("leftRec"))
    grammar.addAlternative(valueGrammar(null))
    grammar.addAlternative(grammar ~ "!")

    testUsingGrammar(grammar)
  }

  test("LeftRecursionPrintOnly") {
    val grammarDocument: Labelled = new Labelled(StringKey("leftRec"))
    grammarDocument.addAlternative(grammarDocument ~ "!")
    grammarDocument.addAlternative(valueGrammar(null))

    val result = getExpectedLeftRecursiveResult

    val document = BiGrammarToPrinter.toDocument(result, grammarDocument)
    val documentResult = document.renderString()
    assertResult(input)(documentResult)
  }

  def getExpectedLeftRecursiveResult: AnyRef = {
    new StringOps("!!!!!").map(s => s.toString).foldLeft[AnyRef](null)((a, b) => (a, b))
  }

  test("PrintingIndirectLeftRecursion") {
    val inner = new Labelled(StringKey("boep"))
    val outer = new Labelled(StringKey("woep"), inner)
    inner.addAlternative(outer ~ "!")
    inner.addAlternative(valueGrammar(null))

    val document = BiGrammarToPrinter.toDocument(getExpectedLeftRecursiveResult, outer)
    val documentResult = document.renderString()
    assertResult(input)(documentResult)
  }
}
