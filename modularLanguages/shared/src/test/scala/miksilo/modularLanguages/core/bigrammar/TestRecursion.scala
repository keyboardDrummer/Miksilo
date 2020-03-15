package miksilo.modularLanguages.core.bigrammar

import miksilo.modularLanguages.core.bigrammar.grammars.Labelled
import miksilo.modularLanguages.core.bigrammar.printer.BiGrammarToPrinter
import miksilo.modularLanguages.core.node.GrammarKey
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.StringOps

case class StringKey(value: String) extends GrammarKey {
  override lazy val toString: String = value
}

class TestRecursion extends AnyFunSuite with DefaultBiGrammarWriter {

  val input = "!!!!!"

  test("RightRecursion") {
    val grammar: Labelled = new Labelled(StringKey("leftRec"))
    grammar.addAlternative("!" ~ grammar)
    grammar.addAlternative(value(null))

    testUsingGrammar(grammar)
  }

  test("DirectRecursion") {
    val grammar: Labelled = new Labelled(StringKey("leftRec"))
    grammar.addAlternative("!" ~ grammar)
    grammar.addAlternative(grammar)
    grammar.addAlternative(value(null))

    testUsingGrammar(grammar)
  }

  def testUsingGrammar(grammar: Labelled): Unit = {
    TestGrammarUtils.parseAndPrintSame(input, Some(getExpectedRightRecursiveResult), grammar)
  }

  def getExpectedRightRecursiveResult: AnyRef = {
    new StringOps("!!!!!").map(s => s.toString).foldRight[AnyRef](null)((a, b) => (a, b))
  }

  ignore("LeftRecursion") {
    val grammar: Labelled = new Labelled(StringKey("leftRec"))
    grammar.addAlternative(value(null))
    grammar.addAlternative(grammar ~ "!")

    testUsingGrammar(grammar)
  }

  test("LeftRecursionPrintOnly") {
    val grammarDocument: Labelled = new Labelled(StringKey("leftRec"))
    grammarDocument.addAlternative(grammarDocument ~ "!")
    grammarDocument.addAlternative(value(null))

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
    inner.addAlternative(value(null))

    val document = BiGrammarToPrinter.toDocument(getExpectedLeftRecursiveResult, outer)
    val documentResult = document.renderString()
    assertResult(input)(documentResult)
  }
}
