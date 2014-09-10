package core.grammarDocument

import org.junit.{Ignore, Assert, Test}

class TestRecursion extends GrammarDocumentWriter {

  val input = "!!!!!"

  @Test
  def testRightRecursion() {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.addOption("!" ~ grammar)
    grammar.addOption(produce(null))

    testUsingGrammar(grammar)
  }

  @Test
  def testDirectRecursion() {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.addOption("!" ~ grammar)
    grammar.addOption(grammar)
    grammar.addOption(produce(null))

    testUsingGrammar(grammar)
  }

  def testUsingGrammar(grammar: Labelled) {
    TestGrammarUtils.parseAndPrint(input, Some(getExpectedRightRecursiveResult), grammar)
  }

  def getExpectedRightRecursiveResult: AnyRef = {
    "!!!!!".map(s => s.toString).foldRight[AnyRef](null)((a, b) => core.grammar.~(a, b))
  }

  @Ignore
  @Test
  def testLeftRecursion() {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.addOption(produce(null))
    grammar.addOption(grammar ~ "!")

    testUsingGrammar(grammar)
  }

  @Test
  def testLeftRecursionPrintOnly() {
    val grammarDocument: Labelled = new Labelled("leftRec")
    grammarDocument.addOption(grammarDocument ~ "!")
    grammarDocument.addOption(produce(null))

    val result = getExpectedLeftRecursiveResult

    val document = PrintValueUsingGrammarDocument.toDocument(result, grammarDocument)
    val documentResult = document.renderString()
    Assert.assertEquals(input, documentResult)
  }

  def getExpectedLeftRecursiveResult: AnyRef = {
    "!!!!!".map(s => s.toString).foldLeft[AnyRef](null)((a, b) => core.grammar.~(a, b))
  }

  @Test
  def testPrintingIndirectLeftRecursion() {
    val inner = new Labelled("boep")
    val outer = new Labelled("woep", inner)
    inner.addOption(outer ~ "!")
    inner.addOption(produce(null))

    val document = PrintValueUsingGrammarDocument.toDocument(getExpectedLeftRecursiveResult, outer)
    val documentResult = document.renderString()
    Assert.assertEquals(input, documentResult)
  }
}
