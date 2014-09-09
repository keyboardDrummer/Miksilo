package core.grammarDocument

import org.junit.{Ignore, Assert, Test}

class TestRecursion extends GrammarDocumentWriter {

  val input = "!!!!!"

  @Test
  def testRightRecursion() {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.orToInner("!" ~ grammar)
    grammar.orToInner(produce(null))

    testUsingGrammar(grammar)
  }

  @Test
  def testDirectRecursion() {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.orToInner("!" ~ grammar)
    grammar.orToInner(grammar)
    grammar.orToInner(produce(null))

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
    grammar.orToInner(produce(null))
    grammar.orToInner(grammar ~ "!")

    testUsingGrammar(grammar)
  }

  @Test
  def testLeftRecursionPrintOnly() {
    val grammarDocument: Labelled = new Labelled("leftRec")
    grammarDocument.orToInner(grammarDocument ~ "!")
    grammarDocument.orToInner(produce(null))

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
    inner.orToInner(outer ~ "!")
    inner.orToInner(produce(null))

    val document = PrintValueUsingGrammarDocument.toDocument(getExpectedLeftRecursiveResult, outer)
    val documentResult = document.renderString()
    Assert.assertEquals(input, documentResult)
  }
}
