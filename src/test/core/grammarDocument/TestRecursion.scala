package core.grammarDocument

import org.junit.{Assert, Test}

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
    TestGrammarUtils.parseAndPrint(input, Some(getExpectedResult), grammar)
  }

  def getExpectedResult: AnyRef = {
    "!!!!!".map(s => s.toString).foldRight[AnyRef](null)((a, b) => core.grammar.~(a, b))
  }

  @Test
  def testLeftRecursion() {
    val grammar: Labelled = new Labelled("leftRec")
    grammar.orToInner(grammar ~ "!")
    grammar.orToInner(produce(null))

    testUsingGrammar(grammar)
  }


  @Test
  def testLeftRecursionPrintOnly() {
    val grammarDocument: Labelled = new Labelled("leftRec")
    grammarDocument.orToInner(grammarDocument ~ "!")
    grammarDocument.orToInner(produce(null))

    val result = "!!!!!".map(s => s.toString).foldLeft[AnyRef](null)((a, b) => core.grammar.~(a, b))

    val document = PrintValueUsingGrammarDocument.toDocument(result, grammarDocument).get
    val documentResult = document.renderString()
    Assert.assertEquals(input, documentResult)
  }
}
