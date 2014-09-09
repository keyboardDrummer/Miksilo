package core.grammarDocument

import org.junit.{Assert, Test}
class TestFailureFeedback extends GrammarDocumentWriter {


  @Test
  def testFailure()
  {
    val input = core.grammar.~("1","2")
    val grammar : GrammarDocument = ("1" : GrammarDocument)*
    val printResult = PrintValueUsingGrammarDocument.toDocument(input, grammar)
    Assert.assertEquals(input, printResult)
  }
}
