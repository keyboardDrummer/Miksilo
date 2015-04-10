package core.bigrammar

import core.bigrammar.printer.{BiGrammarToPrinter, PrintError, BiGrammarToPrinter$}
import org.junit.{Assert, Test}

class TestFailureFeedback extends GrammarDocumentWriter {


  @Test
  def testFailure() {
    val input = core.grammar.~("1", "2")
    val grammar: BiGrammar = ("1": BiGrammar) *

    try {
      BiGrammarToPrinter.toDocument(input, grammar)
      Assert.fail()
    } catch {
      case e: PrintError =>
    }
  }
}
