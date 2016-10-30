package core.bigrammar

import core.bigrammar.printer.{BiGrammarToPrinter, BiGrammarToPrinter$, PrintError}
import org.junit.{Assert, Test}
import org.scalatest.FunSuite

class TestFailureFeedback extends FunSuite with GrammarDocumentWriter {

  test("FailureFeedback") {
    val input = core.grammar.~("1", "2")
    val grammar: BiGrammar = ("1": BiGrammar) *

    try {
      BiGrammarToPrinter.toDocument(input, grammar)
      assert(false)
    } catch {
      case e: PrintError =>
    }
  }
}
