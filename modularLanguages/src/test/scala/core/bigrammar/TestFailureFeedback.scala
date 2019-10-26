package core.bigrammar

import core.bigrammar.printer.{BiGrammarToPrinter, PrintError}
import org.scalatest.FunSuite

class TestFailureFeedback extends FunSuite with WhitespaceTriviaSequenceCombinators {

  test("FailureFeedback") {
    val input = ("1", "2")
    val grammar: BiGrammar = ("1": BiGrammar) *

    try {
      BiGrammarToPrinter.toDocument(input, grammar)
      assert(false)
    } catch {
      case e: PrintError =>
    }
  }
}
