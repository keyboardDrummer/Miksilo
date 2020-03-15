package miksilo.modularLanguages.core.bigrammar

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.exceptions.TestFailedException

class TestColumnAlignment extends AnyFunSuite with WhitespaceTriviaSequenceCombinators {

  //This test shows that when translating the bigrammars to parsers, some of the structure is lost.
  //We might expect this parse and print to become identity for this input, but it doesn't even parse.
  test("test") {
    val input = "Hallo.              Hier staat een tweede gesprek." + System.lineSeparator() +
                "Hoe gaat het met je?"
    val grammar: BiGrammar = (keywordGrammar("Hallo.") % keywordGrammar("Hoe gaat het met je?")) ~ keywordGrammar("Hier staat een tweede gesprek.")
    assertThrows[TestFailedException](TestGrammarUtils.parseAndPrintSame(input, None, grammar))
  }
}
