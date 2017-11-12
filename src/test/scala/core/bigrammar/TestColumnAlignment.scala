package core.bigrammar

import org.scalatest.FunSuite
import org.scalatest.exceptions.TestFailedException

class TestColumnAlignment extends FunSuite with BiGrammarSequenceWriter {

  //This test shows that when translating the bigrammars to parsers, some of the structure is lost.
  //We might expect this parse and print to become identity for this input, but it doesn't even parse.
  test("test") {
    val input = "Hallo.              Hier staat een tweede gesprek." + System.lineSeparator() +
                "Hoe gaat het met je?"
    val grammar: BiGrammar = (("Hallo." : BiGrammar) % "Hoe gaat het met je?") ~ "Hier staat een tweede gesprek."
    assertThrows[TestFailedException](TestGrammarUtils.parseAndPrintSame(input, None, grammar))
  }
}
