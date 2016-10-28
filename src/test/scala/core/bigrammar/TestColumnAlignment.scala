package core.bigrammar

import org.scalatest.FunSuite

class TestColumnAlignment extends FunSuite with GrammarDocumentWriter {

  test("test") {
    val input = "Hallo.              Hier staat een tweede gesprek." + System.lineSeparator() +
                "Hoe gaat het met je?"
    val grammar: BiGrammar = (("Hallo." : BiGrammar) % "Hoe gaat het met je?") ~ "Hier staat een tweede gesprek."
    TestGrammarUtils.parseAndPrintSame(input, None, grammar)
  }
}
