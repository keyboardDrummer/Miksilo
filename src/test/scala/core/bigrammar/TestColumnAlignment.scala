package core.bigrammar

class TestColumnAlignment extends GrammarDocumentWriter {

  def test() {
    val input = "Hallo.              Hier staat een tweede gesprek." + System.lineSeparator() +
                "Hoe gaat het met je?"
    val grammar: BiGrammar = (("Hallo." : BiGrammar) % "Hoe gaat het met je?") ~ "Hier staat een tweede gesprek."
    TestGrammarUtils.parseAndPrintSame(input, None, grammar)
  }
}
