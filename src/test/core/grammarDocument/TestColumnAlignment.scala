package core.grammarDocument

import org.junit.{Ignore, Test}

class TestColumnAlignment extends GrammarDocumentWriter {

  @Ignore
  @Test
  def test() {
    val input = "Hallo.              Hier staat een tweede gesprek." + System.lineSeparator() +
                "Hoe gaat het met je?"
    val grammar: BiGrammar = (("Hallo." : BiGrammar) % "Hoe gaat het met je?") ~ "Hier staat een tweede gesprek."
    TestGrammarUtils.parseAndPrint(input, None, grammar)
  }
}
