package core.bigrammar.grammars

class BiFallback(value: Any, name: String) extends CustomGrammarWithoutChildren with BiGrammarWithoutChildren {
  override def getParserBuilder(keywords: collection.Set[String]) = Fallback(value, name)

  override def containsParser(recursive: BiGrammar => Boolean) = false

  override def write(from: WithMap[Any]) = Printer.fail("fallback cannot print")
}
