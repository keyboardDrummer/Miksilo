package core.bigrammar.grammars

trait CustomGrammarWithoutChildren extends BiGrammar with NodePrinter {
  def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[Any]
}
