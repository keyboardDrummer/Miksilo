package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

object NumberG extends PrintUsingToStringGrammar {
  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[Any] = BiGrammarToParser.wholeNumber
}
