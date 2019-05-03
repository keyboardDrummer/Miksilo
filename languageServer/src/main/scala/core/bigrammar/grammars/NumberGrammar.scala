package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser
import BiGrammarToParser._

object NumberGrammar extends PrintUsingToStringGrammar {
  override def getParser(keywords: scala.collection.Set[String]): Self[Any] = wholeNumber
}
