package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser._

object NumberGrammar extends PrintUsingToStringGrammar {
  override def getParserBuilder(keywords: scala.collection.Set[String]): Parser[Any] = wholeNumber
}
