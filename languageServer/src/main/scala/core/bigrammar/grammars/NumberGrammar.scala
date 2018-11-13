package core.bigrammar.grammars

object NumberGrammar extends PrintUsingToStringGrammar {
  override def getParser(keywords: scala.collection.Set[String]): Parser[Any] = wholeNumber
}
