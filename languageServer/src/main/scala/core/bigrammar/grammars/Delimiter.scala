package core.bigrammar.grammars

case class Delimiter(value: String) extends StringGrammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParser(keywords: scala.collection.Set[String]): Parser[String] = literal(value)
}
