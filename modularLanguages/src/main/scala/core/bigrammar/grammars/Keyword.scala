package core.bigrammar.grammars

case class Keyword(var value: String, reserved: Boolean = true, verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParserBuilder(keywords: scala.collection.Set[String]): Self[String] = {
    if (reserved)
      literalOrKeyword(value)
    else
      literal(value)
  }
}
