package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser

case class Keyword(value: String, reserved: Boolean = true, verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[String] = BiGrammarToParser.literal(value)
}
