package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser._

case class Keyword(var value: String, reserved: Boolean = true, verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParser(keywords: scala.collection.Set[String]): EditorParser[String] = literal(value)
}
