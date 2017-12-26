package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser
import core.bigrammar.BiGrammarToParser.whitespaceChar

case class Delimiter(value: String) extends StringGrammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParser(keywords: Set[String]): BiGrammarToParser.Parser[Any] = whitespaceChar ~> BiGrammarToParser.literal(value)
}
