package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser
import core.bigrammar.BiGrammarToParser.whitespaceG

case class Delimiter(value: String) extends StringGrammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParser = whitespaceG ~> BiGrammarToParser.literal(value)
}
