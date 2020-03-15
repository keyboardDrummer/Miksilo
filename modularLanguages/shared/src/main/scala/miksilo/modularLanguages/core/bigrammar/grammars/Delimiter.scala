package miksilo.modularLanguages.core.bigrammar.grammars

import miksilo.modularLanguages.core.bigrammar.BiGrammarToParser
import miksilo.editorParser.parsers.editorParsers.History

case class Delimiter(value: String, penalty: Double = History.missingInputPenalty, allowDrop: Boolean = true) extends StringGrammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Parser[String] =
    BiGrammarToParser.literal(value, penalty, allowDrop)
}
