package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser
import core.parsers.editorParsers.History

case class Delimiter(value: String, penalty: Double = History.missingInputPenalty) extends StringGrammar {
  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[String] =
    BiGrammarToParser.Literal(value, penalty)
}
