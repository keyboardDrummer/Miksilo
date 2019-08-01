package core.bigrammar.grammars

import core.bigrammar.BiGrammarToParser
import core.parsers.editorParsers.History

case class BiLiteral(value: String,
                     penalty: Double = History.missingInputPenalty,
                     allowDrop: Boolean = true,
                     verifyWhenPrinting: Boolean = false)
  extends StringGrammar(verifyWhenPrinting) {

  if (value.length == 0)
    throw new RuntimeException("value must have non-zero length")

  override def getParserBuilder(keywords: scala.collection.Set[String]): BiGrammarToParser.Self[String] =
    BiGrammarToParser.literal(value, penalty, allowDrop)
}
